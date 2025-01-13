# ![Deskripsi Gambar](https://github.com/hasbimaulanaa/hasbi_portofolio/blob/main/image.png?raw=true) Bellabeat Smart Device Data Analysis
**Junior Data Analyst Portfolio Project**

by Hasbi Maulana

---

## Background Information

**Bellabeat** is a high-tech company that manufactures health-focused smart products. Collecting data on activity, sleep, stress, and reproductive health has allowed Bellabeat to empower women with knowledge about their own health and habits. 

### Business Task

Sršen, the company's cofounder, would like an analysis of Bellabeat’s available consumer data to identify opportunities for growth. She has asked the marketing analytics team to analyze smart device usage data in order to gain insight into how people are already using their smart devices. Then, using this information, she would like recommendations for how these trends can inform Bellabeat marketing strategy. Therefore, in this case study, I will answer the following questions:

1. What are some trends in smart device usage?
2. How can these trends help influence Bellabeat marketing strategy?
3. How could these trends help influence Bellabeat marketing strategy?

### About the Data

The data for this case study comes from [Fitbit Fitness Tracker Data](https://www.kaggle.com/datasets/arashnic/fitbit), a public domain dataset available on Kaggle. It contains personal fitness tracker information from 35 FitBit users. These 35 Fitbit users consented to the submission of all personal tracker data contained in this dataset. 

I will import the data set from Kaggle into RStudio where I can clean, filter, and analyze the data. 

### Limitations

- Sample size: 35 people is not a large enough sample to be representative of all FitBit users
- Outdated: The dataset contains data from a one month period in 2016 only. For a deeper and more accurate analysis of trends, we would need data from the current year, preferably collected for an entire year to look at if trends vary during different times of year. 
- Limited: The dataset does not contain any demographic information about the users, including gender, age, or location, which would be beneficial for marketing purposes to target specific customers


## Data Preparation
1. load packages
```{r}
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
```

2. Load CSV files containing our data
```{r}
# set working directory
# setwd("~/Fitbit Case Study")
# load files
daily_activity <- read.csv("dailyActivity_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
daily_sleep <- read.csv("sleepDay_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")
```

3. Identify number of participants in each data set by counting distinct IDs
```{r}
n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)
n_distinct(METs$Id)
```
There is 35 participants in the activity. 34 participants in the calories, intensities and METs data sets, 23 in the sleep and only 11 in the weight data set. 11 participants is not significant to make any recommendations and conclusions based on this data
Note: Because very few participants contributed weight data, we will exclude it from our analysis

4. Fixing Formatting
```{r}
# intensities
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# activity
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y"))
# METs
METs$ActivityMinute=as.POSIXct(METs$ActivityMinute, format="%m/%d/%Y", tz=Sys.timezone())
METs$ActivityMinute <- format(METs$ActivityMinute, format = "%m/%d/%y")
```
5. Let’s have a look at summary statistics of the data sets
```{r}
# activity
activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()
# explore num of active minutes per category
activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()
# calories
calories %>%
  select(Calories) %>%
  summary()
# sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# weight
weight %>%
  select(WeightKg, BMI) %>%
  summary()
# METs
METs %>%
  select(METs) %>%
  summary()
```
6. Summary
```{r}
TotalSteps     TotalDistance   SedentaryMinutes   Calories   
 Min.   :    0   Min.   : 0.000   Min.   :  32.0   Min.   :   0  
 1st Qu.: 1988   1st Qu.: 1.410   1st Qu.: 728.0   1st Qu.:1776  
 Median : 5986   Median : 4.090   Median :1057.0   Median :2062  
 Mean   : 6547   Mean   : 4.664   Mean   : 995.3   Mean   :2189  
 3rd Qu.:10198   3rd Qu.: 7.160   3rd Qu.:1285.0   3rd Qu.:2667  
 Max.   :28497   Max.   :27.530   Max.   :1440.0   Max.   :4562

VeryActiveMinutes FairlyActiveMinutes LightlyActiveMinutes
 Min.   :  0.00    Min.   :  0.00      Min.   :  0.0       
 1st Qu.:  0.00    1st Qu.:  0.00      1st Qu.: 64.0       
 Median :  0.00    Median :  1.00      Median :181.0       
 Mean   : 16.62    Mean   : 13.07      Mean   :170.1       
 3rd Qu.: 25.00    3rd Qu.: 16.00      3rd Qu.:257.0       
 Max.   :202.00    Max.   :660.00      Max.   :720.0

Calories     
 Min.   : 42.00  
 1st Qu.: 61.00  
 Median : 77.00  
 Mean   : 94.27  
 3rd Qu.:104.00  
 Max.   :933.00

WeightKg           BMI       
 Min.   : 53.30   Min.   :21.45  
 1st Qu.: 61.70   1st Qu.:24.10  
 Median : 62.50   Median :24.39  
 Mean   : 73.44   Mean   :25.73  
 3rd Qu.: 85.80   3rd Qu.:25.76  
 Max.   :129.60   Max.   :46.17 

METs       
 Min.   :  0.00  
 1st Qu.: 10.00  
 Median : 10.00  
 Mean   : 14.24  
 3rd Qu.: 11.00  
 Max.   :189.00 
```
Some interesting discoveries from this summary:
1. High Sedentary Time: An average of 995 minutes or approximately 16.5 hours per day is spent in a sedentary state, which needs to be reduced to improve overall health.
2. Suboptimal Daily Steps: The average daily steps are 6,547, still below the CDC recommendation of 8,000 steps per day for optimal health benefits.
3. Calories Burned: The average calories burned are 2,189 with a significant variation up to 4,562 calories, indicating differences in activity levels among users.
4. Low Sleep Duration: The average sleep sessions recorded are 1.08 sessions with a median of 1 session per day, suggesting possible data inaccuracies or incomplete sleep tracking.
5. Weight and BMI: The average weight is 73.44 kg with a BMI of 25.73, classified as overweight, highlighting the need for education on weight management.
6. The relatively high average METs in this data indicates the presence of active users. However, since the minimum value is 0, it also suggests the existence of a group that is completely inactive.

## Visualization
![](https://github.com/hasbimaulanaa/hasbi_portofolio/blob/main/Picture5.jpg?raw=true)

There is a positive relationship between Very Active Minutes and Calories Burned, but at lower durations, there is considerable variability. 
This insight can be used by Bellabeat to: 
1. Encourage users to increase physical activity with a minimum target for very active minutes to achieve optimal calorie burn results.
2. Provide more personalized workout recommendations based on the duration of activity that effectively burns calories.
Bellabeat can use this insight to encourage users to increase physical activity by targeting a minimum number of steps for optimal calorie burning results.

![image](https://github.com/user-attachments/assets/5347ab0a-b35d-4a22-b3f0-bf951c60e01e)

Here we can clearly see the negative relationship between Sedentary Minutes and Sleep time.
For recomendation Bellabeatapp can provide personalized recommendations based on users' sleep data to ensure they achieve optimal sleep quality (for example, reducing screen time or activities that increase stress before bedtime).

![image](https://github.com/hasbimaulanaa/hasbi_portofolio/blob/main/Picture7.jpg?raw=true)

There is a strong positive relationship between the number of steps (Total Steps) and calories burned (Calories Burned).
The linear regression line displayed shows the trend that the more steps taken, the higher the calories burned, with the influence of body weight also being relevant.

![image](https://github.com/hasbimaulanaa/hasbi_portofolio/blob/main/picture8.png?raw=true)

Insights that can be taken from the graph above are:
1. Daily Activity Pattern: Total activity begins to increase significantly around 06:00 and peaks around noon (12:00). This shows that the majority of users tend to be more active during the day.
2. Decreasing Nightly Activity: After 8:00 p.m., activity intensity begins to decrease, indicating that most users begin to reduce activity and rest at night.
3. Minimal Activity Early Morning: The lowest activity intensity occurs between 00:00 to 04:00, which can be attributed to the sleep time of the majority of users.
4. Multiple Activity Sessions: There are two significant activity peaks, namely around noon (12:00) and late afternoon (18:00-20:00), indicating that many users engage in physical activity during the day and evening.


## Recommendations
How can these trends help influence Bellabeat marketing strategy?

We can make marketing recommendations based on what we have learned about how customers are currently using smart fitness devices:

1. Promote Active Lifestyles:
   - Highlight the importance of reducing sedentary time, as data shows an average of 995 minutes of sedentary time per day.
   - Launch campaigns encouraging users to reach the CDC's recommended daily steps (8,000 steps) to promote a healthier lifestyle.

2. Personalized Workout Suggestions:
   - Develop app features providing personalized workout plans tailored to the user's activity level.
   - Encourage users to set and track goals for 'Very Active Minutes' and reward progress.

3. Sleep Quality Improvement:
   - Provide personalized sleep recommendations based on users' sleep patterns.
   - Educate users on reducing screen time and stress before bed to improve sleep quality.

4. Time-Based Engagement Strategies:
   - Launch campaigns targeting morning and evening activity peaks with motivational content.
   - Use notifications to encourage activity during low periods (early morning and late night).

5. Data-Driven Campaigns:
   - Highlight success stories of users who improved their health using Bellabeat devices.
   - Share visual data insights (like activity graphs) in promotional materials to inspire user engagement.

6. Product Development Suggestions:
   - Develop features that nudge users when sedentary for too long.
   - Enhance data accuracy for sleep and weight metrics by improving tracking algorithms.

By leveraging these insights, Bellabeat can design more effective marketing strategies that emphasize health improvement and personalized guidance, ultimately enhancing user engagement and retention.By leveraging these insights, Bellabeat can design more effective marketing strategies that emphasize health improvement and personalized guidance, ultimately enhancing user engagement and retention.
