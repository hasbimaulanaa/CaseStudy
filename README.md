# ![Deskripsi Gambar](https://github.com/hasbimaulanaa/hasbi_portofolio/blob/main/image.png?raw=true) Bellabeat Smart Device Data Analysis
**Junior Data Analyst Portfolio Project**

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
 Summary Activity
| Statistic   | Total Steps | Total Distance | Sedentary Minutes | Calories |
|-------------|-------------|----------------|--------------------|----------|
| Min         | 0           | 0.000          | 32.0              | 0        |
| 1st Qu.     | 1988        | 1.410          | 728.0             | 1776     |
| Median      | 5986        | 4.090          | 1057.0            | 2062     |
| Mean        | 6547        | 4.664          | 995.3             | 2189     |
| 3rd Qu.     | 10198       | 7.160          | 1285.0            | 2667     |
| Max         | 28497       | 27.530         | 1440.0            | 4562     |

Intensities
| Statistic   | Total Steps | Total Distance | Sedentary Minutes | Calories |
|-------------|-------------|----------------|--------------------|----------|
| Min         | 0           | 0.000          | 32.0              | 0        |
| 1st Qu.     | 1988        | 1.410          | 728.0             | 1776     |
| Median      | 5986        | 4.090          | 1057.0            | 2062     |
| Mean        | 6547        | 4.664          | 995.3             | 2189     |
| 3rd Qu.     | 10198       | 7.160          | 1285.0            | 2667     |
| Max         | 28497       | 27.530         | 1440.0            | 4562     |

Summary Calaories  
| Statistic   | Calories |
|-------------|----------|
| Min.        | 42.00    |
| 1st Qu.     | 61.00    |
| Median      | 77.00    |
| Mean        | 94.27    |
| 3rd Qu.     | 104.00   |
| Max.        | 933.00   |

Summary sleep 
| Statistic   | Sleep    |
|-------------|----------|
| Min.        | 1.000    |
| 1st Qu.     | 1.000    |
| Median      | 1.000    |
| Mean        | 1.086    |
| 3rd Qu.     | 1.000    |
| Max.        | 3.000    |

Summary weight info
| Statistic   | WeightKg | BMI   |
|-------------|----------|-------|
| Min.        | 53.30    | 21.45 |
| 1st Qu.     | 61.70    | 24.10 |
| Median      | 62.50    | 24.39 |
| Mean        | 73.44    | 25.73 |
| 3rd Qu.     | 85.80    | 25.76 |
| Max.        | 129.60   | 46.17 |

Summary METs       
| Statistic   | METs   |
|-------------|--------|
| Min.        | 0.00   |
| 1st Qu.     | 10.00  |
| Median      | 10.00  |
| Mean        | 14.24  |
| 3rd Qu.     | 11.00  |
| Max.        | 189.00 |

Some interesting discoveries from this summary:
1. High Sedentary Time: An average of 995 minutes or approximately 16.5 hours per day is spent in a sedentary state, which needs to be reduced to improve overall health.
2. Suboptimal Daily Steps: The average daily steps are 6,547, still below the CDC recommendation of 8,000 steps per day for optimal health benefits.
3. Calories Burned: The average calories burned are 2,189 with a significant variation up to 4,562 calories, indicating differences in activity levels among users.
4. Low Sleep Duration: The average sleep sessions recorded are 1.08 sessions with a median of 1 session per day, suggesting possible data inaccuracies or incomplete sleep tracking.
5. Weight and BMI: The average weight is 73.44 kg with a BMI of 25.73, classified as overweight, highlighting the need for education on weight management.
6. The relatively high average METs in this data indicates the presence of active users. However, since the minimum value is 0, it also suggests the existence of a group that is completely inactive.

## Recommendations
![](https://github.com/hasbimaulanaa/hasbi_portofolio/blob/main/Picture5.jpg?raw=true)

There is a positive relationship between Very Active Minutes and Calories Burned, but at lower durations, there is considerable variability. 
This insight can be used by Bellabeat to: 
1. Encourage users to increase physical activity with a minimum target for very active minutes to achieve optimal calorie burn results.
2. Provide more personalized workout recommendations based on the duration of activity that effectively burns calories.


Results confirm little correlation, with an r^2 value of .11



3. Average steps per hour:
```{r}
ggplot(mean_steps, aes(x = Hour, y = mean_steps)) +
  geom_col(aes(reorder(Hour, +mean_steps), mean_steps)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Average Steps Taken per Hour of Day",
       x = "Hour", y = "Average Steps")
```

We can see that the most steps were taken in the evening, from 5-7pm, and the least steps in the middle of the night, between 12-4am.

4. I'm going to combine two datasets I created previously, activity_id and steps_byId, in order to find new relationships between variables. 
```{r}
combined_data <- merge(activity_id, steps_byId, by = "Id")

# Putting just the numerical variables into a separate dataframe, then running a correlation matrix
num_data <- combined_data[-1]
cor(num_data)

# Based on the correlation matrix, there is little correlation between the different activity levels, but there is a moderate (.7) correlation between mean steps taken and very active minutes.

ggplot(combined_data, aes(x = mean_steps_id, y = sum_very)) + 
  geom_point() +
  labs(title = "Average Steps Taken in a Day Compared to Very Active Minutes",
       x = "Average Steps", y = "Very Active Minutes")
```

We can see a moderate upwards trend of "very active minutes" increasing as average steps in a day increases. 

5. I completed additional visualizations in Tableau, which can be viewed here: [Bellabeat Dashboard - Tableau](https://public.tableau.com/views/BellabeatCaseStudy_16572546536690/Dashboard1?:language=en-US&:display_count=n&:origin=viz_share_link)

The descriptive statistical analyses and visualizations completed show the following smart device usage trends:

* Sedentary minutes took up the majority of participants' days and were fairly consistent throughout the week.
* Average "very active minutes" were also consistent throughout the week at around 20 minutes each day.
* On average, participants slept the most on Sundays, which was also the day they took the least amount of steps
* Participants took the most steps on Tuesdays and Saturdays.
* On average, the fewest steps were taken at 3:00 and the most steps taken at 18:00
* On average, participants slept about 390 minues, or 6.5 hours per night
* Users who take more steps per day are more likely to engage in "very active minutes"

## Recommendations
How can these trends help influence Bellabeat marketing strategy?

We can make marketing recommendations based on what we have learned about how customers are currently using smart fitness devices:

1. Very few customers utilized the weight log feature, so this does not appear to be a selling point. Focus on marketing other features such as activity, sleep, and steps tracking, and consider further research into how to make the weight log feature more marketable

2. Our data shows that when active, participants engaged the most in "light" activity and did not have many "very active" minutes each day. The company could add a "level up" feature in which participants can earn points based on time spent being active, with higher levels of activity earning more points. This could motivate users to engage in active minutes more often.

3. There's about a 1000 step decrease on Sundays compared to the other days of the week. A notification on Sunday mornings with a goal to hit a certain number of steps, along with a reward for hitting a 7-day streak could help close this gap and encourage customers to use the device all days of the week.

4. Based on data showing the most usage around 6pm, it seems likely most users have typical work hours during the day and get most of their steps in after work. An ad targeted towards working adults focused on easily tracking steps throughout their busy days could be effective. A reminder notification around 12pm and 8pm can encourage users to increase their activity levels during other break times such as lunch and after dinner as well. 

5. On average, participants got less than the CDC recommended 7 hours of sleep per night. Continue marketing the device's sleep tracking feature as participants who are not getting enough sleep may want a way to track their sleep patterns. Consider marketing along with a meditation app or habit tracker. 
