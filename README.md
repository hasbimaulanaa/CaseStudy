# 📊 Bellabeat Smart Device Data Analysis
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

```

## Data Exploration

1. Graph variables of interest, check for outliers in the data
```{r}
summary(daily_activity$TotalSteps)
ggplot(daily_activity, aes(x = TotalSteps)) +
  geom_boxplot()
# Most of the daily total steps appear to be around 4000-11000.
#   There appear to be possible outliers on the high end

steps_upper <- quantile(daily_activity$TotalSteps, .9985, na.rm = TRUE)
# This shows that 99.85% of the observations are at 28,680 or below. 
# Values above this number are more than 3 standard deviations from the mean, 
#   indicating they are outliers. 

daily_activity <- subset(daily_activity, TotalSteps <= 28680)
# 2 outliers were removed

```

2. Extract more information by running descriptive statistics

#### Sleep Data

* What is the average amount of sleep for each participant?
```{r}
mean_sleep <- daily_sleep %>%
  group_by(Id) %>%
  summarize(mean_sleep = mean(TotalMinutesAsleep)) %>%
  select(Id, mean_sleep) %>%
  arrange(mean_sleep) %>%
  as.data.frame()
head(mean_sleep)
  
```


* What percent of the time did participants actually spend sleeping while laying in bed?
```{r}
daily_sleep %>%
  group_by(Id) %>%
  mutate(percent_sleep = (TotalMinutesAsleep/TotalTimeInBed)*100) %>%
  select(Id, percent_sleep) %>%
  summarize(avg_persleep = mean(percent_sleep)) %>%
  arrange(avg_persleep) %>%
  mutate_if(is.numeric, round, 2)

# Most participants slept for at least 90% of the time they spent in bed, with
#   only 4 participants spending a smaller percent of time sleeping, the lowest
#   being 63.37%
```

#### Activity Levels

* Summary stats of different activity levels:
```{r}
suppressPackageStartupMessages(library(psych))

activity_level <- daily_activity[9:12]
describe(activity_level)

```

* Activity levels by participant
```{r}
activity_id <- daily_activity %>%
  group_by(Id) %>%
  summarize(sum_very = sum(VeryActiveMinutes),
            sum_fairly = sum(FairlyActiveMinutes),
            sum_lightly = sum(LightlyActiveMinutes),
            sum_sed = sum(SedentaryMinutes)) %>%
  select(Id, sum_very, sum_fairly, sum_lightly, sum_sed) %>%
  as.data.frame()
head(activity_id)
```

#### Steps  

* On average, during which hour of the day were the most steps taken?
```{r}
hourly_steps %>%
  group_by(Hour) %>%
  summarize(mean_steps = mean(StepTotal)) %>%
  select(Hour, mean_steps) %>%
  arrange(desc(mean_steps)) %>%
  head(1)
# Answer: 6:00PM with an average of about 600 steps
# Creating a dataframe with average hourly steps for later visualization
mean_steps <- hourly_steps %>%
  group_by(Hour) %>%
  summarize(mean_steps = mean(StepTotal)) %>%
  select(Hour, mean_steps) %>%
  arrange(desc(Hour)) %>%
  as.data.frame()
```

* What is the mean and standard deviation for total steps taken by participant?
```{r}
steps_byId <- hourly_steps %>%
  group_by(Id) %>%
  summarize(mean_steps_id = mean(StepTotal), sd_steps_id = sd(StepTotal)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()
head(steps_byId)
```


## Data Visualization

1. Relationship between steps taken in a day and sedentary minutes:
```{r message=FALSE}
ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title="Total Steps vs. Sedentary Minutes",
       x = "Steps", y = "Minutes")
```

There appears to be no correlation between total daily steps taken and sedentary minutes. 
We can confirm with a simple linear regression:
```{r}
sed_steps_lr <-lm(SedentaryMinutes ~ TotalSteps, daily_activity)
summary(sed_steps_lr)
```

Results confirm little correlation, with an r^2 value of .11

2. Average amount of time participants slept each night during the course of the study:
```{r}
# Graph the results
options(scipen = 999)
ggplot(mean_sleep, aes(x = Id, y = mean_sleep)) +
  geom_col(aes(reorder(Id, +mean_sleep), y = mean_sleep)) +
  labs(title = "Average Minutes of Sleep", x = "Participant Id", y = "Minutes") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(yintercept = mean(mean_sleep$mean_sleep), color = "red")
```

The graph shows the average sleep of each participant individually, as well as how their sleep compares to the overall average across all participants. 

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
