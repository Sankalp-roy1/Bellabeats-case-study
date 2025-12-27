---
title: "Bellabeat Case Study"
author: "Sankalp Roy"
date: "27-12-2025"
output:
  html_document:
    toc: true
    toc_depth: 2
    theme: cosmo
---
```{r}
knitr::opts_chunk$set(
echo = TRUE,
warning = FALSE,
message = FALSE
)
```

## **Introduction**

Bellabeat is a high-tech wellness company that develops smart products designed to help women monitor activity, sleep, stress, and overall health.
The goal of this analysis is to explore smart device usage data to uncover behavioral trends that can inform Bellabeat’s marketing strategy.

This case study follows the Ask, Prepare, Process, Analyze, Share, and Act framework.

## **Ask: Business Task**

### Objective:

Analyze smart device usage data to identify trends in activity and sleep, then translate those insights into actionable marketing recommendations for Bellabeat.

### Key Questions:

What trends exist in smart device usage?

How could these trends apply to Bellabeat customers?

How can these trends influence marketing strategy?

Prepare: Data Sources

### Primary Dataset:

FitBit Fitness Tracker Data (Kaggle, CC0 Public Domain)

30 Fitbit users

Daily activity, calories, steps, and sleep

31-day observation period

Limitations:

Small sample size

No gender information

## **Process: Data Cleaning & Preparation**

Fitbit users may not reflect Bellabeat users
```{r}
library(tidyverse)
library(lubridate)
library(janitor)
```


## **Importing Data**


```{r}
daily_activity <- read_csv("dailyActivity_merged.csv") %>%
clean_names()

sleep_day <- read_csv("sleepDay_merged.csv") %>%
clean_names()

daily_calories <- read_csv("dailyCalories_merged.csv") %>%
  clean_names()
```


## **Exploring the Data**


```{r}
head(daily_activity)
glimpse(daily_activity)

head(sleep_day)
glimpse(sleep_day)

n_distinct(daily_activity$id)
```

## **Data Cleaning & Processing**

### Converting dates & removeing duplicates

```{r}
daily_activity <- daily_activity %>%
  mutate(activity_date = mdy(activity_date))

sleep_day <- sleep_day %>%
  mutate(sleep_day = mdy_hms(sleep_day) %>% as_date())

sleep_day <- sleep_day %>%
  distinct(id, sleep_day, .keep_all = TRUE)
```


## **Joining Datasets**

```{r}
activity_sleep <- daily_activity %>%
  left_join(
    sleep_day,
    by = c("id" = "id", "activity_date" = "sleep_day")
  )
```


## **Creating New Variables**


### Activity Level Categories
```{r}
activity_sleep <- activity_sleep %>%
  mutate(activity_level = case_when(
    total_steps < 5000 ~ "Sedentary",
    total_steps < 7500 ~ "Lightly Active",
    total_steps < 10000 ~ "Moderately Active",
    TRUE ~ "Very Active"
  ))
```


## **Summary Statistics**


### Average Daily Metrics
```{r}
summary_stats <- activity_sleep %>%
  summarise(
    avg_steps = mean(total_steps, na.rm = TRUE),
    avg_calories = mean(calories, na.rm = TRUE),
    avg_sleep_hours = mean(total_minutes_asleep / 60, na.rm = TRUE)
  )

summary_stats
```


## **Analyze Trends**


### Steps vs Calories
```{r}
steps_calories <- activity_sleep %>%
  select(total_steps, calories) %>%
  drop_na()

cor(steps_calories$total_steps, steps_calories$calories)
```


### Activity Level Distribution
```{r}
activity_distribution <- activity_sleep %>%
  count(activity_level)

activity_distribution
```


### Sleep vs Steps
```{r}
sleep_steps <- activity_sleep %>%
  filter(!is.na(total_minutes_asleep)) %>%
  summarise(
    avg_steps_sleepers = mean(total_steps),
    avg_sleep_hours = mean(total_minutes_asleep / 60)
  )

sleep_steps
```


# **Visualizations**


### Steps vs Calories
```{r}
ggplot(activity_sleep, aes(x = total_steps, y = calories)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Daily Steps vs Calories Burned",
    x = "Total Steps",
    y = "Calories Burned"
  ) +
  theme_minimal()
```


### Activity Level Breakdown
```{r}
ggplot(activity_distribution, aes(x = activity_level, y = n)) +
  geom_col() +
  labs(
    title = "User Activity Level Distribution",
    x = "Activity Level",
    y = "Number of Records"
  ) +
  theme_minimal()
```



### Average Steps by Day of Week
```{r}
activity_sleep %>%
  mutate(day_of_week = wday(activity_date, label = TRUE)) %>%
  group_by(day_of_week) %>%
  summarise(avg_steps = mean(total_steps, na.rm = TRUE)) %>%
  ggplot(aes(x = day_of_week, y = avg_steps, group = 1)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Average Daily Steps by Day of Week",
    subtitle = "Users are more active on weekdays than weekends",
    x = "Day of Week",
    y = "Average Steps"
  ) +
  theme_minimal()
```


**Insight:Weekday activity is higher, suggesting optimal timing for motivational push notifications and ads.**

### Distribution of Sleep Duration

Purpose: Understand sleep consistency and common ranges.

```{r}
activity_sleep %>%
  filter(!is.na(total_minutes_asleep)) %>%
  mutate(sleep_hours = total_minutes_asleep / 60) %>%
  ggplot(aes(x = sleep_hours)) +
  geom_histogram(binwidth = 0.5) +
  geom_vline(xintercept = 7, linetype = "dashed") +
  labs(
    title = "Distribution of Sleep Duration",
    subtitle = "Dashed line indicates recommended 7 hours",
    x = "Hours of Sleep",
    y = "Number of Records"
  ) +
  theme_minimal()
```


**Insight: Many users sleep below recommended levels, opportunity for sleep-focused education.**

### Sleep vs Steps

Purpose: Show how rest relates to activity.

```{r}
activity_sleep %>%
  filter(!is.na(total_minutes_asleep)) %>%
  mutate(sleep_hours = total_minutes_asleep / 60) %>%
  ggplot(aes(x = sleep_hours, y = total_steps)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Relationship Between Sleep Duration and Daily Steps",
    x = "Sleep (Hours)",
    y = "Total Steps"
  ) +
  theme_minimal()
```


**Insight: Users with moderate, consistent sleep tend to show higher activity levels.**

### Activity Level Share

Purpose: Replace simple counts with proportional insight.

```{r}
activity_sleep %>%
  count(activity_level) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(x = "", y = percent, fill = activity_level)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(
    title = "Proportion of User Activity Levels",
    fill = "Activity Level"
  ) +
  theme_void()
```


**Insight: Most users fall into Sedentary or Lightly Active categories—key targets for behavior-change marketing.**

### Calories Burned by Activity Level

Purpose: Demonstrate value of incremental activity.

```{r}
activity_sleep %>%
  group_by(activity_level) %>%
  summarise(avg_calories = mean(calories, na.rm = TRUE)) %>%
  ggplot(aes(x = activity_level, y = avg_calories)) +
  geom_col() +
  labs(
    title = "Average Calories Burned by Activity Level",
    x = "Activity Level",
    y = "Average Calories Burned"
  ) +
  theme_minimal()
```


**Insight: Even moving from sedentary to lightly active shows meaningful calorie increases—perfect for motivational messaging.**


# **Summary of Findings**

* Activity is higher on weekdays than weekends

* Most users are sedentary or lightly active

* Better sleep is associated with higher activity

* Incremental activity changes produce measurable benefits


# **Conclusion**

**This analysis demonstrates how smart device data can uncover actionable wellness insights. By focusing on consistency, sleep, and incremental improvement, Bellabeat can strengthen user engagement and refine its marketing strategy.**


