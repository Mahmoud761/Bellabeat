---
title: "Bellabeat"
output:
  html_document:
    df_print: paged
  pdf_document: default
  html_notebook: default
---
# Introduction
This is a case study for Google Data Analyst Certificate:

Bellabeat is a high-tech company that manufactures health-focused smart products. It is a successful small company, but they have the potential to become a larger player in the global smart device market. Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women

# Business Task
Our task is to focus on a Bellabeat product and analyze smart device usage data in order to gain insight into how people are already using the smart devices. Then, using this information, we need to recommend how these trends can help Bellabeat marketing strategy.

# Prepar data
## ROCCC analysis
#### Reliability : LOW – dataset was collected from 30 individuals whose gender is unknown.
#### Originality : LOW – third party data collect using Amazon Mechanical Turk.
#### Comprehensive : MEDIUM – dataset contains multiple fields on daily activity intensity, calories used, daily steps                      taken, daily sleep time and weight record.
#### Current : MEDIUM – data is 5 years old but the habit of how people live does not change over a few years
#### Cited : HIGH – data collector and source is well documented
## Package loading  
```{r}

library(plotly)
library(tidyverse) #wrangle data
library(dplyr) #clean data
library(lubridate)  #wrangle date attributes
library(skimr) #get summary data
library(ggplot2) #visualize data
library(cowplot) #grid the plot
library(readr) #save csv
library(tidyr) #for organizing tabular data
library(janitor) #for data examination and cleaning
```


## Importing data
```{r}
path <- file.path("C:","Users","Lenovo","Documents","Bellabeat","Fitabase Data 4.12.16-5.12.16")

setwd(path)

daily_activity <- read.csv("dailyActivity_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
```

## Data Verification
Examining the first few rows of every data set
```{r}
head(daily_activity)
head(sleep_day)
head(weight)
head(hourly_steps)
```
### Check for Na

```{r}

sum(is.na(daily_activity))
sum(is.na(sleep_day))
sum(is.na(weight))
sum(is.na(hourly_steps))
```
### Check for duplicate 
sleep_day have 3 duplicates
```{r}

sum(duplicated(daily_activity))
sum(duplicated(sleep_day))
sum(duplicated(weight))
sum(duplicated(hourly_steps))
```
 daily activity extra 3 users ,sleep day less 6 users, weight less 22 users
```{r}
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(weight$Id)
n_distinct(hourly_steps$Id)


```
#  Process
 Romove duplicates from sleep_day 
```{r}
sleep_day <- sleep_day[!duplicated(sleep_day), ]

#check
sum(duplicated(sleep_day))

```
### Make useful columns and df
 Convert Activity Date into date format and add a column for day of the week  

 Add column for hours in hourly_steps
```{r}
hourly_steps$ActivityHour <- as.POSIXct(hourly_steps$ActivityHour,format="%m/%d/%Y %I:%M:%S %p")
hourly_steps$hour <- format(hourly_steps$ActivityHour,format=('%H'))
head(hourly_steps)
```

 We will classify the average user's sleep into three patterns

```{r}

sleep_day_new <- sleep_day %>% group_by(Id) %>% 
  summarise(avg_time_sleep= mean(TotalMinutesAsleep)) %>%
  mutate(Categores=case_when(
    avg_time_sleep < 300 ~ "Unhealthy Sleep",
    avg_time_sleep >= 320 & avg_time_sleep <=  420 ~ "Normal Sleep",
    avg_time_sleep > 420 ~ "Healthy Sleep"))
```

 We will separate observations into fitness groups based on walking lifestyle: “Sedentary, Needs Improvment, Active, Highly Active”.
```{r}
Steps_categores <- daily_activity %>% group_by(Id) %>% 
  summarise(avg_steps=mean(TotalSteps)) %>% 
  mutate(level_steps= case_when(
    avg_steps < 5000 ~ "Sedentary",
    avg_steps >=5000 & avg_steps < 10000 ~ "Needsimprovment",
    avg_steps >=10000 & avg_steps < 12500 ~ "Active",
    avg_steps >=12500 ~ "Highly active"))
```

marge data table ()
```{r}
merged1 <- merge(daily_activity,sleep_day,by = c("Id"),all= TRUE)

data_marged <- merge(merged1,weight,by = c("Id"),all = TRUE)
```

 Convert Activity Date into date format and add a column for day of the week  
```{r}
data_marged <- data_marged %>%  
  mutate( weekday = weekdays(as.Date(ActivityDate,"%m/%d/%Y")))

#Arrange the days of the week to use in chart
data_marged$weekday <- factor(data_marged$weekday, levels= c("Sunday","Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

## analyze and chart 

Statistics summary mean, median, min, max for Data_marged

```{r}
data_marged %>% 
  dplyr:: select(TotalDistance,TotalMinutesAsleep,TotalSteps,TotalTimeInBed,Calories,weekday,WeightPounds,BMI) %>%
  summary()
```

Let's look at how active users are per hour in total steps. From 5 p.m. to 7 p.m. users take the most steps
```{r}
ggplot(data= hourly_steps,aes(x=hour, y=StepTotal,fill=hour))+geom_bar(stat = "identity")+ labs(title = "Steps by Hour",x="Hours",Y="Steps")

```
Let's look at categories users are per steps .54.5% from users Needs Improvment

```{r warning=FALSE}
plot_ly(Steps_categores,labels= ~level_steps, value=~avg_steps,type = 'pie', textposition = 'outside',textinfo = 'label+percent') %>% 
  layout(title= 'Users categories by steps')
```


```{r}


ggplot(data = data_marged, aes(x=weekday,y=TotalSteps))+geom_bar(stat="identity",fill='steelblue')+labs(title= 'Weeklyday Steps',x="Weekday",y="Steps")
```

Let's look at categories users are per average sleep hours .

```{r warning=FALSE}
sleep_day_new$Categores <- factor(sleep_day_new$Categores, levels= c("Unhealthy Sleep","Normal Sleep","Healthy Sleep"))

plot_ly(sleep_day_new,labels= ~ Categores, value= ~ avg_time_sleep,type = "pie", textposition = 'outside',textinfo = 'label+percent') %>% 
  layout(title='usere categories by hourly sleep')
```


The more active you are, the more steps you take, and the more calories you'll burn. This is an obvious fact, but we can still look at the data to find anything interesting. Here we see that some users have similar weights, but some of them burn more than 2500 calories and nearly 20,000 steps to reach a weight of 60 kg, others only need to burn more than 1500 calories and only about 10,000 steps, and there are those who weigh up to 120 kg and they can burn over 2000 calories with much fewer steps even less than 5000 steps

```{r warning=FALSE}
ggplot(data=data_marged,aes(x=TotalSteps,y=Calories,color=WeightKg))+ geom_point()+stat_smooth(method = lm)+
  scale_color_gradient(low = 'green',high = 'red')+labs(title = "Steps vs Calories",x="Steps",y="Calories")
```
And about the rumor that a person who sleeps a lot burns fewer calories, we note that there is no significant 
correlation between sleeping more and burning fewer calories.

```{r warning=FALSE}
ggplot(data=data_marged,aes(x=Calories,y=TotalMinutesAsleep,color=TotalMinutesAsleep))+ geom_point()+stat_smooth(method = lm)+
  scale_color_gradient(low = 'green',high = 'red')+labs(title = "Sleep Vs Calories",X="Calories" ,y="Sleep Minutes")
```
