install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reader")
install.packages("plotly")
install.packages("skimr")
install.packages("cowplot")
install.packages("tidyr")
install.packages("janitor")

library(plotly)
library(tidyverse) #wrangle data
library(dplyr) #clean data
library(lubridate)  #wrangle date attributes
library(skimr) #get summary data
library(ggplot2) #visualize data
library(cowplot) #grid the plot
library(readr) #save csv
library(tidyr)
library(janitor)

path <- file.path("C:","Users","Lenovo","Documents","Bellabeat","Fitabase Data 4.12.16-5.12.16")

setwd(path)

daily_activity <- read.csv("dailyActivity_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")

head(daily_activity)
head(sleep_day)
head(weight)
head(hourly_steps)

#check for Na
sum(is.na(daily_activity))
sum(is.na(sleep_day))
sum(is.na(weight))
sum(is.na(hourly_steps))

#CHECK FOR DUPLICATES

sum(duplicated(daily_activity))
sum(duplicated(sleep_day))
sum(duplicated(weight))
sum(duplicated(hourly_steps))

#romove duplicates

sleep_day <- sleep_day[!duplicated(sleep_day), ]

sum(duplicated(sleep_day))

#add weekday 

daily_activity <- daily_activity %>%  mutate( weekday = weekdays(as.Date(ActivityDate,"%m/%d/%Y")))
                                              
#add hour column 
hourly_steps$ActivityHour <- as.POSIXct(hourly_steps$ActivityHour,format="%m/%d/%Y %I:%M:%S %p")
hourly_steps$hour <- format(hourly_steps$ActivityHour,format=('%H'))
head(hourly_steps)

#marged data

merged1 <- merge(daily_activity,sleep_day,by = c("Id"),all= TRUE)

data_marged <- merge(merged1,weight,by = c("Id"),all = TRUE)

data_marged$weekday <- factor(data_marged$weekday, levels= c("Sunday","Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

                  

write.csv(data_marged,"data_marged.csv")

sum(is.na(data_marged$Id))
sum(duplicated(data_marged$Id))
n_distinct(data_marged$Id)

#daily activity extra 3 users ,sleep day less 6 users, weight less 22 users 
n_distinct(sleep_day$Id)
n_distinct(weight$Id)
n_distinct(daily_activity$Id)

weight %>%
  filter(IsManualReport=="True") %>% 
  group_by(Id) %>% 
  summarise("Manual Weight Report"==n()) %>% 
  distinct()

# We will classify the average user's sleep into three patterns

sleep_day_new <- sleep_day %>% group_by(Id) %>% 
  summarise(avg_time_sleep= mean(TotalMinutesAsleep)) %>%
  mutate(Categores=case_when(
    avg_time_sleep < 300 ~ "unhealthy sleep",
    avg_time_sleep >= 320 & avg_time_sleep <=  420 ~ "normal Sleep",
    avg_time_sleep > 420 ~ "healthy sleep"))
  

#We will separate observations into fitness groups based on walking lifestyle: “Sedentary, Needs Improvment, Active, Highly Active”.

Steps_categores <- daily_activity %>% group_by(Id) %>% 
  summarise(avg_steps=mean(TotalSteps)) %>% 
  mutate(level_steps= case_when(
    avg_steps < 5000 ~ "Sedentary",
    avg_steps >=5000 & avg_steps < 10000 ~ "Needs Improvment",
    avg_steps >=10000 & avg_steps < 12500 ~ "Active",
    avg_steps >=12500 ~ "Highly active"))


data_marged %>% 
  dplyr:: select(TotalDistance,TotalMinutesAsleep,TotalSteps,TotalTimeInBed,Calories,weekday,WeightPounds,BMI) %>%
  summary()

#Let's look at how active users are per hour in total steps. From 5 p.m. to 7 p.m. users take the most steps.

ggplot(data= hourly_steps,aes(x=hour, y=StepTotal,fill=hour))+geom_bar(stat = "identity")+ labs(title = "steps by hour")

#Let's look at categories users are per steps .54.5% from users Needs Improvment

plot_ly(Steps_categores,labels= ~level_steps, value=~avg_steps,type = 'pie', textposition = 'outside',textinfo = 'label+percent') %>% 
  layout(title= 'Users categories by steps')

##weekly steps

ggplot(data = daily_activity, aes(x=weekday,y=TotalSteps))+geom_bar(stat="identity")+labs(title= 'weeklyday steps')

#average sleep hours
sleep_day_new$Categores <- factor(sleep_day_new$Categores, levels= c("unhealthy sleep","normal Sleep","healthy sleep"))

plot_ly(sleep_day_new,labels= ~ Categores, value= ~ avg_time_sleep,type = "pie", textposition = 'outside',textinfo = 'label+percent') %>% 
  layout(title='usere categories by hourly sleep')

#total steps vs calories

ggplot(data=daily_activity,aes(x=TotalSteps,y=Calories,color=LightActiveDistance))+ geom_point()+stat_smooth(method = lm)+
  scale_color_gradient(low = 'green',high = 'red')


#


ggplot(data=daily_activity,aes(x=TotalSteps,y=Calories,color=LightActiveDistance))+ geom_point()+stat_smooth(method = lm)+
  scale_color_gradient(low = 'green',high = 'red')

#
ggplot(data=data_marged,aes(x=TotalSteps,y=Calories,color=WeightKg))+ geom_point()+stat_smooth(method = lm)+
  scale_color_gradient(low = 'green',high = 'red')
#z
ggplot(data=data_marged,aes(x=TotalMinutesAsleep,y=Calories), color=TotalMinutesAsleep)+geom_point()+stat_smooth(method = lm)+
         scale_color_gradient(low = 'green',high = 'red')
