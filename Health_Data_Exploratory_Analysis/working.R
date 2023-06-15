
# Packages

install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("reshape2")

library("tidyverse")
library("here")
library("skimr")
library("janitor")
library("lubridate")
library("reshape2")



# My Datasets

setwd("~/Data Science/Google Data Analytics/Capstone")

dailyActivity_DF <-
  read.csv("dataset/dailyActivity_merged.csv")
sleepDay_DF <-
  read.csv("dataset/sleepDay_merged.csv")
weightLogInfo_DF <-
  read.csv("dataset/weightLogInfo_merged.csv")



# PROCESS

# Data Preview

head(dailyActivity_DF)
head(sleepDay_DF)
head(weightLogInfo_DF)

View(dailyActivity_DF)
View(sleepDay_DF)
View(weightLogInfo_DF)

str(dailyActivity_DF)
str(sleepDay_DF)
str(weightLogInfo_DF)

skim_without_charts(dailyActivity_DF)
skim_without_charts(sleepDay_DF)
skim_without_charts(weightLogInfo_DF)

colnames(dailyActivity_DF)
colnames(sleepDay_DF)
colnames(weightLogInfo_DF)

# Check for Duplicates
sum(duplicated(dailyActivity_DF))
sum(duplicated(sleepDay_DF))
sum(duplicated(weightLogInfo_DF))

# Check Date col datatype
typeof(dailyActivity_DF$ActivityDate)
typeof(sleepDay_DF$SleepDay)
typeof(weightLogInfo_DF$Date)

# Check number of participants

# For dailyActivity_DF
n_distinct(dailyActivity_DF$Id)

sapply(dailyActivity_DF, function(x) n_distinct(x))

dailyActivity_DF %>%
  group_by(Id) %>%
  summarize(DistinctDates = n_distinct(ActivityDate))

# For sleepDay_DF
n_distinct(sleepDay_DF$Id)
sapply(sleepDay_DF, function(x) n_distinct(x))

# For weightLogInfo_DF
n_distinct(weightLogInfo_DF$Id)
sapply(weightLogInfo_DF, function(x) n_distinct(x))


# Transform

#sleepDay_DF 

# remove duplicates 
sleepData <- sleepDay_DF[!duplicated(sleepDay_DF),]

# split the SleepDay column to SleepDate and SleepTime columns
sleepData <-
  sleepData %>%
  separate(SleepDay, into = c("Date", "SleepTime", "DayStatus"), sep = " ") %>%
  subset(select = -c(SleepTime,DayStatus))

# Change datatype
class(sleepData$Id) = "character"
sleepData$Date <- mdy(sleepData$Date)


#The Weight Log DF

weightData <-
  weightLogInfo_DF %>%
  subset(select = -Fat) %>%
  separate(Date, into = c("Date", "Time", "DayStatus"), sep = " ") %>%
  subset(select = -c(Time,DayStatus))%>%
  mutate(WeightStatus = case_when(BMI > 29.9 ~ 'Obese', 
                                  BMI > 24.9 ~ 'Overweight',
                                  BMI < 18.5 ~ 'Underweight',
                                  TRUE ~ 'Normal Weight'))

# Change datatype
class(weightData$Id) = "character"
weightData$Date <- mdy(weightData$Date)


# The dailyActivity_DF

activityData <- dailyActivity_DF %>% 
  subset(!dailyActivity_DF$TotalSteps == 0) %>%
  subset(!dailyActivity_DF$TotalDistance == 0) %>%
  subset(select = -c(LoggedActivitiesDistance))


# Change datatype
class(activityData$Id) = "character"
activityData$ActivityDate <- mdy(activityData$ActivityDate)

activityData <-
  activityData %>%
  rename("Date" = ActivityDate)

#Creating new columns

activityData <- 
  activityData %>%
  mutate(Day = weekdays(Date)) %>%
  mutate(TotalActiveMinutes = (activityData$VeryActiveMinutes + 
                                 activityData$FairlyActiveMinutes + 
                                 activityData$LightlyActiveMinutes)) %>%
  drop_na()




# activityData <- activityData %>%
#   subset(!activityData$TotalActiveMinutes == 0)
# 
# a <- activityData %>%
#   subset(activityData$TotalActiveMinutes == 0)

write.csv(activityData, file ='fitbit_daily_activity.csv')





# The merged dataframe

fitBitData_merged <-
  merge(sleepData,activityData, by=c("Id", "Date"), all = TRUE)

head(fitBitData_merged)
View(fitBitData_merged)
str(fitBitData_merged)
skim_without_charts(fitBitData_merged)
colnames(fitBitData_merged)
sum(duplicated(fitBitData_merged))
n_distinct(fitBitData_merged$Id)
sapply(fitBitData_merged, function(x) n_distinct(x))

fitBitData_merged <-
  fitBitData_merged %>%
  replace(is.na(fitBitData_merged), 0)


#a <-
#  fitBitData_merged %>%
#  filter(Id == "1503960366")


# Trial
fitBitData_merged2 <-
  list(sleepData, weightData, activityData) %>% reduce(full_join, by= c("Id", "Date"))

fitBitData_merged2 <-
  fitBitData_merged2 %>%
  replace(is.na(fitBitData_merged2), 0) %>%
  subset(select = -c(IsManualReport,LogId))






# ANALYSE



weightAggregated <-
  weightData %>%
  group_by(Id) %>%
  summarise(BodyWeightKg = mean(WeightKg),
            BMIv = mean(BMI)) %>%
  mutate(WeightStatus = case_when(BMIv > 29.9 ~ 'Obese', 
                                  BMIv> 24.9 ~ 'Overweight',
                                  BMIv < 18.5 ~ 'Underweight',
                                  TRUE ~ 'Normal Weight'))

weightAggregated %>% 
  count(weightAggregated$WeightStatus)

sleepAggregated <-
  sleepData %>%
  group_by(Id) %>%
  summarise(SleepRecord = sum(TotalSleepRecords),
            TotalHoursAsleep = (sum(TotalMinutesAsleep)/60),
            AveHoursAsleep = (mean(TotalMinutesAsleep)/60),
            AveTimeInBed = (mean(TotalTimeInBed)/60)) %>%
  mutate(NatureofSleep = case_when(AveHoursAsleep < 7 ~ 'Poor Sleep',
                                   TRUE ~ 'Good Sleep'))

sleepData %>% filter(Id == 1644430081)

sleepAggregated%>%
  count(sleepAggregated$NatureofSleep == 'Good Sleep')


activityAggregated <-
  activityData %>%
  group_by(Id, Day) %>%
  summarise(StepsTaken = sum(TotalSteps),
            AverageSteps = mean(TotalSteps),
            DistanceTravelled = sum(TotalDistance),
            AverageDistance = mean(TotalDistance),
            VeryActiveMins = mean(VeryActiveMinutes),
            FairlyActiveMins = mean(FairlyActiveMinutes),
            LightlyActiveMins = mean(LightlyActiveMinutes),
            TimeSedentaryHours = (sum(SedentaryMinutes)/60),
            AveSedentaryHours = (mean(SedentaryMinutes)/60),
            CaloriesSpent = sum(Calories),
            AveCaloriesSpent = mean(Calories))


  







activityData %>%  
  select(TotalSteps,
         TotalDistance,
         VeryActiveDistance,
         ModeratelyActiveDistance,
         LightActiveDistance,
         SedentaryActiveDistance,
         VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes,
         SedentaryMinutes,
         Calories) %>%
  summary()

activityAggregated %>%
  count(activityAggregated$AverageSteps >= 6000)

activityAggregated %>%
 count(activityAggregated$VeryActiveMins >= 30)




# activityAggregated %>%  
#   select(StepsTaken,
#          AverageSteps,
#          DistanceTravelled,
#          AverageDistance,
#          TimeSedentaryHours,
#          AveSedentaryHours,
#          CaloriesSpent,
#          AveCaloriesSpent) %>%
#   summary()


sleepData %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()


sleepData %>% 
  count(sleepData$TotalMinutesAsleep >= 419)
sleepAggregated%>%
  count(sleepAggregated$AveHoursAsleep >= 7)
sleepAggregated%>%
  count(sleepAggregated$NatureofSleep)



weightData %>%  
  select(WeightKg,
         WeightPounds,
         BMI) %>%
  summary()





# # Creating a day of the week column
# 
# a <- today()
# weekdays(a)
# 
# activityData <- 
#   activityData %>%
#   mutate(Day = weekdays(Date))
# 
# View(activityData)



# ggplot(data = activityAggregated) +
#   aes(x = (Day), y = AverageSteps) +
#   geom_col(fill =  'blue') +
#   labs(x = 'Day of week', y = 'Total Steps', title = 'Total steps per Day')
# ggsave('total_steps_day1.png')
# 
# skim_without_charts(activityData)




# Showing Days the Participants are Most Active

ggplot(data = activityData) +
  aes(x = (Day), y = TotalSteps) +
  geom_col(fill =  'blue') +
  labs(x = 'Day of week', y = 'Total Steps', title = 'Total steps per Day')
ggsave('total_steps_day.png')


ggplot(data = activityData) +
  aes(x = (Day), y = TotalActiveMinutes) +
  geom_col(fill =  'blue') +
  labs(x = 'Day of week',
       y = 'Total Active Minutes',
       title = 'Total Active Minutes per Day')
ggsave('total_active_minutes_day.png')


ggplot(data = activityData) +
  aes(x = (Day), y = Calories) +
  geom_col(fill =  'blue') +
  labs(x = 'Day of week',
       y = 'Calories Expended',
       title = 'Daily Calories Expended')
ggsave('Daily_Calories.png')


# ggplot(data = activityData) +
#   aes(x = (Day), y = Calories) +
#   geom_col(fill =  'blue') +
#   facet_wrap(~Id)
#   labs(x = 'Day of week',
#        y = 'Calories Expended',
#        title = 'Daily Calories Expended')
# ggsave('Daily_Calories_Id.png')


# Showing relationships for Calories expenditure

ggplot(data = activityData) +
  aes(x= TotalActiveMinutes, y = Calories) +
  geom_point(color = 'orange') +
  geom_smooth() +
  labs(x = 'Time Active (minutes)',
       y = 'Calories Expended',
       title = 'Calories Expended vs Time Active')
ggsave('Calories_Expended_vs_Time_Active.png')


ggplot(data = activityData) +
  aes(x= TotalSteps, y = Calories) +
  geom_point(color = 'orange') +
  geom_smooth() +
  labs(x = 'Total Steps',
       y = 'Calories Expended',
       title = 'Calories Expended vs Total Steps')
ggsave('calories_expended_vs_total_steps.png')


ggplot(data = activityData) +
  aes(x= SedentaryMinutes, y = Calories) +
  geom_point(color = 'orange') +
  geom_smooth() +
  labs(x = 'Time Sedentary (minutes)',
       y = 'Calories Expended',
       title = 'Calories Expended vs Time Sedentary (minutes)')
ggsave('Calories_Expended_vs_Sedentary_(minutes).png')


#analysing the participants' activities per time


VeryActiveMins <- sum(activityData$VeryActiveMinutes)
FairlyActiveMins <- sum(activityData$FairlyActiveMinutes)
LightlyActiveMins <- sum(activityData$LightlyActiveMinutes)
SedentaryMins <- sum(activityData$SedentaryMinutes)
total <- (VeryActiveMins + FairlyActiveMins + LightlyActiveMins + SedentaryMins)

VeryActive <- round((VeryActiveMins/total) * 100,0)
FairlyActive <- round((FairlyActiveMins / total)* 100,0)
LightlyActive <- round((LightlyActiveMins / total)* 100,0)
Sedentary <- round((SedentaryMins / total)* 100,0)

print(SedentaryMins/60)
print(Sedentary)
print(VeryActive + FairlyActive+LightlyActive)

x <- c(VeryActive, FairlyActive, LightlyActive, Sedentary)
y <- c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes")

pie(x,y)


percentage <- 
  data.frame(Activity=c("Very Active", "Fairly", "Lightly", "Sedentary" ),
             Proportion_of_Time=c(VeryActive,FairlyActive,LightlyActive,Sedentary),
             Time = c(VeryActiveMins, FairlyActiveMins, LightlyActiveMins, SedentaryMins)
)

ggplot(data = percentage) +
  geom_col() + 
  aes(x= Proportion_of_Time, y= Activity)+
  labs(x = 'Time (Percent)',
       y = 'Activity Type',
       title = 'Time Spent per Activity Type')
ggsave('Time_Spent_per_Activity_Type.png')


  

ggplot(data = percentage) +
  geom_col() + 
  aes(x= (Time)/60, y= Activity) +
  labs(x = 'Time (Hours)',
       y = 'Activity Type',
       title = 'Time Spent per Activity Type')
ggsave('Time_Spent_per_Activity_Type2.png')

  



# plot_ly(percentage, labels = ~level, values = ~minutes, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
#   layout(title = 'Activity Level Minutes',
#          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
# 




# analysing the sleep patterns of participants, and the sleep-time distribution

ggplot(data = sleepData) +
  geom_boxplot() +
  aes(y= TotalMinutesAsleep/60) +
  labs(y = 'Time Asleep (Hours)',
       title = 'Sleep Time Plot')
ggsave('Sleep_Time_Plot.png')


ggplot(data = sleepAggregated) +
  geom_bar() +
  aes(y= NatureofSleep) +
  labs(y = 'Nature of Sleep',
       title = 'Nature of Sleep Plot')
ggsave('Nature_of_Sleep_Plot.png')




# analysing the weight distribution of participants

ggplot(data = weightData) +
  geom_boxplot() +
  aes(y= WeightKg) +
  labs(y = 'Weight (Kg)',
       title = 'Weight Plot')
ggsave('Weight_Plot.png')

ggplot(data = weightData) +
  geom_histogram() +
  aes(x= WeightKg) +
  labs(x = 'Weight (Kg)',
       title = 'Weight Distribution Plot')
ggsave('Weight_Distribution_Plot.png')

ggplot(data = weightData) +
  geom_bar() +
  aes(x= WeightStatus) +
  labs(x = 'Weight (Kg)',
       title = 'Weight Distribution Plot')
ggsave('Weight_Distribution_Plot1.png')

ggplot(data = weightAggregated) +
  geom_col() +
  aes(y= BodyWeightKg, x=Id) +
  labs(x = 'Weight (Kg)',
       title = 'Weight Distribution Plot')
ggsave('Weight_Distribution_Plot2.png')


ggplot(data = weightAggregated) +
  geom_col() +
  aes(y= BMIv, x=Id) +
  labs(x = 'Weight (Kg)',
       title = 'Weight Distribution Plot')
ggsave('Weight_Distribution_Plot3.png')







...




# # CREATING A COMBINED DATAFRAME
# 
# fitBitData_merged2 <-
#   list(sleepData, weightData, activityData) %>% reduce(full_join, by= c("Id", "Date"))
# 
# skim_without_charts(fitBitData_merged)
# 
# 
# 
# heartrate_seconds_DF %>%
#   separate(Time, into = c("SleepDate", "SleepTime", "DayStatus"), sep = " ") %>%
#   unite("SleepTime", c("SleepTime", "DayStatus"), sep = "_")
# 
# 
# 
# str(heartrate_seconds_DF$Time)
# typeof(heartrate_seconds_DF$Time)
# mdy_hms(heartrate_seconds_DF$Time)
# 
# my_dataframe <- replace(my_dataframe, is.na(my_dataframe), 0)
# 
# sleepDay_DF %>%
#   separate(SleepDay, into = c("SleepDate", "SleepTime", "DayStatus"), sep = " ") %>%
#   unite("SleepTime", c("SleepTime", "DayStatus"), sep = "_") %>%
#   sleepDay_DF$SleepDate <- as.Date(sleepDay_DF$SleeDate, "%m/%d/%Y")%>%
#   group_by(Id) %>%
#   mutate(SleepDays = count(SleepDate, vars = Id))%>%
#   summarise(SumSleepRecords = sum(TotalSleepRecords),
#             SumTotalMinutesAsleep = sum(TotalMinutesAsleep),
#             AveTotalMinutesAsleep = ave(TotalMinutesAsleep),
#             SumTotalTimeInBed = sum(TotalTimeInBed),
#             AveTotalTimeInBed = ave(TotalTimeInBed)) 
# 
# filter(sleepDay_DF, Id == 1503960366)