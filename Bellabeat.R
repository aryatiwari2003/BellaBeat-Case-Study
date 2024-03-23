library(tidyverse) 
library(reshape2)
library(scales)

#Importing the data
dailyActivity_merged <- read.csv("C:/Users/aryat/OneDrive/Documents/GoogleCaseStudy/Case2/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
dailyCalories_merged <- read.csv("C:/Users/aryat/OneDrive/Documents/GoogleCaseStudy/Case2/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
dailyIntensities_merged <- read.csv("C:/Users/aryat/OneDrive/Documents/GoogleCaseStudy/Case2/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
dailySteps_merged <- read.csv("C:/Users/aryat/OneDrive/Documents/GoogleCaseStudy/Case2/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
sleepDay_merged <- read.csv("C:/Users/aryat/OneDrive/Documents/GoogleCaseStudy/Case2/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weightLogInfo_merged <- read.csv("C:/Users/aryat/OneDrive/Documents/GoogleCaseStudy/Case2/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")


merge_1 <- merge(dailyActivity_merged, dailyCalories_merged, by = c("Id","Calories"))
merge_2 <- merge(dailyIntensities_merged, dailyIntensities_merged, by = c("Id","ActivityDay","SedentaryMinutes", "LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes", "SedentaryActiveDistance", "LightActiveDistance", "ModeratelyActiveDistance", "VeryActiveDistance"))

merge_daily <- merge(merge_1, merge_2, by = c("Id","ActivityDay","SedentaryMinutes", "LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes", "SedentaryActiveDistance", "LightActiveDistance", "ModeratelyActiveDistance", "VeryActiveDistance")) %>%
  select(-ActivityDay) %>% rename(Date = ActivityDate)

daily_data <- merge(merge_daily, sleepDay_merged, by = "Id",all=TRUE) %>% drop_na() %>% select(-SleepDay, -TrackerDistance)

#In order to better visualize the data I will group the user into four categories based on for which of their activity types they have more minutes, this will be very useful to quickly see patterns and visualize them:
data_by_usertype <- daily_data %>%
  summarise(
    user_type = factor(case_when(
      SedentaryMinutes > mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Sedentary",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes > mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Lightly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes > mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Fairly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes > mean(VeryActiveMinutes) ~ "Very Active",
    ),levels=c("Sedentary", "Lightly Active", "Fairly Active", "Very Active")), Calories, .group=Id) %>%
  drop_na()

#With this new table I can now visualize both the user type distribution and the calories burned for every user type:
data_by_usertype %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  ggplot(aes(user_type,y=total_percent, fill=user_type)) +
  geom_col()+
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none") +
  labs(title="User type distridution", x=NULL) +
  theme(legend.position="none", text = element_text(size = 20),plot.title = element_text(hjust = 0.5))

ggplot(data_by_usertype, aes(user_type, Calories, fill=user_type)) +
  geom_boxplot() +
  theme(legend.position="none") +
  labs(title="Calories burned by User type", x=NULL) +
  theme(legend.position="none", text = element_text(size = 20),plot.title = element_text(hjust = 0.5))

#I think it will also be interesting to check the relation between Distance/Steps and Calories burned so I plot them:
daily_data %>%
  summarise(
    distance = factor(case_when(
      TotalDistance < 4.5 ~ "< 4.5 mi",
      TotalDistance >= 4.5 & TotalDistance <= 7 ~ "4.5 > & < 7 mi",
      TotalDistance > 7 ~ "> 7 mi",
    ),levels = c("> 7 mi","4.5 > & < 7 mi","< 4.5 mi")),
    steps = factor(case_when(
      TotalSteps < 6000 ~ "< 6k steps",
      TotalSteps >= 6000 & TotalSteps <= 10000 ~ "6k > & < 10k Steps",
      TotalSteps > 10000 ~ "> 10k Steps",
    ),levels = c("> 10k Steps","6k > & < 10k Steps","< 6k steps")),
    Calories) %>%
  ggplot(aes(steps,Calories,fill=steps)) +
  geom_boxplot() +
  facet_wrap(~distance)+
  labs(title="Calories burned by Steps and Distance",x=NULL) +
  theme(legend.position="none", text = element_text(size = 10),plot.title = element_text(hjust = 0.5))

#Now let's focus on the sleep quality, for that I will now make categories for the sleeping time and I will make a new table with the sleeping categories percent for each individual user type:
sleepType_by_userType <- daily_data %>%
  group_by(Id) %>%
  summarise(
    user_type = factor(case_when(
      SedentaryMinutes > mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Sedentary",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes > mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Lightly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes > mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Fairly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes > mean(VeryActiveMinutes) ~ "Very Active",
    ),levels=c("Sedentary", "Lightly Active", "Fairly Active", "Very Active")),
    sleep_type = factor(case_when(
      mean(TotalMinutesAsleep) < 360 ~ "Bad Sleep",
      mean(TotalMinutesAsleep) > 360 & mean(TotalMinutesAsleep) <= 480 ~ "Normal Sleep",
      mean(TotalMinutesAsleep) > 480 ~ "Over Sleep",
    ),levels=c("Bad Sleep", "Normal Sleep", "Over Sleep")), total_sleep = sum(TotalMinutesAsleep) ,.groups="drop"
  ) %>%
  drop_na() %>%
  group_by(user_type) %>%
  summarise(bad_sleepers = sum(sleep_type == "Bad Sleep"), normal_sleepers = sum(sleep_type == "Normal Sleep"),over_sleepers = sum(sleep_type == "Over Sleep"),total=n(),.groups="drop") %>%
  group_by(user_type) %>%
  summarise(
    bad_sleepers = bad_sleepers / total, 
    normal_sleepers = normal_sleepers / total, 
    over_sleepers = over_sleepers / total,
    .groups="drop"
  )

#Now we can plot the data for each user type:
sleepType_by_userType_melted<- melt(sleepType_by_userType, id.vars = "user_type")

ggplot(sleepType_by_userType_melted, aes(user_type, value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x=NULL, fill="Sleep type") + 
  theme(legend.position="right",text = element_text(size = 20),plot.title = element_text(hjust = 0.5))

