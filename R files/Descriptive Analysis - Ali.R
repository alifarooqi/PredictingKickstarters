library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) 


ksdf = read.csv("../CSV files/ks-projects-201801.csv", header = TRUE)
ksdf.proj = subset(ksdf, state %in% c("failed", "successful"))

#Data preprocessing
ksdf.proj$deadline <- as.Date(ksdf.proj$deadline)
ksdf.proj$launched <- as.Date(ksdf.proj$launched)
ksdf.proj$duration <- as.numeric(ksdf.proj$deadline - ksdf.proj$launched)


# Final State of the KS project
ggplot(ksdf.proj, aes(state, fill = state)) +
  geom_bar() +
  ylab("# of Projects") + xlab("Final State") +
  theme(legend.position = "bottom") +
  ggtitle("Final State of the Kickstarter projects")

# Main Categories present in the dataset
ggplot(ksdf.proj, aes(x = main_category, fill = ksdf.proj$state)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("") +
  ggtitle("Main categories of the KS Projects") +
  scale_fill_discrete("Project status")

# Duration of Projects
ggplot(ksdf.proj, aes(duration, fill = ksdf.proj$state)) +
  geom_histogram(binwidth = 5) +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("Duration (Days)") +
  ggtitle("Duration of the KS projects (in days)")

# Duration against % success of Projects [Failed]
all_durations = sort(unique(ksdf.proj$duration))
durationVsuccess = data.frame(matrix(NA, nrow=length(all_durations), ncol=2))
index = 1
for (d in all_durations){
  durationVsuccess[index, 1] = d
  s = subset(ksdf.proj, duration == d)
  ss = subset(s, state == "successful")
  durationVsuccess[index, 2] = nrow(ss)/nrow(s)*100
  index = index +1
}
ggplot(data=durationVsuccess, aes(x=X1, y=X2)) +
  geom_histogram(binwidth = 5)
  
ggplot(durationVsuccess, aes(X1, X2)) +
  geom_histogram(binwidth = 5) + 
  scale_y_continuous(labels=percent)
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("Duration (Days)") +
  ggtitle("Duration of the KS projects (in days)")

# Box plot of duration of successful and unsuccessful projects
ggplot(ksdf.proj, aes(x = state, y = duration, fill = ksdf.proj$state)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  coord_flip() +
  xlab("") + ylab("Duration") +
  ggtitle("Active Duration of the KS projects")+
  scale_fill_discrete("Project status")

# No of backers against success of project 
ggplot(ksdf.proj, aes(x = state, y = log(backers), fill = ksdf.proj$state)) +
  geom_boxplot() +
  coord_flip() + 
  theme(legend.position = "bottom") +
  ylab("# of Backers (log-transformed)") + xlab("") +
  ggtitle("# of Backers of the KS projects (Log)")+
  scale_fill_discrete("Project status")

# Amount pledged against success of project
ggplot(ksdf.proj, aes(x = state, y = log(usd_pledged_real), fill = ksdf.proj$state)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ylab("USD pledged (log-transformed)") + xlab("") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + 
  ggtitle("USD pledged for the KS projects (Log)")+
  scale_fill_discrete("Project status")

# Goal against success of project
ggplot(ksdf.proj, aes(x = state, y = log(usd_goal_real), fill = ksdf.proj$state)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ylab("Goal in USD (log-transformed)") + xlab("") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + 
  ggtitle(" Goal of the KS projects (Log)")+
  scale_fill_discrete("Project status")

# Backers by category
ggplot(data = ksdf.proj, aes(x = main_category, y = backers, fill = state)) + 
  ylab("# of backers") + xlab("Category") +
  geom_bar(stat= "identity")

# USD Pledged by category
ggplot(data = ksdf.proj, aes(x = main_category, y = usd_pledged_real, fill = state)) + 
  ylab("US$ Pledged") + xlab("Category") +
  geom_bar(stat= "identity")