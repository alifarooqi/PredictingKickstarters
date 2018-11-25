#installing and using libraries
install.packages("lubridate")
library(lubridate)

#getting dataset
og = read.csv("ks-projects-201801.csv", header = TRUE)
df = og

#remove usd.pledged and goal columns
df$usd.pledged <- NULL
df$goal <- NULL

#calculating duration column
startday = round_date(as.POSIXct(as.character(df$launched)), "day")
endday = as.POSIXct(as.character(df$deadline), format="%Y-%m-%d")
df$duration <- difftime(endday, startday, units='days') + 1

#removing 
df<- df[(df$state == "canceled" | df$state == "failed" | df$state == "live" ), ]

#write cleaned data to file 
write.csv(df, file = "cleaned-ks-data.csv")
