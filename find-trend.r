library(data.table)
library(ggplot2)
library(pROC)

# To be modified if using mac
ksdf = read.csv("C:\\Users\\ali1997\\Downloads\\PredictingKickstarters\\ks-projects-201801.csv", header = TRUE)

# Converting the launch and deadline Dates to correct format:
ksdf$launch_date <- as.Date(ksdf$launched, "%Y-%m-%d")
ksdf$deadline_date <- as.Date(ksdf$deadline, "%Y-%m-%d")

# also addding month and year columns for the deadline and launch dates:
ksdf$launch_year <- substr(ksdf$launched, 1,4)
ksdf$launch_mth <- substr(ksdf$launch_date, 1,7)

ksdf$final_year <- substr(ksdf$deadline,1,4)
ksdf$final_mth <- substr(ksdf$deadline, 1, 7)

ksdf[1:5, c('ID', 'name', 'main_category', 'launch_year', 'launch_mth', 'final_year', 'final_mth')]

# No of projects by year
table(ksdf$launch_year)

# All categories in a year (2016) [Only to visualize]
ggplot(subset(ksdf,launch_year %in% c("2016")), aes(x=main_category)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') +
  facet_wrap(~launch_year)

# Top 3 categories in a year (2016) [Used in for loop later]
names(head(sort(table(subset(ksdf,launch_year %in% c("2016"))$main_category), decreasing = T), 3))

# Top 3 categories in each year
all_years = sort(unique(ksdf$launch_year))
trend = data.frame(matrix(NA, nrow = 3, ncol = length(all_years)))
idx = 1
for (year in all_years){
  topThree = names(head(sort(table(subset(ksdf,launch_year %in% c(year))$main_category), decreasing = T), 3))
  trend[, idx] = topThree
  names(trend)[idx] <- year
  idx = idx+1
}

#Display trends
trend