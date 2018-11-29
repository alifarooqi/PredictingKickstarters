install.packages("RColorBrewer") 
library("RColorBrewer") 

ksdf = read.csv("../CSV files/ks-projects-201801.csv", header = TRUE)
ksdf.proj = subset(ksdf, state %in% c("failed", "successful", "live"))
ksdf.proj$year = format(as.Date(ksdf.proj$launched), "%Y")

#showing category success/fail by year
myFunc <- function(vec){
  length(vec[vec=="successful"])/(length(vec))
}

ht1 = aggregate(ksdf.proj2$state, by=list(year=ksdf.proj2$year, main_category=ksdf.proj2$main_category), FUN=myFunc)
colnames(ht1)[3] <- "ratio_of_success"

ggplot(ht1, aes(x = reorder(main_category,ratio_of_success), y = ratio_of_success, fill=main_category)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~year)+ 
  theme(axis.text.x = element_blank(), legend.position = "bottom") + 
  scale_fill_manual(values=colorRampPalette(brewer.pal(8,"Accent"))(15), guide = guide_legend(nrow=2))+
  xlab("Main categories")

# goal and pledged by categories over the years
ksdf.proj2 = ksdf.proj[!ksdf.proj$year==2018,]

helpfulthing1 = aggregate(ksdf.proj2$usd_goal_real, by=list(year=ksdf.proj2$year, main_category=ksdf.proj2$main_category), FUN=mean)
helpfulthing2 = aggregate(ksdf.proj2$usd_pledged_real, by=list(year=ksdf.proj2$year, main_category=ksdf.proj2$main_category), FUN=mean)

total <- merge(helpfulthing1, helpfulthing2,by=c("year", "main_category"))
total$percentage <- total$x.y/total$x.x
colnames(total)[3] = "goal"
colnames(total)[4] = "pledged"

#goal
ggplot(total, aes(x = main_category, y = goal, fill=main_category)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~year)+ 
  theme(axis.text.x = element_blank(), legend.position = "bottom") + 
  scale_fill_manual(values=colorRampPalette(brewer.pal(8,"Accent"))(15), guide = guide_legend(nrow=2))

#pledged
ggplot(total, aes(x = main_category, y = pledged, fill=main_category)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~year)+ 
  theme(axis.text.x = element_blank(), legend.position = "bottom") + 
  scale_fill_manual(values=colorRampPalette(brewer.pal(8,"Accent"))(15), guide = guide_legend(nrow=2))

#percentage/ratio
ggplot(total, aes(x = main_category, y = percentage, fill=main_category)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~year)+ 
  theme(axis.text.x = element_blank(), legend.position = "bottom") + 
  scale_fill_manual(values=colorRampPalette(brewer.pal(8,"Accent"))(15), guide = guide_legend(nrow=2))

#duration and pledged over years
cleaned = read.csv("../CSV files/cleaned-ks-data.csv", header = TRUE)
cleaned$year =  format(as.Date(cleaned$launched), "%Y")
cleaned.proj = cleaned[!cleaned$year==2018,]

#duration over years
ht3 = aggregate(x=cleaned.proj$duration, by =list(year=cleaned.proj$year), FUN=mean)
ggplot(ht3, aes(x=year, y=x)) + geom_point() + labs(y="duration")

#pledged over years
ht4 = aggregate(x=cleaned.proj$pledged, by =list(year=cleaned.proj$year), FUN=mean)
ggplot(ht4, aes(x=year, y=x)) + geom_point() + labs(y="pledged")



