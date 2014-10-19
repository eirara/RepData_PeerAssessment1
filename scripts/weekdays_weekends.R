library(plyr)
library(ggplot2)

data <- read.csv("activity.csv", colClasses=c("numeric", "Date", "numeric"))
data <- transform(data, 
                  steps = ifelse(is.na(steps), 
                                 floor(ave(steps,
                                       interval,
                                       FUN = function(x) mean(x, na.rm = TRUE))), 
                                 steps))

data$weekDayEnd <- ifelse(!weekdays(as.Date(data$date)) %in% c("Saturday", "Sunday"),
                         "weekday","weekend")

new_data <- ddply(data, c("interval","weekDayEnd"), summarize, mean_steps=mean(steps))

#ggplot(d,aes(Sepal.Length,Sepal.Width)) +geom_line() + facet_grid(.~Species)
print(ggplot(new_data, aes(interval, mean_steps)) + 
      geom_line() + 
      facet_grid(weekDayEnd~.) + 
      xlab("Interval") +
      ylab("Number of Steps")) +
      ggtitle("Time Series Plot of the 5-Minute Interval\nand the Average No. of Steps Taken
                (Across All Weekend or Weekday Days)")


