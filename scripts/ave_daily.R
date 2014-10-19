library(plyr)

data <- read.csv("activity.csv", colClasses=c("numeric", "Date", "numeric"))
#data <- na.omit(data)
mean_data <- ddply(data,
                   c("interval"),
                   summarize,
                   mean_steps=mean(steps, na.rm = T))

plot(mean_data$interval,
     mean_data$mean_steps,
     ylab = "Ave. No. of Steps Taken", xlab = "5-Minute Interval", type="l")

max_row <- mean_data[mean_data$mean_steps == max(mean_data$mean_steps), ]
max_5 <- max_row[, c("interval")]