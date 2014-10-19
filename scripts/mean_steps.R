library(plyr)

data <- read.csv("activity.csv", colClasses=c("numeric", "Date", "numeric"))
#data <- na.omit(data)
sum_steps <- ddply(data,.(date),summarize,total_steps=sum(steps))
hist(sum_steps$total_steps,
     xlab = "Number of Steps",
     main = "Histogram of the Total No. of Steps Taken Each Day",
     col="red")

mean_ <- mean(sum_steps$total_steps, na.rm = T)
median_ <- median(sum_steps$total_steps, na.rm = T)