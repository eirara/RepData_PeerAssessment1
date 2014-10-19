library(plyr)

data <- read.csv("activity.csv", colClasses=c("numeric", "Date", "numeric"))
#data <- na.omit(data)
na_count <- nrow(data) - nrow(na.omit(data))

new_data <- transform(data, 
          steps = ifelse(is.na(steps), 
                          floor(ave(steps,
                                interval,
                                FUN = function(x) mean(x, na.rm = TRUE))), 
                          steps))

new_na_count <- nrow(data) - nrow(na.omit(new_data))

new_sum_steps <- ddply(new_data,.(date),summarize,total_steps=sum(steps, na.rm = F))

hist(new_sum_steps$total_steps,
     xlab = "Total Number of Steps Taken Each Day",
     main = "",
     col="red")

new_mean <- mean(new_sum_steps$total_steps, na.rm = T)
new_median <- median(new_sum_steps$total_steps, na.rm = T)
