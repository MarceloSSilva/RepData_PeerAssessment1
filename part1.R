part1 <- function() {
        
        library(dplyr)
        
        # load fiel
        activity <- read.csv("activity.csv", na.strings="NA")
        
        # select only steps and date columns
        steps_date <- subset(activity, , select = c("steps", "date"))
                
        #png(file= "plot1.png", width = 580, height = 580)
        
        sum.steps <- steps_date %>% group_by(date) %>% summarise_each(funs(sum))
        
        hist(sum.steps$steps, main = paste ("Histogram of steps"), xlab = "steps", ylim = c(0,30))
        mean(sum.steps$steps, na.rm = TRUE)
        median(sum.steps$steps, na.rm = TRUE)
        
        
        # part 2
        
        #2.1
        steps_interval <- subset(activity, , select = c("steps", "interval"))
        mean.steps <- steps_interval %>% group_by(interval) %>% summarise_each(funs(mean(., na.rm = TRUE)))
        with(mean.steps, plot(mean.steps$steps,type = "l"))
        
        #2.2
        max(mean.steps$steps)
        
        # 3.1
        sum(is.na(activity$steps))
        
        # 3.2
        activity2 <- activity
        merged <- merge(activity2, mean.steps, by = "interval")
        for ( i in 1:nrow(activity2)) {
                if (is.na(activity2$steps[i]) == TRUE) {
                        activity2$steps[i] = merged$steps.y[i]
                }
        }
        #
        # 3.3 
        steps_date2 <- subset(activity2, , select = c("steps", "date"))
        sum.steps2 <- steps_date2 %>% group_by(date) %>% summarise_each(funs(sum))
        
        hist(sum.steps2$steps, main = paste ("Histogram of steps"), xlab = "steps", ylim = c(0,30))
        mean(sum.steps2$steps, na.rm = TRUE)
        median(sum.steps2$steps, na.rm = TRUE)
        
        
        library(lubridate)
        for ( i in 1:nrow(activity2)) {
                if (wday(ymd(activity$date[i])) == 1 ) {
                        activity2$weekday[i] = "weekend"
                } else
                        if (wday(ymd(activity$date[i])) == 7 ) {
                                activity2$weekday[i] = "weekend" 
                        } else
                                activity2$weekday[i] = "weekday"
        }
        
        
        #dev.off()
        library(lattice)
        steps_date3 <- subset(activity2, , select = c("steps", "interval", "weekday"))
        mean.steps3 <- steps_date3 %>% group_by(interval, weekday) %>% summarise_each(funs(mean))
        xyplot(steps ~ interval | weekday, data = mean.steps3, type = "l")