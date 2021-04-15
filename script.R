library(tidyverse)
library(lubridate)

#1. load data
activity <- 
    read_csv("activity.csv")


#2. histogram of total steps per day
activity %>%
    group_by(date) %>%
    summarize(tot_steps = sum(steps, na.rm = TRUE)) %>%
    ggplot(aes(date, tot_steps)) + geom_col() +
    xlab("Date") + ylab("Total Steps")


#3. mean and median number of steps per day
activity %>%
    group_by(date) %>%
    summarize(mean = mean(steps, na.rm = TRUE),
              median = median(steps, na.rm = TRUE))


#4. time series plot of average number of steps taken
activity %>%
    group_by(date) %>%
    summarize(avg = mean(steps, na.rm = TRUE)) %>%
    ggplot(aes(date, avg)) + geom_point() + geom_line() +
    xlab("Date") + ylab("Average Number of Steps")

#5. five minute interval that contains on average the maximum number of steps
activity %>%
    group_by(interval) %>%
    summarize(avg_steps = mean(steps, na.rm = TRUE)) %>%
    arrange(desc(avg_steps)) %>%
    `[`(1,)


#6. impute missing data
activity %>%
    group_by(date) %>%
    summarize(miss = sum(is.na(steps))) %>%
    ggplot(aes(date, miss)) + geom_point()

activity %>%
    group_by(date) %>%
    summarize(n())

missing_days <- 
    activity %>%
    group_by(date) %>%
    summarize(miss = sum(is.na(steps))) %>%
    filter(miss > 0) %>%
    `$`(date)
#days with missing values have all observations as missing
#therefore, cannot impute by using neighboring observations

#potentially impute by averaging across other days' intervals

activity %>%
    filter(interval == 1830) %>%
    ggplot(aes(date, steps)) +
    geom_point()

activity %>%
    group_by(interval) %>%
    summarize(mean = mean(steps, na.rm = TRUE),
              median = median(steps, na.rm = TRUE),
              std = sd(steps, na.rm = TRUE))
#baselines are pretty much all at 0 steps
#with every interval having serious outliers
#imputing with the mean would likely lead to distortion
#will impute with the median instead

act_imp <- 
    activity %>%
        group_by(interval) %>%
        mutate(median = median(steps, na.rm = TRUE),
               steps = if_else(is.na(steps), median, steps)) %>%
        select(-median)

#7. histogram after impute
act_imp %>%
    group_by(date) %>%
    summarize(tot_steps = sum(steps)) %>%
    ggplot(aes(date, tot_steps)) + geom_col() +
    xlab("Date") + ylab("Total Steps")


#8 avg steps across weekends and weekdays per interval
act_imp %>%
    mutate(day = wday(date),
           class = if_else(day == 7 | day == 1, "weekend", "weekday"),
           class = factor(class)) %>%
    group_by(class, interval) %>%
    summarize(avg_steps = mean(steps)) %>%
    ggplot(aes(interval, avg_steps)) + geom_col() + facet_wrap(~class) +
    xlab("Interval") + ylab("Average Number of Steps")