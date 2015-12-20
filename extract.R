assignment <- function(){
  completeSteps <- read.csv('data/activity.csv')
  minuteAverage <- tapply(completeSteps$steps, completeSteps$interval, mean, na.rm = TRUE)
  DFminuteAverage <- as.data.frame(minuteAverage)
  dailySteps <- tapply(completeSteps$steps, completeSteps$date, sum, na.rm = TRUE)
  dailyStepsMean <- mean(dailySteps)
  dailyStepsMedian <- median(dailySteps)
  # hist(dailySteps, main = 'Daily Steps Average', col = 'blue')
  minutes <- dateFormat(rownames(DFminuteAverage))
  max_performance <- minutes[which(minuteAverage == max(minuteAverage))]
  # idealMinute <- substr(max_performance, 12, 16)
  completeSteps$processed <- 0
  for (iter in 1:length(completeSteps$steps)){
    if (is.na(completeSteps$steps[iter])){
      completeSteps$processed[iter] <- minuteAverage[((iter-1) %% length(minuteAverage))+1]
    } else {
      completeSteps$processed[iter] <- completeSteps$steps[iter]
    }
  }
  minute_mod <- tapply(completeSteps$processed, completeSteps$interval, mean, na.rm = TRUE)
  newMinuteAverage <- tapply(completeSteps$processed, completeSteps$interval, mean)
  newDailySteps <- tapply(completeSteps$processed, completeSteps$date, sum)
  newDailyStepsMean <- mean(newDailySteps)
  newDailyStepsMedian <- median(newDailySteps)
  weekend <- c('sábado', 'domingo')
  weekClass <- ifelse(weekdays(as.POSIXct(completeSteps$date,
                                          format = '%Y-%m-%d')) %in% weekend,
                      'weekend', 'weekday')
  hist(newDailySteps, col = 'red')
}

dateFormat <- function(cadena){
  as.POSIXct(substr(paste('000', cadena, sep = ''),
                    nchar(cadena), nchar(cadena)+3), format = '%H%M')
}
