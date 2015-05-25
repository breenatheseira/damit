# This app can change values daily:
# Day 1, Month: Jan, River: Galas, Dabong, Wind: False, Rain: No - Very Heavy & Extreme

library(car)
library("rpart")
library("rpart.plot")
library("ggplot2")

dam_prediction <- get(load('./data/River.rda'))
sample_data <- data.frame(get(load('./data/DailyValues.rda')))
sample_data$Wind<- recode(sample_data$Wind, "FALSE='False'; TRUE ='True'")

fit <- rpart(Rank_Dam ~ Day + Month + River + Wind + Rain,method="class", data=dam_prediction,control=rpart.control(minsplit=4))

damplot <- function() {
  rpart.plot(fit, type=4, extra=1)
}

damcategory <- function(daySelect = "1", monthSelect = "January", riverSelect = "Sungai Galas, Dabong", rainSelect = "1", windSelect = "True"){

  newdata <- data.frame(
    Day = as.numeric(daySelect),
    Month = monthSelect,
    River= riverSelect,
    Rain =as.numeric(rainSelect),
    Wind = windSelect)

  return(predict(fit,newdata=newdata,type=c("class")))
}

new_date <- function(day = "1", month ="January"){

  monthArr <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  daysArr <- c(31,28,31,30,31,30,31,31,30,31,30,31)

  total_days <- as.numeric(day)
  for (i in 1:length(monthArr)){
    if (month == monthArr[i]){
      if (total_days <= daysArr[i]){
        new_day <- total_days
        new_month <- month
      } else {
        new_day <- total_days - daysArr[i]
        # for Dec to return to January
        if (month == "December")
          new_month <- monthArr[1]
        else
          new_month <- monthArr[i+1]
      }
    }
  }
  return(paste(new_day, new_month, sep = " "))
}

river_result <- function(day = "1", month ="January", river = "Sungai Galas, Dabong", rain = "1", wind = "True"){

  # today's result
  date <- new_date(day, month);
  split <- unlist(strsplit(date, " ", fixed = TRUE))
  today <- damcategory(day,month,river, rain, wind)
  return(today)
}

bundled_result <- function(){
  array <- NULL

  for (i in 1:30){
    today <- river_result(sample_data[i,1],sample_data[i,2],"Sungai Galas, Dabong",sample_data[i,3],sample_data[i,4])
    tomorrow <- river_result(sample_data[i+1,1],sample_data[i+1,2],"Sungai Galas, Dabong",sample_data[i+1,3],sample_data[i+1,4])
    week1 <- river_result(sample_data[i+7,1],sample_data[i+7,2],"Sungai Galas, Dabong",sample_data[i+7,3],sample_data[i+7,4])
    week2 <- river_result(sample_data[i+14,1],sample_data[i+14,2],"Sungai Galas, Dabong",sample_data[i+14,3],sample_data[i+14,4])
    month <- river_result(sample_data[i+30,1],sample_data[i+30,2],"Sungai Galas, Dabong",sample_data[i+30,3],sample_data[i+30,4])
    array[i] <- paste(today, tomorrow, week1, week2, month, sep = " ")
  }

  return(array)
}
