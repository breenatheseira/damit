# This app can change values daily:
# Day 1, Month: Jan, River: Galas, Dabong, Wind: False, Rain: No - Very Heavy & Extreme

library(car)
library("rpart")
library("rpart.plot")
library("ggplot2")

dam_prediction <- get(load('./data/River.rda'))

#dam_prediction$Wind<- recode(dam_prediction$Wind, "0:1.5='False'; 1.6:21.0='True'")

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

  return(result = predict(fit,newdata=newdata,type=c("class")))
}

monthArr <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
daysArr <- c(31,28,31,30,31,30,31,31,30,31,30,31)

new_date <- function(day = "1", month = "January", duration){

  day = "31"
  month = "March"
  duration
  total_days <- as.numeric(day) + duration

  for (i in 1:length(monthArr)){
    if (month == monthArr[i]){
      if (total_days < daysArr[i]){
        new_day <- total_days
        new_month <- month
      } else {
        new_day <- total_days - daysArr[i]
        # for Dec to return to January
        if (month == "December")
          new_month <- monthArr[1]
        else
          ner_month <- monthArr[i+1]
      }
    }
  }
  return(paste(new_day, new_month, sep = " "))
}
