# This app can change values daily:
# Day 1, Month: Jan, River: Galas, Dabong, Wind: False, Rain: No - Very Heavy & Extreme

library(car)
library("rpart")
library("rpart.plot")
library("ggplot2")

dam_prediction <- get(load('./data/River.rda'))

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

new_date <- function(date = "1 January", duration){

  monthArr <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  daysArr <- c(31,28,31,30,31,30,31,31,30,31,30,31)

  date <- unlist(strsplit(date, " ", fixed = TRUE))
  day = date[1]
  month = date[2]

  total_days <- as.numeric(day) + as.numeric(duration)

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
          new_month <- monthArr[i+1]
      }
    }
  }
  return(paste(new_day, new_month, sep = " "))
}

river_result <- function(date = "1 January", river = "Sungai Galas, Dabong", rain = "1", wind = "True"){
  # today's result
  split <- unlist(strsplit(date, " ", fixed = TRUE))
  today <- damcategory(split[1],split[2],river, rain, wind)

  # tomorrow
  date <- new_date(date, 1);
  split <- unlist(strsplit(date, " ", fixed = TRUE))
  tomorrow <- damcategory(split[1],split[2],river, rain, wind)

  # after 1 week
  date <- new_date(date, 7);
  split <- unlist(strsplit(date, " ", fixed = TRUE))
  after1_week <- damcategory(split[1],split[2],river, rain, wind)

  # after 1 fortnight
  date <- new_date(date, 14);
  split <- unlist(strsplit(date, " ", fixed = TRUE))
  after1_week <- damcategory(split[1],split[2],river, rain, wind)

  # after 1 month
  date <- new_date(date, 30);
  split <- unlist(strsplit(date, " ", fixed = TRUE))
  after_1month <- damcategory(split[1],split[2],river, rain, wind)

  return(paste(today, tomorrow, after1_week, after1_week, after_1month))
}
