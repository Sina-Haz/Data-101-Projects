library(readr)
library(caret)
library(Metrics)
library(usethis)
usethis::edit_r_environ()

Train.Dataset = read.csv("/Users/shazeghi/Downloads/citybike.train.csv")
test1 = read.csv("/Users/shazeghi/Downloads/citybike.test100withduration.csv")
testA = read.csv("/Users/shazeghi/Downloads/citybike.test10000Awithduration.csv")
final.test = read.csv("/Users/shazeghi/Downloads/citybike.test10000B.csv")
View(Train.Dataset)

compute.distance = function(df){
  distance = sqrt((df$start.station.latitude-df$end.station.latitude)^2+(df$start.station.longitude-df$end.station.longitude)^2)
  df$Distance = distance
  return(df)
}

compute.day = function(df){
  day_num = as.numeric(substr(df$starttime,9,10))
  days = day_num %% 7
  df$Day[days >= 5] = "weekend"
  df$Day[days < 5] = "weekday"
  return(df)
}
Train.Dataset = compute.day(Train.Dataset)
test1 = compute.day(test1)
testA = compute.day(testA)
final.test = compute.day(final.test)

Train.Dataset = compute.distance(Train.Dataset)
test1 = compute.distance(test1)
testA = compute.distance(testA)
final.test = compute.distance(final.test)


Model = lm(tripduration~Distance+gender+birth.year+usertype+Day,data = Train.Dataset)
summary(Model)
predict100 = predict(Model,test1)
rmse(test1$tripduration,predict100)

predictA = predict(Model,testA)
rmse(testA$tripduration,predictA)

#my rmse for testA with my Model is 13272.66

final.prediction = predict(Model,final.test)
write.csv(final.prediction,file="skh79-12-kagglesubmission.csv")

submission = read.csv("/Users/shazeghi/skh79-12-kagglesubmission.csv")
View(submission)

colnames(submission) = c("Id","Predicted")

write.csv(submission,file = "skh79-12-kagglesubmission.csv",row.names = FALSE)

