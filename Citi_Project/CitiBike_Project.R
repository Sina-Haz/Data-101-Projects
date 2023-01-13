library(readr)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("lubridate")
library(lubridate)

CitiData = read.csv("/Users/shazeghi/Documents/Data 101/citibike.May20.50K.csv")
StartlocationFreq = table(CitiData$start.station.name)
barplot(StartlocationFreq,main = "Starting Locations",xlab = "location",ylab = "frequency",col = "blue")

#Filtering out data to find more popular start locations
newStart = subset(StartlocationFreq,StartlocationFreq > 200)

#barplot to show popular locations
barplot(newStart,main = "Frequent Starting Locations",xlab = "location",ylab = "frequency",col = "blue")
names(newStart)

#Filtering data to look at customers not subscribed
CitiCasual = subset(CitiData, CitiData$usertype == "Customer")
hist(2020 - CitiCasual$birth.year, main = "Ages of non-subscribers",xlab = "age",ylab = "Frequency",col = "dark red")


#scatter plot to show more general trends
scatter.smooth(2020 - CitiData$birth.year,CitiData$tripduration, xlab = "Age",ylab = "Trip Length", main = "Age vs Trip Duration")


#want to test hypothesis that people under 35 ride longer than people over 35

#subsets of data
under35 = subset(CitiData, (2020 - CitiData$birth.year) < 35)
over35 = subset(CitiData,(2020 - CitiData$birth.year) >= 35)

#null hypothesis: Both age groups have about same trip length
#alternate hypothesis: trip length for those under 35 > than for those over 35 (one tailed test)

#function that returns statistical values for data in x
explore<-function(x){
  data<-c("Mean"=mean(x, na.rm=TRUE),
          "Median"=median(x, na.rm =T), 
          "Standard Deviation" = sd(x, na.rm =T),
          "Length" = length(x))
  return(data)
}

under35_stats = explore(under35$tripduration)
over35_stats = explore(over35$tripduration)

#standard deviation acquired
sd_stats = sqrt(under35_stats[3]^2/under35_stats[4] + over35_stats[3]^2/under35_stats[4])

#get z score
z_score = (under35_stats[1] - over35_stats[1])/sd_stats
z_score

#get p-value
pnorm(-abs(z_score))

#since p-value much greater than 0.05 we cannot reject null hypothesis
View(CitiData)


#Do men ride longer than women?

