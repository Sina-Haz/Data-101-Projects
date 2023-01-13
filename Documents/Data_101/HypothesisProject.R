library(readr)

CitiData = read.csv("/Users/shazeghi/Documents/Data 101/citibike.May20.50K.csv")


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




#Do wommen ride longer than men?
CitiMen = subset(CitiData, CitiData$gender == 1)
CitiWomen = subset(CitiData, CitiData$gender == 2)

men_stats = explore(CitiMen$tripduration)
women_stats = explore(CitiWomen$tripduration)

gender_sd_stats = sqrt(men_stats[3]^2/men_stats[4] + women_stats[3]^2/women_stats[4])
gender_sd_stats

men_stats[1]
women_stats[1]

z_score2 = (women_stats[1] - men_stats[1])/gender_sd_stats
z_score2

pnorm(-abs(z_score2))

