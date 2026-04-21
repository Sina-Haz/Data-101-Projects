install.packages("devtools")
devtools::install_github("devanshagr/PermutationTestSecond",force=TRUE)
library(readr)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("lubridate")
library(lubridate)

historic_temp = read.csv("/Users/shazeghi/Documents/Data 101/historic_temp.csv")
View(historic_temp)
US_temps = subset(historic_temp,historic_temp$Country == "United States                           ")
View(US_temps)

#claim: the average temperature has changed over the past 50 years from 1940 to 1990(two-tailed)
Data_from_fourties = subset(US_temps,US_temps$Year == 1940)
Data_from_nineties = subset(US_temps,US_temps$Year == 1990)

explore<-function(x){
  data<-c("Mean"=mean(x, na.rm=TRUE),
          "Median"=median(x, na.rm =T), 
          "Standard Deviation" = sd(x, na.rm =T),
          "Length" = length(x))
  return(data)
}

data1 = explore(Data_from_fourties$AverageCelsiusTemperature)
data2 = explore(Data_from_nineties$AverageCelsiusTemperature)


sd_data = sqrt(data1[3]^2/data1[4] + data2[3]^2/data2[4])

z_score = (data2[1] - data1[1])/sd_data
p_val = 2*pnorm(-abs(z_score))

z_score
p_val



# PermutationTestSecond::Permutation(<dataframe_name>, <cat_column_name>, <num_col_name>,
# <n_permutations>, <cat_name_lower_mean>, <cat_name_higher_mean>)

#Actual Test (this gives p-val of 0 so we know that there 
#definitely has been a rise of temperature over the 60 year gap)
PermutationTestSecond::Permutation(US_temps, "Year", "MaxCelsiusTemp",1000, "1925", "1985")

#Auxiliary Test to make sure Permutation Test working properly. It is b/c this gives
#a p-val of .204 which means that we should keep null hypothesis that these two groups have about 
#the same temperature
PermutationTestSecond::Permutation(US_temps, "Year", "MaxCelsiusTemp", 
                                                         1000, "1983", "1985")


