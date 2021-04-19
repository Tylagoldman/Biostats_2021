#19 April 2021
#Intro_R_part_2_day_1 
#Tyla Goldman 3837300

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)


data(package = .packages(all.available = TRUE))
#typing ?BOD into console tells us whether data exists in r 
#typing BOD in console gives an idea of nature of data- numerical data 
#str displays the internal structure of data 
#data frame looks like a spreadsheet and contains various data 
#'num"- floating point numbers 
#'Órd.factor" - ordereed factor-level of order in the data, for example, small to large 

view(Nile)

#Load built-in data as an R object 
Loblolly
pines <- Loblolly
str(pines)
head(pines)
View(pines)
class(pines$height)

view(Nile)

#samplesa and population 
#population is data from the overall study 
#sample is a subset of data extracted from the population of data- needs to be unbias, hold variety 
#as sample size increases, variation decreases 

#calculate sample size 
chicks <- as_tibble(ChickWeight)
nrow(chicks) #cant be sample size as it includes the repetitive data for the smae chicks 
unique(chicks$Chick) #unique counts the number of unique levels at a particular column. 
#THEREFORE n=50

view(chicks)
library(e1071)

chicks %>%
  filter(Time == "20") %>%
  group_by(Diet) %>%
  summarise(mean_weight = mean(weight), sd_weight = sd(weight), median_weight = median(weight), kurtosis_weight = kurtosis(weight),
            min_weight = min(weight),
            qrt1_weight = quantile(weight, p=0.25),
            median_weight = median(weight),
            qrt3_weight = quantile(weight, p=0.75),
            max_weight = max(weight),
            lower_weight = range(weight)[1],
            upper_weight = range(weight)[2])

chicks %>%
  filter(Time == "20") %>%
  summarise(mean_weight = mean(weight), sd_weight = sd(weight), median_weight = median(weight), kurtosis_weight = kurtosis(weight))

#can observe whether data is normally distributed where mean=median 

#kurtosis- describes the shape/properties of the tail. 
#normal distribution- kurtosis = 0 
#negative kurtosis- data have a thin tail 
#positive kurtosis- data have a fat tail 
#THEREFORE- THE ABOVE DATA WEIGHT IS 