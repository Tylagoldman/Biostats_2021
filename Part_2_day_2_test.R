#Spot test 1
#Tyla Goldman(3837300)

#QUESTION 1

#Data classes are mainly dominated by quantitative and qualitative data. for starters quantitative data is made up of continuous data and discrete data, this is collectivelly considered to be numerical data hence the data being "quantitative", to explain; continuous data involves measured data, date exists as a special type of continuous data; temperature is another example of continuous data; while discrete data involves intiger/whole number type of data; for example, #number of cars <- c(1, 5, 2, 10). 
#Qualitative data is rather descriptive data such as ordinal data that most obvious represents data with a particular structured order to it; for example, positions first/ second/ third, or character data, for example the sample numbers 

glimpse()
view()
str()
head()
tail()
summary()

#Skewness in lamis terms deal with observing whether the bulk of ones data lies on either side of the mean/ median measurement of data- this will result in 'skewed data'
#skewed data can exist positively or negatively dependent on the directon to which the bulk of the data is skewed.
#Kurtosis is a function of r that descrbes the positive/ negative skewness of the data 
#unskewed data involved data that is relatively eqally distributed on either side of the mean/median, well seen through graphical display of histogram containing a bell shape    

#QUESTION 2

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(scales)
library(ggsn)
library(maps)
library(ggrepel)
library(dplyr)
library(dslabs)


data(package = .packages(all.available = TRUE))

Orange <- datasets::Orange
view(Orange) #general observatoions of data 
str(Orange)
head(Orange)
tail(Orange)
summary(Orange)

#From the internal structure analysis of the dataset using 'str' function:
#it can be seen that ordinal/ factorial data is made up/ described through numerical data- the ages and circumferences  

#first 6 rows of data
head(Orange)

#Last 6 rows of data 
tail(Orange)

#column names of data 
colnames(Orange)

#Summary statistics of data
summary(Orange)

#Mean, median, standard deviation of orange data 
Orange %>%
  group_by(Tree) %>%
  summarise(mean_age = mean(age), mean_circumference = mean(circumference),
            median_age = median(age), median_circumference = median(circumference),
            sd_age = sd(age), sd_circumference = sd(circumference))

#Skewness and kurtosis of orange data 
library(e1071)

Orange %>%
  summarise(kurtosis_age = kurtosis(age), kurtosis_circumference = kurtosis(circumference)) #platykurtic kurtosis 
skewness(Orange$age) #this data part is left-skewed
skewness(Orange$circumference) #this data is right_skewed 

#Min, max, first & third quartile of circumference 
Orange %>%
  summarise(min_circumference = min(circumference),
            max_circumference = max(circumference),
            qrt1_circumference = quantile(circumference, p = 0.25),
            qrt3_circumference = quantile(circumference, p = 0.75))
#plots 
Orange %>%
  group_by(Tree) %>%
  summarise(mean_age = mean(age),
            sd_age = sd(age))
ggplot(data = Orange) +
  geom_col(aes(x = Tree, y = age), fill = "purple", col = "black") +
  geom_errorbar(aes(ymin = mean_age - sd_age,
                    ymax = mean_age + sd_age,
                    x = Tree), col = "black", width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Tree", y = "Age(years)", title = "Column graph showing the ages of oranges on respective trees")

