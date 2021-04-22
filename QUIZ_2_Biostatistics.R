#Tyle Noelle Goldman 3837300
#quiz 2

#QUESTION 1
library(tidyverse)
library(corrplot)
library(ggpubr)
library(lubridate)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(ggrepel)
library(dslabs)
library(reshape2)
library(plotly)
library(reshape)

Orange <- datasets::Orange
view(Orange)
head(Orange)
tail(Orange)
str(Orange)
summary(Orange)

Orange_edit <- Orange %>%
  gather(circumference, age, key = "variable", value = "value")
Orange_edit
ggplot(data = Orange_edit, aes(x = variable, y = value, fill = Tree)) + #first step is always to plot data 
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplot showing a circumference of Trees")

#Ho = The circumferece of tree 3 and 1 is similar 
#H1 = The circumference of tree 3 and 1 is similar  

#THEREFORE doing correlation to test for similarity 

two_assum <- function(x) { #generate own function 
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}


Orange_edit %>% 
  group_by(Tree) %>% 
  summarise(circumference_var = two_assum(value)[1],
            circumference_norm = two_assum(value)[2])
#assumptions review for correlation:  
#normality distribution absent because p-value is not greater than 0.05 
#data is homoscedastic as variance of variables less than 4 times  
#linearaty 
#absenece of outliers 
#pair-wise daat 
#continuous data 

#Kendall rank correlation will be used ad non normal data is present 

Orange_edit$circumference <- as.numeric(cut((Orange_edit$), breaks = 3))

cor.test(Orange_edit$circumference, Orange_edit$Tree, method = "kendall")
box

ToothGrowth <- datasets::ToothGrowth
ToothGrowth

Toothgrowth_edit <- ToothGrowth %>%
  gather(len, dose, key = "variable", value = "value")

ggplot(data = Toothgrowth_edit, aes(x = variable, y = value, fill = supp)) + #first step is always to plot data 
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplot showing difference in OJ and VC supplement")
#Ho = there is no significant difference between the toothlenth for different supp
#H1 = there is a significant difference for toothlenth of supp 
Toothgrowth_edit %>% 
  group_by(supp) %>% 
  summarise(len_var = two_assum(value)[1],
            len_norm = two_assum(value)[2])
#observing the assumptions for Shapiro test 

#not homoscedistic - varience is greater than four times the different supplements  
#not normally distributed data- p value not greater than 0.05


compare_means(len ~ supp, data = ToothGrowth, method = "wilcox.test")
#using an althernitive anova test as the Shapiro test did will not accomodate un normal data  
#RESULTS = the p value is greater than 0.05 therefore a significant difference does not exist among the toothlenth for different supplements 

#QUESTION 2

SACTN_data <- read_csv("data/SACTN_data.csv")
view(SACTN_data)
head(SACTN_data)
tail(SACTN_data)
str(SACTN_data)
summary(SACTN_data)

SACTN_tidy <- SACTN_data %>%
  separate(col = index, into = c("site", "source"), sep = "/ ") %>%
  mutate(month = lubridate::month(date))
view(SACTN_tidy)
head(SACTN_tidy)
tail(SACTN_tidy)
str(SACTN_tidy)
summary(SACTN_tidy)

graph <- SACTN_tidy %>%
  group_by(month) %>%
  summarise(mean_temp = mean(temp),
            sd_temp = sd(temp)) 
graph

SACTN.summary2 <- summarySE(data = SACTN_tidy, measurevar = "temp", groupvars = c("month"))
ggplot(data = SACTN_tidy, aes(x = month, y = temp)) + #first step is always to plot data 
  geom_segment(data = SACTN.summary2, aes(x = month, xend = month, y = temp - ci, yend = temp + ci, colour = month),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = month), alpha = 0.6, show.legend = F) 
