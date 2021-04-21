#Assighnment_1
#Tyla Noelle Goldman 3837300
#20 April 2021

#Section 1
view(BOD)
#ANSWER= C 

#Section 2
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

data("murders")

#Exploring of data 
glimpse(murders)
head(murders)
tail(murders)
str(murders)
summary(murders)

#To place the data into context, it deals with ‘US gun murders by state for 2010’, where column headings: ‘state’ represent various states in the US- column ‘abb’ hold the abbreviations of the respective states, ‘region’ displays the geographical US region in which the respective state is found, while the latter two columns display the population sizes of states and the number of murders by gun in 2010. Internal structure assessment show that ‘state’ and ‘abb’ columns are filled with character values while ‘population’ and ‘total’ columns are filled with numerical data. The numerical data for population size and total gun murders are not normally distributed as their mean does not equal the median.  
#Overall, the data is made up of 51 observations of five variables, more simply 51 rows and 5 columns make up data. 
#The data is tidy as one observation per row/ one variable per column exists and no inappropriate spacing in column headings exist. Lastly, no standardization or missing data is seen.      

#murder_1 dataset displaying population size for various states.  
murder_1 <- murders %>%
  select(state, population)
view(murder_1)
str(murder_1)
head(murder_1)
tail(murder_1)
summary(murder_1)

#Removal of Florida from dataset. 
murder_2 <- murders %>%
  filter(state != "Florida")
view(murder_2)
str(murder_2)
head(murder_2)
tail(murder_2)
summary(murder_2)

#Removal of data from the south region- to form new data called 'no_south'. 
no_south <- murders %>%
  filter(region != "South")
view(no_south)
str(no_south)
head(no_south)
tail(no_south)
summary(no_south)

#Number of states in the south category. 
south <- murders %>%
  filter(region == "South")
view(south)
str(south)
head(south)
tail(south)
summary(south)
nrow(south) #command containing final answer. 
#FINAL ANSWER = 17 states in south category.

#Population size of south an dwest regionally. 
murders %>%
  select(region, population) %>%
  filter(region == "South") %>%
  summarise(population_size_south = sum(population))
#FINAL ANSWER = 115674434 south region population size.

murders %>%
  select(region, population) %>%
  filter(region == "West") %>%
  summarise(population_size_west = sum(population))
#FINAL ANSWER = 71945553 west region population size.   

#Dataset set containing population size of northeast region.
population_size_NE <- murders %>%
  select(region, population) %>%
  filter(region == "Northeast")
view(population_size_NE)
str(population_size_NE)
head(population_size_NE)
tail(population_size_NE)
summary(population_size_NE)

#Creating my own plots.
murder_regions <- murders %>%
  mutate(perc_pop_murder = ((total/population)*100)) %>%
  group_by(region) %>%
  summarise(mean_perc_pop_murder = mean(perc_pop_murder),
            sd_perc_pop_murder = sd(perc_pop_murder))

ggplot(data = murder_regions) +
  geom_col(aes(x = region, y = mean_perc_pop_murder), fill = "purple", col = "black") +
  geom_errorbar(aes(ymin = mean_perc_pop_murder - sd_perc_pop_murder,
                    ymax = mean_perc_pop_murder + sd_perc_pop_murder,
                    x = region), col = "black", width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "Murdered population(%)", title = "Fig1: Column graph showing percentage US population getting gun murdered per region in 2010")
              
murder_reg_safety <- murders %>%
  filter(region %in% c("South", "West"))
murder_state_name <- murder_reg_safety %>%
  mutate(label = if_else(state %in% c("California", "Texas"), "yes", "no"))

ggplot(data = murder_state_name, aes(x = population, y = total)) +
  geom_point(aes(col = region, size = region)) +
  geom_text_repel(aes(label = if_else(label == "yes", state, "")), col = "black", size = 3) +
  geom_smooth(method = "lm", aes(group = region, colour = region)) +
  labs(x = "Population size", y = "Number of murders", title = "Fig2:Number of murders VS population size for the states within safest and unsafest regions, South and West respectively")
#Used the two graphs to develop statistics on the safest and most dangerous state in the US to assist tourists. 
#According to figure 1: South is the most dangerous region of the US with the highest percentage population being murdered in 2010.
#West is the safest region of the Us with the lowest percentage population being murdered in 2010.
#Figure 2: In comparison of the number of murders to the population size for the safest and most dangerous region, graphical display shows direct proportion.
#The safest and more dangerous region both experience an increase in murder as the population size increases.
#where the most dangerous state in the south region is Texas therefore tourists to travel with caution and the safest state in the West region is California for tourists to travel. 
#Thus murder in a area is feulled by the population size which can relate to a higher population experiencing grreater competition for example for jobs ect.
#Therefore these areas dwell towards higher poverty which impacts acts of violence.
#This too means that over time the safe regions can become dangerous as population size increases 

#comparing population size of south and west region 

murders_compare <- murders %>%
  select(region, population) %>% 
  filter(region %in% c("South","West")) %>%
  group_by(region) %>%
  summarise(mean_population = mean(population))

ggplot(data = murders_compare, aes(x = region, y = mean_population, fill = region)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "Population size", title = "Comparitive column graph: population size of south vs west region") +
  theme(legend.position = "none")
  
#data frame of >20 but <100 and exclude value 120

murders_range <- murders %>%
  filter(total %in% (20:100)) %>%
  arrange(total) 
view(murders_range)
head(murders_range)
tail(murders_range)
str(murders_range)
summary(murders_range)

#slice data of row 10-24 and 26
murder_slice <- murders %>%
  slice(10:24, 26) 

#murders as tibble data 

murders_tibble <- as_tibble(murders)

#tibble murders data grouped by region 

murders_grouped <- murders %>%
  group_by(region)
tibble_murders_grouped <- as_tibble(murders_grouped)

#Section 3
data("heights")
glimpse(heights)
view(heights)
head(heights)
tail(heights)
str(heights)
summary(heights)

#The dataset 'heights'is made up of two columns named sex and height paired with 1, 050 rows of respective data. 
#The sex is made up of factorial data and the heights is made up of numerical data.
#From a summary of the data its seen that the mean and median of the heights is relatively close therefore, the data can be relatively normally distributed. 
#The data is tidy as one observation per row/ one variable per column exists and no inappropriate spacing in column headings exist. 
#Lastly, no standardization or missing data is seen.

heights %>%
  group_by(sex) %>%
  summarise(mean_height = mean(height), 
            sd_height = sd(height),
            median_height = median(height),
            min_heigh = min(height),
            max_height = max(height))

#Section 4
#A
x <- c(1, 6, 21, 19, NA, 73, NA) #FINAL ANSWER = 2
sum(is.na(x))
y <- c(NA, NA, 3, NA, 13, 24, NA)#FINAL ANSWER = 4
sum(is.na(y))

#B
personnel_function <- function(x){
  x <- (sum(is.na(x)))
  return(x)
  
}

personnel_function(x)
personnel_function(y)

#C
Bats <- c(10, 8, NA, 5, 7)
personnel_function(Bats) #FINAL ANSWER = 1 NA value 

Length <- c(0.5, 8, 4.9, NA, 7.6, NA, 5.3, 2, NA)
personnel_function(Length) #FINAL ANSWER = 3 NA values 

Flower_colour <- c("red", "blue", NA, "peach", NA, "yellow")
personnel_function(Flower_colour) #FINAL ANSWER = 2 NA values 

#Section 5

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))
seasonal_data_tidy <- Seasonal_data %>%
  gather(winter, spring, summer, Autumn, key = "seasons", value = "temperature")
view(seasonal_data_tidy)
str(seasonal_data_tidy)
head(seasonal_data_tidy)
tail(seasonal_data_tidy)
summary(seasonal_data_tidy)

#HYPOTHESIS = My hypothesis is that average temperature will be the highest in summer and lowest in winter, with reference to the phenomenon of seasons where summer is occurant when earths axis tilt is towards the sun and winter when earths axis tilt is away from the sun therefore, causing greater heat in summer and visaversa.     
temp_col <- seasonal_data_tidy %>%
  group_by(seasons) %>%
  summarise(mean_temp = mean(temperature),
            sd_temp = sd(temperature))
view(temp_col)
str(temp_col)
head(temp_col)
tail(temp_col)
summary(temp_col)

ggplot(data = temp_col, aes(x = seasons, y = mean_temp)) +
  geom_col(aes(fill = seasons, col = "black")) +
  geom_errorbar(aes(ymin = mean_temp - sd_temp,# the aesthetic associated with the error bars
                    ymax = mean_temp + sd_temp,
                    x = seasons),
                col = "black",
                width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Season", y = "Temperature(°C)", title = "Fig1: Column graph showing average temperature over varioous seasons from 2015-2018") +
  theme(legend.position = "none")

temp_col_2 <- seasonal_data_tidy %>%
  group_by(seasons)
  
view(temp_col_2)

ggplot(data = temp_col_2, aes(x = seasons, y = temperature, col = seasons, group = "year")) +
  geom_col(aes(fill = seasons, col = "black")) +
  facet_wrap(~year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Season", y = "Average temperature(°C)", title = "Fig2: Boxplot showing average temperature over varioous seasons per year for 2015-2018")
  
#According to figure 1,summer displays the highest average temperature and winter displays the lowest average temperature.    
#Standard error bars display the significant difference among the results where significant difference mean the results observed is not a byproduct of chance/ sampling error 
#The lack of overlap among summer and winter errorbar means significant difference exists. 
#this improves the reliability of the observation
#reliability is observed again through figure 2, where the temperature per season is analyzed per year.
#Figure 2 continues to display the tred of high summer temperatures an dlow winter temperatures for 2015,2017 and 2018
#The temperature is not the highest for summer in 2016, rather autumn held the highest average temperature. 
#Overall the two figures mainly support the trend of highest average temperatures to exist in summer and lowest average temperatures to exist in winter 
#IN CONCLUSION- the hypothesis is supported. 
cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))
cats_data

cats_data_tidy <- cats_data %>%
  separate(col = position, into = c("first_place", "second_place", "third_place"), sep = "-") %>% 
  unite(minutes, seconds, col = "total_time", sep = "-")
view(cats_data_tidy)

#Section 6 
data(package = .packages(all.available = TRUE))

data("storms") #built in data loaded - from package dplyr 

view(storms)
str(storms)
head(storms)
tail(storms)
summary(storms)

storms_gather <- storms %>% 
  gather(ts_diameter, hu_diameter, key = "wind types", value = "Diameter")
view(storms_gather) #Observed two columns both containing diameter.
                  #it is untidy to have the same measurement of data over more than one column- it promotes wide data 
                  #therefore the two columns were gathered 
                  #terms placed in inverted commas as they did not exist in the headings of previous data- they new terms 

#THERE WAS NO DATA TO SPREAD- THE DATA DID NOT APPEAR LONG 

#To illistrate the seperate function, I will unite the date(this can possibly stand in for the impossibility of spreading the data in terms of marks allocation) 
#Then I will seperate the data 

storms_unite <- storms %>%
  unite(year, month, day, col = "date", sep = "-") #common practice to unite the date together as it is more proffessional and reduces the widness of the dataset 
view(storms_unite)                                #the term date was placed in inverted commas as it is not a column heading existing in data 
#Then to separate back to original data
storms_seperate <- storms_unite %>%
  separate(col = date, into = c("year", "month", "day"), sep = "-")
view(storms_seperate) #in this case; year, month and day are considered new headings as they were eliminated when data was united. 

storms_edited_1 <- storms_unite %>% #used the storms_unite as it contains a tider for of date as date is represented in one discrete column
  select(name, date, pressure) %>% #extracted particular desired columns from dataset
  group_by(name) %>% #ensures that all the data for a particular storm is located together 
  mutate(pressure_perc = pressure*100) %>% #developing a new column in which pressure data is displayed as a percentage 
  arrange(pressure_perc)
view(storms_edited_1)  

storms_edited_2 <- storms_unite %>%
  select(name, date, wind) %>%
  group_by(name) %>% 
  mutate(wind_perc = wind*100) %>% #developing a new column in which wind data is displayed as a percentage
  arrange(wind_perc)
view(storms_edited_2)

#grouping of data, although applied, did not serve any relevance for this particular data as the main sectors of data were already well grouped 

FINAL <- left_join(storms_edited_1, storms_edited_2) #joined two datasets that held important information regarding wind and pressure of particular storms
view(FINAL)


