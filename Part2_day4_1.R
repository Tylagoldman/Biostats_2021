# Biostatistics day 4 
#Tyla Noelle Goldman 3837300
#CONFIDENCE INTERVALS 

#range of mean values that you can arrive at if yu for example have repeated experiment 100 times 
#confidence intervals have an upper and lower bound 
#95 out of 100 times, estimates will fall in range of means 
#repeatung experiment- 5 out of 100 times, data will fall in range of mean
#p value less than 0.05

#bootstraps- repeats experiment with data placed into the program and then calculate the confidence intervals - the range of means 

library(tidyverse) #loading of various needed packages 
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

Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)
str(data)

library(rcompanion)
# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

# one-way data
CI_1 <- groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

#Find a way to plot this data with a structure that dislays confidence interval 
#MY THOUGHTS - standard error bars tend to show limits therefore a similar plot regquired 
#needs to be a column graph or box plot as it accomodates standard error bar type fuction 

#ITS NOT A STANDARD ERROR BAR - ITS A CONFIDENCE INTERVAL BAR AS AJ SAID MULTIPLE TIMES 

#data_edited <- data %>% 
  #group_by(Sex) %>%
  #summarise(mean_steps = mean(Steps),
#            sd_steps = sd(Steps))
#data_edited  

ggplot(data = CI_1, aes(x = Sex, y = Mean, fill = Sex)) + #mean used as this graph shows confidence intervals which is determined around a mean range 
  geom_col(position = "dodge", col = "black") +
  scale_fill_manual(values = c("azure3", "azure4")) + #professional colours that still dislay different columns 
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex), position = position_dodge(0.45), width = 0.2) +
  labs(x = "Sex", y = "Steps", subtitle = "Confidence intervals of data displayed", caption = "Graph generated on 22/04/2021") +
  theme(plot.caption = element_text(hjust = 0)) + #caption was repositioned 
  ggtitle(~underline("Figure 1: Bar graph showing steps per sex" )) + #underline allows for a heading that follows the rules of biological graphs 
  theme(plot.title = element_text(face = "bold", colour = "red"),
        plot.subtitle = element_text(color = "blue")) +
  theme(legend.position = "remove") #removed legend for this code as column names are clearly displayed 
  

# two-way data
CI <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

ggplot(data = CI, aes(x = Sex, y = Mean, fill = Sex)) +
  geom_col(position = "dodge", col = "black") +
  scale_fill_manual(values = c("darkred", "mediumblue")) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex), position = position_dodge(0.45), width = 0.2) +
  facet_wrap(~Teacher, ncol = 3) +
  labs(x = "Sex", y = "Steps", subtitle = "Confidence intervals of data is diaplayed", #subtitle and caption added
       caption = "Graph generated - 22/04/2021") +
  theme(plot.caption = element_text(hjust = 0)) + #caption was repositioned 
  ggtitle(~underline("Figure 1: Bar graph showing sex vs steps vs teacher " )) + #underline allows for a heading that follows the rules of biological graphs 
  theme(plot.title = element_text(face = "bold")) 
#left legend in this graph as alot is shown in graph so easy differentiation between male and female is helpful 
                                              

  # by bootstrapping
  groupwiseMean(Steps ~ Sex,
                data = data,
                conf = 0.95,
                digits = 3,
                R = 10000,
                boot = TRUE,
                traditional = FALSE,
                normal = FALSE,
                basic = FALSE,
                percentile = FALSE,
                bca = TRUE)
anova  = aov(Steps ~Sex*Teacher, data = data) #multiple variables therefore doing anova  
summary(anova)

anova_Tukey <- TukeyHSD(anova)
mat <- plot(anova_Tukey, col = "cyan3") 
  
#any confidence interval bar much greater than 0 then there is significant difference 
