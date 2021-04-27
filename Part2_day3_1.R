#Part2_Day3.1
#Tyla Noelle Goldman 
#LINEAR REGRESSION 

#reading in the required packages: 
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
library(ggthemes)
library(rcompanion)

#do linear regression when attempting to find the impact of one variable on another 
#linear models = show you that code exists 

#Regressions test the statistical significance of the dependence of 
#one continuous variable on one or many independent continuous variables.

#Y = dependent variable / response variable 
#X = independent variable / predictor 
#Regression parameters- determined by minimizing the error sum of squares of error term:
#alpha = incept term (where fitted line starts and intercepts with y-axis)
#Beta = slope

#residual variation = amount of variation not explained by linear relationship of y on x  

#the above explained factors make up the linear regression model - can be seen bellow 

head(faithful) #reading in the data 

view(faithful)
str(faithful)
tail(faithful)
summary(faithful)

#linear regression model: 
eruption.lm <- lm(eruptions ~ waiting, data = faithful)     #MOST IMPORTANT PIECE
summary(eruption.lm) #getting a summary of the linear regression - think of it like planning a graph 
#intercept very important from this summary and see the standard error of the estimate
#with this layout of linear regression coded - its makes similar to a "dataset" 
#so from here various parameters/ information that can be gathered from a line graph can be questioned

#Making the actual linear regression graph: 

slope <- round(eruption.lm$coef[2], 3) #eruption.lm is the regression model we generated 
#p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) + #specifying the data of which the line graph is made and its respective x & y axis 
  geom_point(shape = "plus",outlier.colour = "red", outlier.shape = 10,
             outlier.size = 4, col = "red", alpha = 0.8) + #changed point shapes, outliers edited 
  theme_tufte() + #changed theme 
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) + #placing varius text at particular locations on the plot
  stat_smooth(method = "lm", colour = "purple4", size =1, linetype = "longdash") + #developing the actual line of regression with different linetype 
  ggtitle(~underline("Figure 1: Old Faithful eruption data" )) + #the more biologically figure correct format with underline 
  theme(plot.title = element_text(size = 20, face = "bold")) +
  labs(subtitle = expression(italic("Linear regression")), caption = "Graph generated: 21/04/2021", #caption added 
       x =expression(italic("Waiting time (minutes)")),
       y = expression(italic("Eruption duration (minutes)"))) #changed font to italic 
# use the accessor function to grab the coefficients:
erupt.coef <- coefficients(eruption.lm)
erupt.coef

# how long would an eruption last of we waited, say, 80 minutes?
waiting <- 80 
# the first and second coef. can be accessed using the 
# square bracket notation:
erupt.pred <- erupt.coef[1] + (erupt.coef[2] * waiting)
erupt.pred # the unit is minutes

#The prediction is that, given a waiting time of 80 minutes since the previous eruption, the next eruption will last 4.176 minutes.

#another way of doing it .
pred.val <- data.frame(waiting = c(60, 80, 100))
predict(eruption.lm, pred.val) # returns waiting time in minutes

#the coefficient of determination 
summary(eruption.lm)$r.squared

#gives varence of y 
#how well the observed outcome variable is predicted by the observed influential variable
#0 = no relation 
#1 = perfect fit - point also lie on straight line 

library(tidyverse)
n <- 100
set.seed(666)
rand.df <- data.frame(x = seq(1:n),
                      y = rnorm(n = n, mean = 20, sd = 3))
ggplot(data = rand.df, aes(x = x, y = y)) +
  geom_point(colour = "red", shape = "plus") + #changed point colour and shape 
  stat_smooth(method = "lm", colour = "purple4", size = 0.75, fill = "seashell3",
              alpha = 0.3, linetype = "longdash") + #changed line type and colour 
  ggtitle(~underline("Figure 2: Random normal data" )) + #the more biologically correct figure title format 
  theme(plot.title = element_text(size = 20, face = "bold")) + #bolded title 
  labs(subtitle =expression(italic("Linear regression")),
       x =expression(italic("X (independent variable)")),
       y =expression(italic("Y (dependent variable)"))) + #changed font 
  theme_tufte() #changed theme 

#can dispay the two graphs on one sheet with the below formula of ggarrange. 
#label graphs accordingly 

#grid <- ggarrange(A, B,
#                 ncol = 1, nrow = 2,
#                  labels = c("A", "B"),
#                  common.legend = TRUE)
#grid
