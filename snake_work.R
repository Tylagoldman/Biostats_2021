#Snakes exersice 

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

snakes <- read_csv("data/snakes.csv") 
snakes$day = as.factor(snakes$day) #changing the day column data to a factor as anova is applied to independent variables, not the current continous data  
                                  #generate factorial data 
snakes.summary <- snakes %>% 
  group_by(day, snake) %>% #summary does not work when data is grouped 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup() #ungroupig of the grouped data- easily diplay of summary 
snakes.summary

#my speculation for this error is due to the grouping of day and snake causing a form of untidy data where too mnay variables are existing for one observation 

snakes.summary <- snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

library(Rmisc)
snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) + #geom segment allows you to edit how information is displayed per segment of the boxplot
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + #coding for a boxplot-type of graph
  geom_jitter(width = 0.05) #spreads out compacted data so that boxplot is more pleasantly displayed 

snakes.aov <- aov(openings ~ day + snake, data = snakes) # ~ shows that you analysing the varience of the openings using day and snake 
summary(snakes.aov) #anova of data 

par(mfrow = c(2, 2))
# Checking assumptions...
# make a histogram of the residuals;
# they must be normal
snakes.res <- residuals(snakes.aov)
hist(snakes.res, col = "red")

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic
plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")

snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")

#MY OWN GRAPH 
view(snakes)
snake_graph <- snakes %>%
  group_by(snake)
view(snake_graph)

ggplot(data = snake_graph, aes(x = snake, y = openings, fill = snake)) +
  geom_col() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = expression(italic("Snake")), y = expression(italic("Openings")), title = expression(italic("Column graph showing the number of openings for each snake")))

snakes_bouji <- snakes %>%
  group_by(day, snake)

ggplot(data = snakes_bouji, aes(x = day, y = openings, fill = snake)) +
  geom_col(position = "dodge", col = "black") +
  scale_fill_brewer(palette = "Greens") +
  labs(x = "Day", y = "Openings", fill = "Snake",
       title = "Graph showing the openings per day of different snakes in study") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"))

