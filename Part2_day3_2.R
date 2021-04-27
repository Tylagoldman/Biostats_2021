# Tyla Noelle Goldman 3837300
#CORRELATION 

#Loading of all the required packages: 
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

#correlation describes the relationship among variables from the same sample-
#level of similarity among data of the variables 

ecklonia <- read_csv("data/ecklonia.csv")
view(ecklonia)
str(ecklonia)
head(ecklonia)
tail(ecklonia)
summary(ecklonia)

ecklonia_sub <-  ecklonia%>%
  select(-species, - site, - ID) #only have ordinal/ continuous data for correlation 
ecklonia_sub  #like all the dependent variables are leftover 
view(ecklonia_sub)
str(ecklonia_sub)
head(ecklonia_sub)
tail(ecklonia_sub)
summary(ecklonia_sub)

ecklonia_pearson <- cor(ecklonia_sub) #this makes diagrams like the heat map 
ecklonia_pearson #correlation of all the variable from data where non ordinal/continuous data have been removed
view(ecklonia_pearson)
str(ecklonia_pearson)
head(ecklonia_pearson)
tail(ecklonia_pearson)
summary(ecklonia_pearson)

#How can one check if there is un normal data in their dataset: 
ecklonia_norm <- ecklonia_sub %>% #used to check for non normal data 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

#DOIND CORRELATION AMONG SPECIFIC VARIABLES - one variable correlated with another

#PEARSON CORRELATION- continuous data 
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length, #correlation only used for continuous data 
         use = "everything", method = "pearson") # look at correlation, greater than 0.05- more correlated as ultimate correlation is at 1
#the correlation is nearing 1 therefore high correlation exist among stipe length and frond length - strong relationship between the variables   

#SPEARMAN RANK CORRELATION - for ordinal data 

# Create ordinal data for variables you want to compare 
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cat <- cor.test(ecklonia$length, ecklonia$digits) #cat is just a name given so that i can view the final product 
cat
#low correlation therefore a stog relationship does not exist among these variables 

#KENDALL RANK CORRELATION- works on ordinal and continuous data and works for normal and not normal data  
ecklonia_norm <- ecklonia_sub %>% #used to check for non normal data 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

box <- cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")
box

#ONE PANEL VISUAL 

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2)) #calculating the correlation 
                                                                                  #'round' allows for values to be rounded off to two decimal places 

library(ggthemes)

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "springgreen", se = F) + #' se' is for the confidence interval around smooth 
  geom_point(colour = "olivedrab1", shape = "triangle", size = 2) + #used very bright colors to aid graph visualization above the dark background 
  geom_label(x = 300, y = 240, label = r_print) + #changed point shape and size 
  labs(x = "Stipe length (cm)", y = "Frond length (cm)",
       subtitle = "One panel visual of correlaton displayed through Pearson method",
       caption = "Graph generated: 21/04/2021") + #added subtitle and caption 
  ggtitle(~underline("Figure 1: Line graph showing fond length VS Stipe lenghth" )) + #underline allows for a heading that follows the rules of biological graphs 
  theme(plot.title = element_text(face = "bold")) + #bolded title 
  theme(plot.title = element_text(lineheight = 0.6)) + #allowing title to be visable on sheet 
  theme_dark() #changed theme to a dark background


  
#MULTIPLE PANEL VISUAL

corrplot(ecklonia_pearson, method = "square") #changed shape 

view(ecklonia_pearson)

#HEATMAP EXERSICE 

#Method 1
heatmap(ecklonia_pearson) #used the built in heatmap function of r 
heatmap(ecklonia_pearson, Rowv =NA, Colv = NA, col = Change_col(100)) #removed lines and added new colors through personel palette 

Change_col <- colorRampPalette(c("cyan", "deeppink3")) #developing my own colour palette  


#method 2
ecklonia_melt <- melt(ecklonia_pearson)
view(ecklonia_melt)
ggplot_ecklonia <- ggplot(ecklonia_melt, aes(X1, X2)) +
  geom_tile(aes(fill = value))
ggplot_ecklonia #sometimes view functions dont work 

ggplot_ecklonia + scale_fill_gradient(low = "blue", high = "salmon") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(~underline("DISPLAY OF INTERACTIONS AMONG VARIABLES OF ECKLONIA DATA")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(x = "", y = "", 
       fill = "Similarity magnitude") #changed the legend heading 

library(dlply)
library(plyr)
