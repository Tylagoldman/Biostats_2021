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

#PEARSON CORRELATION 

ecklonia <- read_csv("data/ecklonia.csv")
ecklonia_sub <-  ecklonia%>%
  select(-species, - site, - ID)

 
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length, #correlation only used for continuous data 
         use = "everything", method = "pearson") # look at correlation, greater than o.o5- more corrreclated as ultimate correlation is at 1

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

#SPEARMAN RANK CORRELATION - for ordinal data 

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)

#Kendal rank correlation- works on ordinal an dcontinuous data and works for normal and not data  

ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

#ONE PANEL VISUAL 

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

#MULTIPLE PANEL VISUAL

corrplot(ecklonia_pearson, method = "circle")

view(ecklonia_pearson)

#HEATMAP EXERSICE 

#Method 1
heatmap(ecklonia_pearson) 
heatmap(ecklonia_pearson, Rowv =NA, Colv = NA) 

#method 2
ecklonia_melt <- melt(ecklonia_pearson)
view(ecklonia_melt)
ggplot_ecklonia <- ggplot(ecklonia_melt, aes(X1, X2)) +
  geom_tile(aes(fill = value))
ggplot_ecklonia #sometimes view functions dont work 

ggplot_ecklonia + scale_fill_gradient(low = "blue", high = "salmon") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "", title = "Graph displaying interactions among variables of ecklonia data")

library(dlply)
library(plyr)
