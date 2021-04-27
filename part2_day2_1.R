#Biostatistics_part2_day2
#Tyla Noelle Goldman 3837300
#T - TESTS 

#loading of all the relevant packages: 
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
library(plotly) #contains the t-test 
library(reshape)
library(ggthemes)
library(rcompanion)

#species abundance over different sites
#use t-tests or ANOVA test 
#two-sample t-test most commonly used 
#p value greater than 0.05= normal data- bell shape in histogram also displays normality - checks assumption for t-test
#when p vale is less than 0.05 = is a significant difference among samples(for t-tests and ANOVA)
#if a t-test/ anova not possible as it does not meet assumptons then standardize data  
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))
view(r_dat)
str(r_dat)
head(r_dat)
tail(r_dat)
summary(r_dat)

# Create histogram - first step is to plot data to get a quick idea of what the data describes 
graph <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  scale_fill_manual(values = c("lightcoral", "lemonchiffon4")) + #changed colours 
  ggtitle(~underline("Histogram showing r.dat data" )) + #underline allows for a heading that follows the rules of biological graphs 
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) + 
  labs(x = "value", caption = "Graph generated: 20/04/2021", fill = "Sample") + #alpha helps us see the histogram past the shadows 
  theme_bw() +  #cation added, capitilized legend heading  and theme changed 
  geom_label(aes(x = 12, y = 190, label = "Bell shape represents \n normally distributed data"), fill = "lightgoldenrod")
graph  #labeling added 
#histograms is a good way to check the normality of the data 
#bell shape displays normal data 

#NOTE TO SELF: in exams you will get a piece of code- best way to understand is to run it line by line

#want to look into differences among the populations of the dataset = t-test to be performed 
#if its multiple variables= perform ANOVA- the sameas t-test just more variabbles 

#all these test must first meet assumptions before they can be applied
#THE ASSUMPTIONS:
#the dependent variable must be continuous (i.e. it is measured at the interval or ratio level),
#the observations in the groups being compared are independent of each other,
#the data are normally distributed, and
#that the data are homoscedastic, and in particular, that there are no outliers.

#test for NORMALITY:   -SHAPIRO-WILK NORMALITY TEST
shapiro.test(r_dat$dat) #calculated the normality of the whole dataset 
#need to check the normality of all the dataset parts 
#only interested in p-vale 
#p-value greater than o.o5 is normal data 

# NB NB NB IMPORTANT NORMALITY TEST SETUP !!!   #MIGHT AS WELL GO DOWN TO TWO-FOR-ONE
#                                               NORMALITY AND HOMOSCEDICITY CALCULATED IN ONE CODE**
r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2])) #now test is applied to the two different samples 
#only look at p value <- normal data seen because value greater than 0.05 for both sample sets

#HOMOSCEDACITY- comparing variance between samples are similar 
#             -like the similarity= therefore a big output from this test will represent significant difference among variable 
#             -this is NOT DESIRED 
#             -THEREFORE--- VARIENCE AMONG SAMPLES SHOULD NOT BE MORE THAN FOUR TIMES GREATER THAN ONE ANOTHER 
#             -AIM LESS THAN 4 FOR HOMOSCEDACITY PRESENT   


r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))
#once all assumptions are approved- the t-test can be done 
#if data proves NOT normal then t-test cant be done 

#TWO FOR ONE - check for normality and homoscedicy at the same time 
two_assum <- function(x) { #generate own function 
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}


r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])

#ALL ASSUMPTIONS MET - NOW TO DO T-TESTS 
#ONE SAMPLE T-TEST = comparing one sample to the mean

# create a single sample of random normal data
#set.seed(666)
#r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# check normality
#shapiro.test(r_one$dat) # p value greater than 0.05 therefore normal data 

# compare random data against a population mean of 20
#t.test(r_one$dat, mu = 20) #comparing sample to the mean therefore provide the mean

# compare random data against a population mean of 30
#t.test(r_one$dat, mu = 30)

# ggplot(data = r_one, aes(y = dat, x = sample)) +
#  geom_boxplot(fill = "lightsalmon") +
  # population  mean (mu) = 20
#  geom_hline(yintercept = 20, colour = "blue", 
#             size = 1, linetype = "dashed") +
  # population  mean (mu) = 30
#  geom_hline(yintercept = 30, colour = "red", 
#             size = 1, linetype = "dashed") +
#  labs(y = "Value", x = NULL) +
#  coord_flip()

#MORE OFFICIAL  MORE OFFICIAL  MORE OFFICIAL  MORE OFFICIAL
#ONE SIDED ONE SAMPLE T-TEST
t.test(r_one$dat, mu = 30, alternative = "less")  #trailing tail

t.test(r_one$dat, mu = 30, alternative = "greater") #leading tail



#two-sample t-tests: compare two pieces of data 

# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!) - due to homoscedicity 
t.test(dat ~ sample, data = r_two, var.equal = TRUE)
#tilda sighn has to do with how the data changes at each sample 
#p value greater than 0.05 therefore a significant differenvce exists 





#VISUALIZING DATA - RUNNING A WORK FLOW
ecklonia <- read_csv("data/ecklonia.csv") %>% #loading in of data 
  gather(key = "variable", value = "value", -species, -site, -ID)

view(ecklonia)
str(ecklonia)
head(ecklonia)
tail(ecklonia)
summary(ecklonia)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) + #first step is always to plot data 
  geom_boxplot(outlier.colour = "burlywood4", outlier.shape = "square", outlier.size = 2) + #outliers of graph edited 
  coord_flip() +
  theme_economist() + #changed the theme 
  scale_fill_economist() + #gave the plot more appropriate colours for the theme 
  scale_color_economist() +
  labs(title = "Boxplot showing variables for Batsata Rock and Bouldres beach", caption = "Graph generated: 20/04/2021",
       fill = "SITE", x = "", 
       subtitle = "1)Graph used to make observations of data \n 2)Form hypothesis") + #added caption and subtitle containing graph purposes(displayed over two lines using '\n')
  theme(plot.title = element_text( hjust = 0)) + #repositioned title 
  theme(legend.position = "bottom") #moved the legend 
#can see batasta rock shows greater measurements than boulders beach 
#remember we doing this process to see differences in variables, therefore boxplot shows stipe mass may be similar 
#desiered to do t-test to see if the difference exist THEREFORE HYPOTHESIS 
#Ho is the hypothesis stating no difference 
#H1 is hypothesis stating difference

# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass") #making a new plot only of the variable under question
ecklonia_sub
view(ecklonia_sub)
str(ecklonia_sub)
head(ecklonia_sub)
tail(ecklonia_sub)
summary(ecklonia_sub)

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot(outlier.colour = "burlywood4", outlier.shape = "square", outlier.size = 3) + #outliers of graph edited 
  coord_flip() +
  ggtitle("Boxplot showing stipe mass for Batsata Rock and Boulders Beach") +
  labs(y = "stipe mass (kg)", x = "", caption = "Graph generated: 20/04/2021", fill = "SITE") + #add caption 
  theme_classic() + #capitalized legend heading as it was looking lost 
  theme(legend.position = "bottom") + #moved legend
  theme(legend.title = element_text(color = "mediumvioletred", size = 10), #changed legend text colours 
        legend.text = element_text(color = "black", face = "bold")) +
  theme(legend.background = element_rect(fill = "darkgray")) + #legend background 
  scale_fill_manual(values = c("gray82", "gray27")) + #changed box colours
  theme(plot.title = element_text(colour = "mediumvioletred")) #coloured heading 


ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2]) #check assumptions before you can do statistical t-test for differences 
# traditional output                              #ASSUMPTIONS CONFIRMED THEREFORE T_TEST 

#two-sided two sample t-test
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater") #the t-test
#p-vale greater than 0.05 therefore less significant difference among variables 

compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")
#compare is another method of doing t-test however the output is not as information strong 

#ANOVA
#one factor anova= comparing one variable 
#two factor anova = comparing more than one variable 

chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")
 #when you have multiple variabes you can run multiple t-tests but a type1 error occur- this decreases relaibility of results 
#therefore ANOVA allows for multiple variables to be compared 

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)

TukeyHSD(chicks.aov1)

