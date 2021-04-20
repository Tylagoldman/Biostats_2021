#Biostatistics_part2_day2
#Tyla Goldman 3837300

library(tidyverse)
library(plotly) #contains the t-test function 
library(lubridate)
library(ggplot2)
library(ggpubr)
library(scales)
library(ggsn)
library(maps)
library(ggrepel)
library(readr)
#species abundance over different sites
#use t-tests or ANOVA test 
#two-sample t-test most commonly used 
#when p vale is less than 0.05 = is a significant difference(for t-tests and ANOVA)
#p value greater than 0.05= normal data- bell shape in histogram also displays normality 

set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Create histogram - first step is to plot data to get a quick idea of what the data describes 
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) + #fill by sample is making the differnt colors 
  labs(x = "value") #alpha helps us see the histogram past the shadows 
h
#histograms is a good way to check the normality of the data 
#bell shape displays normal data 

#NOTE TO SELF: in exams you will get a piece of code- best way to understand is to run it line by line

#test for normality 
shapiro.test(r_dat$dat) #calculated the normality of the whole dataset 
#need to check the normality of all the dataset parts 

r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2])) #now test is applied to the two different samples 
#only look at p value <- normal data seen because value greater than 0.05
#HOMOSCEDACITY 
#whether the varience between samles are similar 
# when varience is not 4 times more than each other 
r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))
#once all assumptions are approved- the t-test can be done 
#if data proves NOT normal then t-test cant be done 

#TWO FOR ONE 
two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])

#comparing one sample to the mean= one-sample t- test
# create a single sample of random normal data
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# check normality
shapiro.test(r_one$dat) # p value greater than 0.05 therefore normal data 

# compare random data against a population mean of 20
t.test(r_one$dat, mu = 20) #comparing sample to the mean therefore provide the mean 

ggplot(data = r_one, aes(y = dat, x = sample)) +
  geom_boxplot(fill = "lightsalmon") +
  # population  mean (mu) = 20
  geom_hline(yintercept = 20, colour = "blue", 
             size = 1, linetype = "dashed") +
  # population  mean (mu) = 30
  geom_hline(yintercept = 30, colour = "red", 
             size = 1, linetype = "dashed") +
  labs(y = "Value", x = NULL) +
  coord_flip()

#two-sample t-tests: compare two pieces of data 

# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)
t.test(dat ~ sample, data = r_two, var.equal = TRUE)
#tilda sighn has to do with how the data changes at each sample 
#p value greater than 0.05 therefore a signifucant differenvce exists 

#VISUALIZING DATA 
ecklonia <- read_csv("data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)
view(ecklonia)
ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplot showing a variety of variables fir Batsata Rock and Bouldres beach")

# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])
# traditional output
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")

#ANOVA
#one factor anova= comparing one variable 
#two factor anova = comaring more than one variable 

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

