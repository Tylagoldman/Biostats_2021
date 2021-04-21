#Part2_Day3.1

#do linear regression when attempting to find tbe impact of one variable on another 
#linear models = show you that code exists 

head(faithful)

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) + #specifying the data of which the line graph is made and its respective x & y axis 
  geom_point(outlier.colour = "red", outlier.shape = 10,
             outlier.size = 4, col = "red", alpha = 0.8) +
  theme_bw() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) + #placing varius text at particular locations on the plot
  stat_smooth(method = "lm", colour = "yellow", size =1) + #developing the actual line of regression
  labs(title = expression(italic("Old Faithful eruption data")),
       subtitle = expression(italic("Linear regression")),
       x =expression(italic("Waiting time (minutes)")),
       y = expression(italic("Eruption duration (minutes)"))) #labels of plot
# use the accessor function to grab the coefficients:
erupt.coef <- coefficients(eruption.lm)
erupt.coef

# how long would an eruption last of we waited, say, 80 minutes?
waiting <- 80 
