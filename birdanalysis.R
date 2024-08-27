# Assuming the dataset is stored in a variable called mscdissdata_birddata_
#Basic plots

boxplot(richness ~ meander_type, data = mscdissdata_birddata_,
        xlab = "Meander Type", ylab = "Richness",
        main = "Boxplot of Richness by Meander Type")

ggplot(mscdissdata_birddata_, aes(x = meander_type, y = richness)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.6) +
  xlab("Meander Type") + ylab("Richness") +
  ggtitle("Violin Plot of Richness by Meander Type") +
  facet_wrap(~ site_name, scales = "free", ncol = 2)

#Basic model and null model
birdmodel <- glmer(richness ~ meander_type + (1 | site_name), data = mscdissdata_birddata_, family = poisson(link = "log"))
birdmodel
birdmodelnull <- glmer(richness ~ 1 + (1 | site_name), data = mscdissdata_birddata_, family = poisson(link = "log"))
anova(birdmodel, birdmodelnull)

#Testing residuals
qqnorm(resid(birdmodel))
qqline(resid(birdmodel))

#Adding further variables
birdmodel2 <- glmer(richness ~ meander_type + presence_forest + (1 | site_name), data = mscdissdata_birddata_, family = poisson(link = "log"))
birdmodel2
anova(birdmodel2, birdmodel)

birdmodel2.5 <- glmer(richness ~ meander_type + tree + (1 | site_name), data = mscdissdata_birddata_, family = poisson(link = "log"))
birdmodel2.5
anova(birdmodel2.5, birdmodel)

birdmodel3 <- glmer(richness ~ meander_type + presence_wetland + (1 | site_name), data = mscdissdata_birddata_, family = poisson(link = "log"))
birdmodel3
anova(birdmodel3, birdmodel)

birdmodel4 <- glmer(richness ~ meander_type + presence_agriculture + (1 | site_name), data = mscdissdata_birddata_, family = poisson(link = "log"))
birdmodel4
anova(birdmodel4, birdmodel)


    