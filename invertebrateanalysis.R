#Packages used in this analysis
library(lme4)
library(nlme)
library(ggplot2)



#I start with some basic descriptive statistics of my dataset
mean(mscdissdata_invertdata_$shannonsdiversity[mscdissdata_invertdata_$meander_type == "remeandered"])
sd(mscdissdata_invertdata_$shannonsdiversity[mscdissdata_invertdata_$meander_type == "remeandered"])
mean(mscdissdata_invertdata_$shannonsdiversity[mscdissdata_invertdata_$meander_type == "straight"])
sd(mscdissdata_invertdata_$shannonsdiversity[mscdissdata_invertdata_$meander_type == "straight"])

mean(mscdissdata_birddata_$richness[mscdissdata_birddata_$meander_type == "remeandered"])
sd(mscdissdata_birddata_$richness[mscdissdata_birddata_$meander_type == "remeandered"])
mean(mscdissdata_birddata_$richness[mscdissdata_birddata_$meander_type == "straight"])
sd(mscdissdata_birddata_$richness[mscdissdata_birddata_$meander_type == "straight"])

#Here I change the reference level for my data set to remeandered, which is more useful than the default natural.
mscdissdata_invertdata_$meander_type <- factor(mscdissdata_invertdata_$meander_type)
mscdissdata_invertdata_$meander_type <- relevel(mscdissdata_invertdata_$meander_type, ref = "remeandered")

#Here I set up my initial models, for predictive analysis
model <- lmer(shannonsdiversity ~ meander_type + (1 | site_name), data = mscdissdata_invertdata_)
summary(model)
null_model <- lmer(shannonsdiversity ~ 1 + (1 | site_name), data = mscdissdata_invertdata_)
model2 <- lmer(shannonsdiversity ~ meander_type + substrate + (1 | site_name), data = mscdissdata_invertdata_)
summary(model2)
model2.5 <- lmer(shannonsdiversity ~ meander_type + tree + (1 | site_name), data = mscdissdata_invertdata_)
summary(model2.5)
model3 <- lmer(shannonsdiversity ~ meander_type + substrate + tree + (1 | site_name), data = mscdissdata_invertdata_)
summary(model3)
model4 <- lmer(shannonsdiversity ~ meander_type + presence_forest + (1 | site_name), data = mscdissdata_invertdata_)
summary(model4)
model5 <- lmer(shannonsdiversity ~ meander_type + presence_wetland + (1 | site_name), data = mscdissdata_invertdata_)
summary(model5)
model6 <- lmer(shannonsdiversity ~ meander_type + presence_agriculture + (1 | site_name), data = mscdissdata_invertdata_)
summary(model6)
modelx <- lmer(shannonsdiversity ~ meander_type + substrate + tree + presence_wetland + (1 | site_name), data = mscdissdata_invertdata_)
summary(modelx) #This model includes all significant variables in one model. The null model below removes meander type.
modelxnull <- lmer(shannonsdiversity ~ substrate + tree + presence_wetland + (1 | site_name), data = mscdissdata_invertdata_)


#Here I set up NLME models, which gives further information about categorical values with multiple levels
modellme <- lme(shannonsdiversity ~ meander_type, random =~1 | site_name, data = mscdissdata_invertdata_)
summary(modellme)
model2lme <- lme(shannonsdiversity ~ meander_type + substrate, random =~1 | site_name, data = mscdissdata_invertdata_)
summary(model2lme)

#Here I complete Q-Q plots to test the fit of my first model, and the model including all variables
qqnorm(resid(model))
qqline(resid(model))
qqnorm(resid(modelx))
qqline(resid(modelx))

#Here I compare models, to see which added variables improve the model
anova(model, null_model)
anova(model2, model)
anova(model2.5, model)
anova(model4, model)
anova(modelx, modelxnull)
anova(model5, model)
anova(model6, model)

#Here I plot
plot <- ggplot(mscdissdata_invertdata_, aes(x = meander_type, y = shannonsdiversity)) +
  geom_boxplot() +
  facet_wrap(~ site_name, ncol = 3) + 
  labs(x = "Meander Type", y = "Shannon's Diversity") +
  ggtitle("Shannon's Diversity by Meander Type, Faceted by Site Name") +
  theme_minimal()
plot







##### EPT analysis

plotept <- ggplot(mscdissdata_invertdata_, aes(x = meander_type, y = EPT)) +
  geom_boxplot() +
  facet_wrap(~ site_name, ncol = 3) + 
  labs(x = "Meander Type", y = "EPT%") +
  ggtitle("EPT% by Meander Type, Faceted by Site Name") +
  theme_minimal()
plotept

modelept <- glmer(EPT ~ meander_type + (1 | site_name), data = mscdissdata_invertdata_, family = binomial(link = "logit"))
modelept
summary(modelept)
modeleptnull <- glmer(EPT ~ 1 + (1 | site_name), data = mscdissdata_invertdata_, family = binomial(link = "logit"))
anova(modelept, modeleptnull)

mean(mscdissdata_invertdata_$EPT[mscdissdata_invertdata_$meander_type == "remeandered"])
mean(mscdissdata_invertdata_$EPT[mscdissdata_invertdata_$meander_type == "straight"])
mean(mscdissdata_invertdata_$EPT[mscdissdata_invertdata_$meander_type == "natural"])

qqnorm(resid(modelept))
qqline(resid(modelept))




##### Evenness analysis
plotevenness <- ggplot(mscdissdata_invertdata_, aes(x = meander_type, y = evenness)) +
  geom_boxplot() +
  facet_wrap(~ site_name, ncol = 3) + 
  labs(x = "Meander Type", y = "Species Evenness") +
  ggtitle("Evenness by Meander Type, Faceted by Site Name") +
  theme_minimal()

plotevenness

modelevenness <- glmer(evenness ~ meander_type + (1 | site_name), data = mscdissdata_invertdata_, family = binomial(link = "logit"))
summary(modelevenness)
modelevenness
modelevennessnull <- glmer(evenness ~ 1 + (1 | site_name), data = mscdissdata_invertdata_, family = binomial(link = "logit"))
anova(modelevenness, modelevennessnull)

qqnorm(resid(modelevenness))
qqline(resid(modelevenness))






##### Richness analysis
plotrichness <- ggplot(mscdissdata_invertdata_, aes(x = meander_type, y = speciesrichness)) +
  geom_boxplot() +
  facet_wrap(~ site_name, ncol = 3) + 
  labs(x = "Meander Type", y = "Species Richness") +
  ggtitle("Species Richness by Meander Type, Faceted by Site Name") +
  theme_minimal()

plotrichness

modelrichness <- glmer(speciesrichness ~ meander_type + (1 | site_name), data = mscdissdata_invertdata_, family = poisson(link = "log"))
summary(modelrichness)
modelrichnessnull <- glmer(speciesrichness ~ 1 + (1 | site_name), data = mscdissdata_invertdata_, family = poisson(link = "log"))
anova(modelrichness, modelrichnessnull)






##### Abundance analysis
modelabundance <- glmer(abundance ~ meander_type + (1 | site_name), data = mscdissdata_invertdata_, family = poisson(link = "log"))
summary(modelabundance)
modelabundancenull <- glmer(abundance ~ 1 + (1 | site_name), data = mscdissdata_invertdata_, family = poisson(link = "log"))
anova(modelabundance, modelabundancenull)

plotabundance <- ggplot(mscdissdata_invertdata_, aes(x = meander_type, y = abundance)) +
  geom_boxplot() +
  facet_wrap(~ site_name, ncol = 3) + 
  labs(x = "Meander Type", y = "Abundance") +
  ggtitle("Invertebrate Abundance by Meander Type, Faceted by Site Name") +
  theme_minimal()
plotabundance
