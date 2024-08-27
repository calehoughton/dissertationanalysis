#Testing data from spreadsheet with averaged difference between remeandered and straightened diversities
library(dplyr)

#Testing normality
qqnorm(resid(yearssincerm))
qqline(resid(yearssincerm))
plot(yearssincerm)

#LM of project age
yearssincerm <- lm(difference ~ years_since_rm, data = mscdissdata_siteaverages_)
summary(yearssincerm)

#Data set without Garrell Burn, which was an outlier
filtered_data <- mscdissdata_siteaverages_ %>%
  filter(site_name != "garrell")

yearssincermmodified <- lm(difference ~ years_since_rm, data = filtered_data)
summary(yearssincermmodified)

#Plotting
ggplot(mscdissdata_siteaverages_, aes(x = years_since_rm, y = difference, label = site_name)) +
  geom_point() +                    
  geom_smooth(method = "lm") +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  labs(title = "Difference in Shannon's Diversity From Straightened Sections vs Years Since Meandering",
       x = "Years Since Meandering",
       y = "Difference in Shannon's") +
  theme_minimal()  

ggplot(filtered_data, aes(x = years_since_rm, y = difference, label = site_name)) +
  geom_point() +              
  geom_smooth(method = "lm") +   
  geom_text(vjust = 1, hjust = 1, size = 3) +
  labs(title = "Difference in Shannon's Diversity From Straightened Sections vs Years Since Meandering",
       x = "Years Since Meandering",
       y = "Difference in Shannon's") +
  theme_minimal() 


#Here I test the distance remeandered, which was not included
distancerm <- lm(difference ~ distance_remeandered, data = mscdissdata_siteaverages_)
summary(distancerm)
