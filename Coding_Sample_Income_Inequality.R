
library(faraway)
library(texreg)
library(stargazer)
library(interplot)
library(interactions)


# importing data set 
ESS_9 <- read.csv('/Users/christine/Dropbox/ESS/ESS9e03_1.csv')

# sub-setting data frame
# -> countries, age, edu level, gender, gov role, net household income, fairness of wealth disparity 
sub_ESS_9 <- ESS_9[ESS_9$cntry %in% c("NL", "BE", "FR", "IT", "DE"), c("essround", "cntry", "agea", "eisced", "gndr", "gincdif", "hinctnta", "wltdffr")]

# adding dummy variable for gender (1 = women & 0 = men)
sub_ESS_9$gndr <- ifelse(sub_ESS_9$gndr == 1, 1, 0)

# dropping rows associated w 0's and cleaning education level (eisced)
sub_ESS_9[sub_ESS_9$eisced==0] <- NA

# Shifts 77/88/55 to NA's - R removes this during regressions 
sub_ESS_9[sub_ESS_9 == 77] <- NA
sub_ESS_9[sub_ESS_9 == 55] <- NA
sub_ESS_9[sub_ESS_9 == 88] <- NA
```

##### MAKING MODELS & INTERACTION TERMS ####
# all variables
m <- lm(wltdffr ~ eisced + gndr + agea + hinctnta, data = sub_ESS_9)

# interaction btwn edu level & net household income
m_interaction <- lm(wltdffr ~ eisced*hinctnta, data = sub_ESS_9)

# interaction btwn cntry, net household income
m_interaction2 <- lm(wltdffr ~ cntry*hinctnta, data = sub_ESS_9)

# Descriptive Statistics 
summary(m) #stargazer(m, type = "text") 
summary(m_interaction) #stargazer(m_interaction, type = "text") 

# Regression tables to transfer over to Overleaf 
stargazer(m, m_interaction) 
stargazer(m, m_interaction2)



# Plotting Interaction Terms
interact_plot(m_interaction, pred = "hinctnta", modx = "eisced", interval = TRUE,
              legend.main = "Education Level",
              main.title = "Interaction Plot",
              x.label = "Household Income",
              y.label = "Predicted Probability of Perception of Income Inequality",
              colors = c('maroon3', 'hotpink', 'violet'))


# Interacting Plots by EU Country 
interact_plot(m_interaction2, pred = "hinctnta", modx = "cntry", interval = TRUE,
              legend.main = "Education Level",
              main.title = "Interaction Plot",
              x.label = "Household Income",
              y.label = "Predicted Probability of Perception of Income Inequality")



