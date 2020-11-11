## Snake Rostral Cooling

# Set working directory
setwd("C:/R/Work/rostralcooling")

# Load relevant packages
library(multcomp) # Post hoc comparisons
library(lme4) #GLM

# Import CSV file, opens a window
temps <- read.csv(file.choose())

# Descriptive statistics of durations, averages, outputted into a txt file
describe(temps)
stargazer(temps, type="text", title="Descriptive Statistics for Temperature Data", 
          digits=3, out="tempsdescriptive.txt")

# Check assumptions of data

# ANCOVA on landmark data, with species as covariate
ancova.temp <- aov(temp ~ landmark + species, data = temps)

# View results
anova(ancova.temp, type="III")

# Post hoc tests
postancova.temp <- glht(ancova.temp, linfct = mcp(technique = "Tukey"))

# view a summary and confidence intervals of the post hoc comparisons
summary(postancova.temp)
confint(postancova.temp)

# T-test for head surface area
ttest.temp <- t.test(warm_percent, cold_percent, data=temps, paired = TRUE)
ttest.temp

# Generalized linear model
glm.temp.full <- glmm(cold_percent ~ surface_area + species + SVL, data=temps)
glm.temp.sa <- glmm(cold_percent ~ species + SVL, data=temps)
glm.temp.species <- glmm(cold_percent ~ surface_area + SVL, data=temps)
glm.temp.svl <- glmm(cold_percent ~ surface_area + species, data=temps)

# Compare the models for the likelihood test
likelihood.temp <- anova(glm.temp.full, glm.temp.sa, glm.temp.species, glm.temp.svl)

