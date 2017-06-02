# RMSEP + House Effects

# Packages ----------------------------------------------------------------

library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(s20x)
library(R330)
library(mgcv)

# Loading in Data ---------------------------------------------------------

setwd("~/Election Prediction Model/Polling Data")

RMSEP.df <- read_csv("RMSEP Polls.csv")
House.df <- read_csv("90 Days Before Poll Error.csv")


# RMSEP v Days Before -----------------------------------------------------

plot(RMSEP~`Days Before`, data = RMSEP.df)
lines(lowess(RMSEP.df$`Days Before`, RMSEP.df$RMSEP))


# Estimating House Effects ------------------------------------------------

# This will fit a glm for each party and use days before and pollster as explanatory hoping to get an estimate house effect for each pollster/party combo

Labour.fit <- glm(Labour~`Days Before`+`Pollster`, data = House.df)
National.fit <- glm(National~`Days Before`+`Pollster`, data = House.df)
NZFirst.fit <- glm(`NZ First`~`Days Before`+`Pollster`, data = House.df)
Act.fit <- glm(ACT~`Days Before`+`Pollster`, data = House.df)
Green.fit <- glm(Green~`Days Before`+`Pollster`, data = House.df)
UnitedFuture.fit <- glm(`United Future`~`Days Before`+`Pollster`, data = House.df)
Maori.fit <- glm(Maori~`Days Before`+`Pollster`, data = House.df)
Mana.fit <- glm(Mana~`Days Before`+`Pollster`, data = House.df)
Conservative.fit <- glm(Conservative~`Days Before`+`Pollster`, data = House.df)

# This is the same as above apart from also uses the average miss of polls in the last month (excluding that poll if it was in the last month) as explanatory
# Will likely use this estimation of house effects as it deals with a national polling miss. Will estimate a national polling miss as a random variable? Probably have it off National and use the co-var matrix to spit out numbers for everyone elses change

Labour.fit1 <- glm(Labour~`Days Before`+`Pollster`+`Labour Miss`, data = House.df)
National.fit1 <- glm(National~`Days Before`+`Pollster`+`National Miss`, data = House.df)
NZFirst.fit1 <- glm(`NZ First`~`Days Before`+`Pollster`+`NZ First Miss`, data = House.df)
Act.fit1 <- glm(ACT~`Days Before`+`Pollster`+`ACT Miss`, data = House.df)
Green.fit1 <- glm(Green~`Days Before`+`Pollster`+`Green Miss`, data = House.df)
UnitedFuture.fit1 <- glm(`United Future`~`Days Before`+`Pollster`+`United Future Miss`, data = House.df)
Maori.fit1 <- glm(Maori~`Days Before`+`Pollster`+`Maori Miss`, data = House.df)
Mana.fit1 <- glm(Mana~`Days Before`+`Pollster`+`Mana Miss`, data = House.df)
Conservative.fit1 <- glm(Conservative~`Days Before`+`Pollster`+`Conservative Miss`, data = House.df)

# Storing Estimates + STDevs
Labour.df <- summary(Labour.fit1)$coef[,c(1:2)]
National.df <- summary(National.fit1)$coef[,c(1:2)]
NZFirst.df <- summary(NZFirst.fit1)$coef[,c(1:2)]
Act.df <- summary(Act.fit1)$coef[,c(1:2)]
Green.df <- summary(Green.fit1)$coef[,c(1:2)]
UnitedFuture.df <- summary(UnitedFuture.fit1)$coef[,c(1:2)]
Maori.df <- summary(Maori.fit1)$coef[,c(1:2)]
Mana.df <- summary(Mana.fit1)$coef[,c(1:2)]
Conservative.df <- summary(Conservative.fit1)$coef[,c(1:2)]

AllHouse.df <- rbind(Labour.df, National.df, NZFirst.df, Act.df, Green.df, UnitedFuture.df, Maori.df, Mana.df, Conservative.df)

write.csv(AllHouse.df, "House Effects for 14.csv")

# House Effects for 2017

House.df <- read_csv("90 Days Before Poll Error fOR 2017.csv")

Labour.fit1 <- glm(Labour~`Days Before`+`Pollster`+`Labour Miss`, data = House.df)
National.fit1 <- glm(National~`Days Before`+`Pollster`+`National Miss`, data = House.df)
NZFirst.fit1 <- glm(`NZ First`~`Days Before`+`Pollster`+`NZ First Miss`, data = House.df)
Act.fit1 <- glm(ACT~`Days Before`+`Pollster`+`ACT Miss`, data = House.df)
Green.fit1 <- glm(Green~`Days Before`+`Pollster`+`Green Miss`, data = House.df)
UnitedFuture.fit1 <- glm(`United Future`~`Days Before`+`Pollster`+`United Future Miss`, data = House.df)
Maori.fit1 <- glm(Maori~`Days Before`+`Pollster`+`Maori Miss`, data = House.df)
Mana.fit1 <- glm(Mana~`Days Before`+`Pollster`+`Mana Miss`, data = House.df)
Conservative.fit1 <- glm(Conservative~`Days Before`+`Pollster`+`Conservative Miss`, data = House.df)

# Storing Estimates + STDevs
Labour.df <- summary(Labour.fit1)$coef[,c(1:2)]
National.df <- summary(National.fit1)$coef[,c(1:2)]
NZFirst.df <- summary(NZFirst.fit1)$coef[,c(1:2)]
Act.df <- summary(Act.fit1)$coef[,c(1:2)]
Green.df <- summary(Green.fit1)$coef[,c(1:2)]
UnitedFuture.df <- summary(UnitedFuture.fit1)$coef[,c(1:2)]
Maori.df <- summary(Maori.fit1)$coef[,c(1:2)]
Mana.df <- summary(Mana.fit1)$coef[,c(1:2)]
Conservative.df <- summary(Conservative.fit1)$coef[,c(1:2)]

AllHouse.df <- rbind(Labour.df, National.df, NZFirst.df, Act.df, Green.df, UnitedFuture.df, Maori.df, Mana.df, Conservative.df)

write.csv(AllHouse.df, "House Effects for 17.csv")
