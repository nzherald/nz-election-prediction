# Candidate GLM builder

# Packages ----------------------------------------------------------------

library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(s20x)
library(R330)

# Loading in Data ---------------------------------------------------------

setwd("~/Election Prediction Model/Previous Election Data/Candidate Data")

Candidate.df <- read_csv("General Election Candidate Data for 14.csv")

CandidateB14.df <- filter(Candidate.df, Year<2012) # Training Set
CandidateA14.df <- filter(Candidate.df, Year==2014) # Test Set


# Fitting Models ----------------------------------------------------------

# N.B. the variable Vote to Use is in reference to the previous candidate vote which will be used in the model it is defined as follows:
# Same candidate + Same electorate -> That candidate's vote in the last election
# Different candidate + same electorate -> The Previous candidate's vote in that electorate last election
# Same candidate + different (but not new) electorate -> mixture of candidate's previous vote and previous vote in that electorate (Mixture defined by the model that follows)
# New candidate + new (or electorate that previous election didn't have candidate of that party run in) electorate -> Seperate model defined later


# Mix Calculations --------------------------------------------------------

# Calculating how to mix values

Mixed.df <- filter(CandidateB14.df, `Mixer`==TRUE)
allpossregs(`Candidate Vote`~`Party Vote Electorate`+Incumbent+`Same Party`*(`Candidate Previous Vote`+`Previous Candidate Vote electorate`), data=Mixed.df)

# Best is below (chose using Cross Validation)
Mixed.fit <- glm(`Candidate Vote`~`Party Vote Electorate`+Incumbent+`Previous Candidate Vote electorate`, data=Mixed.df)

# So should only use Previous candidate vote electorate will now replace these values on Candidate B14.df

i=1
while(i<1860){
  if(CandidateB14.df[i,15]==TRUE){
    CandidateB14.df[i,7] <- CandidateB14.df[i,9]
  }
  i = i+1
}

# Now seperating into first time party runs in and non
NewCand.df <- filter(CandidateB14.df, is.na(`Vote to Use`))
RestB14.df <-filter(CandidateB14.df, !is.na(`Vote to Use`))



# First Time Model --------------------------------------------------------

FirstTimers.fit <- glm(log(`Candidate Vote`)~`Party Vote Electorate`, data = NewCand.df)
plot(FirstTimers.fit, which = 1:6)

# Two issues - Nat running in Ohariu in 02 after not 99 (perhaps just hard code all Ohariu will see after next model)
# Other is underesimate of Colin Craig by ~16/17% perhaps add a party leader boost?

## FIX ## Will be to remove these two as likely won't be required again unless Gareth Morgan runs in which case will use a model similar to planned model for the Maori Electorates

## FIX ## 02/06 changed response to log now have to deal with Green party. Will use + Party as an all poss regs with CV + PV *1000 indicates best -> Also using a log party vote term because should be all log

FirstTimers.fit1 <- glm(log(`Candidate Vote`)~log(`Party Vote Electorate`)+Party, data = NewCand.df[-c(146, 191),])
plot(FirstTimers.fit1, which = 1:6)

# General Use Model -------------------------------------------------------

### EDIT 31/05: Added *Party as term b/c is important - see Caleb's analysis from last year

allpossregs(log(`Candidate Vote`*10000)~I(`Party Vote Electorate`*10000)*Party+I(`Vote to Use`*10000)*(`Same Electorate`+`Same Candidate`+`Same Party`)+Incumbent, data = RestB14.df)

# Now dropping stuff not imp:

allpossregs(log(`Candidate Vote`*10000)~I(`Party Vote Electorate`*10000)*Party+I(`Vote to Use`*10000)+Incumbent, data = RestB14.df)

# Note the *10000 in both of above is to ensure that the response is positive so that all poss regs works

# Going to re do this once dropped the points which cause issues

# Best Model Using Cross Validation
Candidate.fit <- glm(log(`Candidate Vote`)~`Party Vote Electorate`*Party+`Vote to Use` + `Incumbent`, data = RestB14.df)
plot(Candidate.fit, which = 1:6)

# Three issues - Dunne 1999 misses by 26% points b/c low party vote, Nat cand Epsom 2008 overshoots by 40% b/c given to Act, 2011 Act Epsom misses by 19% bc low party vote 
# Also need to figure out how to deal with Wigram the year that Jim starts his own party as alliance candidate misses projections by 20% (after removing the three above)
# Doing more analysis shows Dunne is consistently under predicted by 10-20%

## FIX ONE ## Will remove Ohariu and fit it as its own model

# Need to figure out other issues - considering just removing Alliance candidate that misses proj as same issue not going to be required (hopefully)

## FIX TWO ## Will remove Epsom 08 + 11 and fit a model which has ACT + NAT dummy variables

Ohariu.df <- filter(RestB14.df, Electorate == "Ohariu")
Epsom08.df <- filter(RestB14.df, Electorate == "Epsom" & Year == 2008)
Epsom11.df <- filter(RestB14.df, Electorate == "Epsom" & Year == 2011)
Epsom.df <- rbind(Epsom08.df, Epsom11.df)
RestB14.df <- setdiff(RestB14.df, Ohariu.df)
RestB14.df <- setdiff(RestB14.df, Epsom.df)

Candidate.fit1 <- glm(log(`Candidate Vote`)~`Party Vote Electorate`*Party+`Vote to Use`+`Vote to Use` + `Incumbent`, data = RestB14.df)
plot(Candidate.fit1, which = 1:6)

# Jim Anderton causes issues - can solve the 2002 Alliance candidate by treating them as a first timer but still Jim is a pain. Perhaps I'll have to hard code Wigram 02-08?

## FIX THREE ## No need to hard code Wigram 02-08 instead will just drop the rows from the dataset

RestB14.df <- RestB14.df[-c(1534:1555),]

Candidate.fit2 <- glm(log(`Candidate Vote`)~`Party Vote Electorate`*Party+`Vote to Use` + `Incumbent`, data = RestB14.df)
plot(Candidate.fit2, which = 1:6)

# Again Normal Q-Q not perfect perhaps Log of Party vote and vote to use?

# Both

Candidate.fit3 <- glm(log(`Candidate Vote`)~log(`Party Vote Electorate`)*Party+log(`Vote to Use`) + `Incumbent`, data = RestB14.df)
plot(Candidate.fit3, which = 1:6)

# Party Only

Candidate.fit4 <- glm(log(`Candidate Vote`)~log(`Party Vote Electorate`)*Party+`Vote to Use` + `Incumbent`, data = RestB14.df)
plot(Candidate.fit4, which = 1:6)

# Vote to use only

Candidate.fit6 <- glm(log(`Candidate Vote`)~`Party Vote Electorate`*Party+log(`Vote to Use`) + `Incumbent`, data = RestB14.df)
plot(Candidate.fit5, which = 1:6)

# In the Interaction models Jim Anderton 1999 and Winston 2002 spanners but probably won't drop Winnie although considering removing Jim although to be fair the Alliance values don't matter any more so may just leave him

# Lots of this is Mangere - a labour strong hold where party vote + cand vote is very high. Will not worry about leverage normality is slightly concerning - for now willn just leave it and hope that the co-variance calculations will solve all woes

## FIX ## Going to just leave Jim b/c Alliance values don't matter and won't remove Winston because potentially just outlier. The interacton models don't work well for Maori + Progressive but will have to live with it because Maori only run small number of candidates in GE and progressive no longer exist.

# Issue with the Party terms is that for first time Parties such as Mana and Conservative in 2014 will have to figure this out

# 03/06 Appears that the log- log +log model is the best based on AIC and residual deviance. Will therefore use this model (3)

# Model which use + Party rather than * Party for Mana + Conservative

Candidate.fit5 <- glm(log(`Candidate Vote`)~log(`Party Vote Electorate`)+Party+log(`Vote to Use`) + `Incumbent`, data = RestB14.df)
plot(Candidate.fit5, which = 1:6)

# Individual Electorate Models --------------------------------------------

# Ohariu

## N.B. On 03/06 did analysis to see if should use log candidate vote and log explanatory -> however b/c small samples and strange distributions doesn't work therefore went to a model which only has log response

allpossregs(log(`Candidate Vote`*1000)~`Party Vote Electorate`+`Vote to Use`+Incumbent+I(`Party Vote Electorate`^2)+I(`Vote to Use`^2), data = Ohariu.df)

Ohariu.fit <- glm(log(`Candidate Vote`)~`Party Vote Electorate`+`Vote to Use`+I(`Party Vote Electorate`^2)+I(`Vote to Use`^2)+Incumbent, data = Ohariu.df)

# Epsom 08+11
Epsom.df <- mutate(Epsom.df, ACT = c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE))
Epsom.df <- mutate(Epsom.df, National = c(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE))

# all poss regs not working therefore doing this by hand

Epsom.fit <- glm(log(`Candidate Vote`)~log(`Party Vote Electorate`)+ACT+National+log(`Vote to Use`), data = Epsom.df)
Epsom.fit1 <- glm(log(`Candidate Vote`)~log(`Party Vote Electorate`)+ACT+National, data = Epsom.df)
Epsom.fit2 <- glm(log(`Candidate Vote`)~log(`Party Vote Electorate`)+ACT, data = Epsom.df)

# Not great but hopefully works for 2014


# 2014 Testing ------------------------------------------------------------

# Setting Up Data Frames

i=1
while(i<370){
  if(CandidateA14.df[i,15]==TRUE){
    CandidateA14.df[i,7] <- CandidateA14.df[i,9]
  }
  i = i+1
}

Ohariu14.df <- filter(CandidateA14.df, Electorate == "Ohariu")
Epsom14.df <- filter(CandidateA14.df, Electorate == "Epsom")
RestA14.df <- setdiff(CandidateA14.df, Ohariu14.df)
RestA14.df <- setdiff(RestA14.df, Epsom14.df)
Epsom14.df <- mutate(Epsom14.df, ACT = if(Party=="ACT") TRUE else FALSE)
Epsom14.df <- mutate(Epsom14.df, National = if(Party=="ACT") TRUE else FALSE)
# Above two rows don't work so just using to make new column now making the values correct
Epsom14.df[2,16] <- TRUE
Epsom14.df[4,17] <- TRUE

# Ohariu + Epsom 2014 -----------------------------------------------------

Ohariu14.df <- mutate(Ohariu14.df, Pred = exp(predict.glm(Ohariu.fit, Ohariu14.df)))

# Testing Epsom
Epsom14.df <- mutate(Epsom14.df, Pred = exp(predict.glm(Epsom.fit2, Epsom14.df)))


# First Timers 2014 -------------------------------------------------------

# Testing the First timers and the Others

New14.df <- filter(RestA14.df, is.na(`Vote to Use`))
RestA14.df <- setdiff(RestA14.df, New14.df)

New14.df <- mutate(New14.df, Pred = exp(predict.glm(FirstTimers.fit1, New14.df)))

## FIX ## for Mana and Conservative going to use a simple estimte from the first time last election where their Party value is + the margin of missing. This will be used for all models

# Conservative + Mana 2014 ------------------------------------------------

# Seperating Conservative and Mana

Conservative.df <- filter(RestA14.df, Party == "Conservative Party")
Mana.df <- filter(RestA14.df, Party == "Mana")
RestA14.df <- setdiff(RestA14.df, Conservative.df)
RestA14.df <- setdiff(RestA14.df, Mana.df)

# Calculating Constants 

Conservative11.df <- filter(NewCand.df, Party == "Conservative Party")
ConservativeE11 <- mean(Conservative11.df$`Candidate Vote`-exp(predict.glm(FirstTimers.fit1, Conservative11.df)))

Mana11.df <- filter(NewCand.df, Party == "Mana")
ManaE11 <- mean(Mana11.df$`Candidate Vote`-exp(predict.glm(FirstTimers.fit1, Mana11.df)))

# Now predicting values - N.B. formula used is the from Candidate fit 5 + errors

Conservative.df <- mutate(Conservative.df, Pred = exp(Candidate.fit5$coef[1]+Candidate.fit5$coef[2]*log(`Party Vote Electorate`)+Candidate.fit5$coef[11]*log(`Vote to Use`))+ConservativeE11)

Mana.df <- mutate(Mana.df, Pred = exp(Candidate.fit5$coef[1]+Candidate.fit5$coef[2]*log(`Party Vote Electorate`)+Candidate.fit5$coef[11]*log(`Vote to Use`))+ManaE11)

# The Rest 2014 -----------------------------------------------------------

# Predicting for the model

RestA14P.df <- mutate(RestA14.df, Pred = exp(predict.glm(Candidate.fit3, RestA14.df)))

# Combining all for a 2014 master list

Epsom14.df <- Epsom14.df[,-c(16:17)]
Pred14.df <- bind_rows(RestA14P.df, New14.df, Epsom14.df, Ohariu14.df, Conservative.df, Mana.df)

write.csv(Pred14.df, "log~log+log.csv")
