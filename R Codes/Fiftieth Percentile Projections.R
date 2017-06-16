# 2014 50% Predictions::

# Packages ----------------------------------------------------------------

library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(s20x)
library(R330)

# Reading in Data ---------------------------------------------------------

setwd("~/Election Prediction Model/Previous Election Data/Strong Transition Model")
Electorate14Data.df <- read_csv("Electorate Data STM 14 Pred.csv")
Party14.df <- read_csv("Party Change 14 Pred.csv")

# Party Vote Electorate ---------------------------------------------------

STM <- function(param, Electorates, Parties){
  
  # The Following Lines mutate the data frame to calculate the strong and weak voters in each electorate
  
  Electorates <- mutate(Electorates, Type = "Real") # This is here because can't be bothered changing col numbers etc.
  Electorates <- mutate(Electorates, `Weak Raw` = if_else(`Total Vote`>param[1]-0.05,if_else(Percent>param[1],param[1]*Votes,Percent*Votes),if_else(Percent>`Total Vote`+0.05,(`Total Vote`+0.05)*Votes,Percent*Votes)))
  Electorates <- mutate(Electorates, `Strong Raw`= Percent*Votes-`Weak Raw`)
  Electorates <- mutate(Electorates, Weak = `Weak Raw`/Votes)
  Electorates <- mutate(Electorates, Strong = `Strong Raw`/Votes)
  
  # This just hard codes in Prev Weak/Strong
  PrevElec <- read_csv("PostElec414.csv")
  Electorates <- mutate(Electorates, `Prev Weak` = 0)
  Electorates[,14] <- PrevElec[,4]
  Electorates <- mutate(Electorates, `Prev Strong` = 0)
  Electorates[,15] <- PrevElec[,5]
  
  # This calculates strong/weak votes for each year and the loyalty rates for each party
  
  # Force into annoying loops b/c don't know how to do what I want to and can't find help on google D:
  
  i = 1
  Parties <- mutate(Parties, `Total Weak` = 0)
  Parties <- mutate(Parties, `Total Strong` = 0)
  while(i<dim(Parties)[1]+1){
    Parties[i,5] <- sum(filter(Electorates, Party == as.character(Parties[i,1])& Year == as.numeric(Parties[i,2]))$`Weak Raw`)
    Parties[i,6] <- sum(filter(Electorates, Party == as.character(Parties[i,1])& Year == as.numeric(Parties[i,2]))$`Strong Raw`)
    i = i+1
  }
  
  Parties <- mutate(Parties, `Weak %` = `Total Vote`*`Total Weak`/(`Total Weak`+`Total Strong`))
  Parties <- mutate(Parties, `Strong %` = `Total Vote`*`Total Strong`/(`Total Weak`+`Total Strong`))
  
  # Hard coding Alliance
  Parties[2,8] <- 0
  Parties[2,7] <- 0
  # Done by hand because easier
  
  Parties <- mutate(Parties, `Prev Weak` = c(0.0107,0.0005,0.0265,0.10413988,0.25287202,0.0108,0.0143,0.300517515,0.06512652,0.006))
  Parties <- mutate(Parties, `Prev Strong` = c(0,0,0,0.00064601207,0.0219279819,0,0,0.1725828511,0.0007734848,0))
  
  # Loyalty of Weak voters (and strong if applicable)
  
  Parties <- mutate(Parties, Loyalty = ifelse(`T Chng`>0, 0, ifelse(`Prev Strong`>`Total Vote`,0,(`Total Vote`-`Prev Strong`)/`Prev Weak`)))
  Parties <- mutate(Parties, SLoyalty = ifelse(`Total Vote`>`Prev Strong`,1,`Total Vote`/`Prev Strong`))
  Parties[2,12] = 1
  
  # Estimating Lost Voters
  i = 1
  Electorates <- mutate(Electorates, Lost = 0)
  while(i<dim(Electorates)[1]+1){
    Electorates[i,16] <- ifelse(as.numeric(Electorates[i,8])>0, 0, (1-as.numeric(filter(Parties, Party==as.character(Electorates[i,3])&Year==as.numeric(Electorates[i,2]))[,11]))*as.numeric(Electorates[i,14])+(1-as.numeric(filter(Parties, Party==as.character(Electorates[i,3])&Year==as.numeric(Electorates[i,2]))[,12]))*as.numeric(Electorates[i,15]))
    i = i +1
  }
  
  # Now adding voters
  
  i = 1
  j = 1 # counter for electorate + year combinations
  Electorates <- mutate(Electorates, Gained = 0)
  Electorates <- mutate(Electorates, Pred = 0)
  Electorates <- mutate(Electorates, Index = c(1:dim(Electorates)[1]))
  # adding an index so that when I filter the data frame I can know which rows need replacement
  Electorates <- Electorates[,c(19,1:18)]
  # reordering so index is the first row
  while(j<65){ 
    Gainz <- filter(Electorates, Year==as.numeric(Electorates[i,3])& `Electoral District`==as.character(Electorates[i,2]))
    Remain <- sum(Gainz$Lost)
    ChngT <- 0.056288407
    m = 1
    while(m<dim(Gainz)[1]+1){
      if(Gainz[m,9]<=0){
        m=m+1
      } else{
        Gainz[m,18] <- as.numeric(Gainz[m,18]) + Remain*as.numeric(Gainz[m,9])/as.numeric(ChngT)
        m = m+1           
      }
    }
    # Now calculating predicted values
    
    Gainz <- mutate(Gainz, Pred = `Prev Percent` - Lost + Gained)
    Electorates[c(as.numeric(Gainz[1,1]):as.numeric(Gainz[dim(Gainz)[1],1])),18] <- Gainz[,18]
    Electorates[c(as.numeric(Gainz[1,1]):as.numeric(Gainz[dim(Gainz)[1],1])),19] <- Gainz[,19]
    j = j+1
    i = as.numeric(Gainz[dim(Gainz)[1],1])+1 #skips to start of next electorate
  }
  
  Electorates <- mutate(Electorates, Error = (Percent-Pred))
  assign("STM Preds", Electorates, envir = globalenv())
}

STM(param = 0.32, Electorates = Electorate14Data.df, Parties = Party14.df)

# Calculating Candidate Vote ----------------------------------------------

# Loading in Data ---------------------------------------------------------

setwd("~/Election Prediction Model/Previous Election Data/Candidate Data")

Candidate.df <- read_csv("General Election Candidate Data for 14.csv")

CandidateB14.df <- filter(Candidate.df, Year<2012) # Training Set
CandidateA14.df <- filter(Candidate.df, Year==2014) # Test Set

# Fitting Models ----------------------------------------------------------

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

FirstTimers.fit1 <- glm(`Candidate Vote`~log(`Party Vote Electorate`), family = gaussian(log), data = NewCand.df[-c(191),])

# General Use Model -------------------------------------------------------

Ohariu.df <- filter(RestB14.df, Electorate == "Ohariu")
Epsom08.df <- filter(RestB14.df, Electorate == "Epsom" & Year == 2008)
Epsom11.df <- filter(RestB14.df, Electorate == "Epsom" & Year == 2011)
Epsom.df <- rbind(Epsom08.df, Epsom11.df)
RestB14.df <- setdiff(RestB14.df, Ohariu.df)
RestB14.df <- setdiff(RestB14.df, Epsom.df)

RestB14.df <- RestB14.df[-c(1534:1555),]


Candidate.fit4 <- glm(`Candidate Vote`~log(`Party Vote Electorate`)*Party+log(`Vote to Use`) + `Incumbent`, family = gaussian(log), data = RestB14.df[-c(1422,1530),])

# Individual Electorate Models --------------------------------------------

# Ohariu

Ohariu.fit <- glm(`Candidate Vote`~log(`Party Vote Electorate`)+log(`Vote to Use`)+Incumbent, family = gaussian(log), data = Ohariu.df)

# Epsom 08+11
Epsom.df <- mutate(Epsom.df, ACT = c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE))
Epsom.df <- mutate(Epsom.df, National = c(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE))

Epsom.fit2 <- glm(`Candidate Vote`~log(`Vote to Use`)+log(`Party Vote Electorate`)*National+ACT, family = gaussian(log), data = Epsom.df)


# Changing Party Vote in Candidate 14 -------------------------------------

# This next section will ensure that the party vote is the party as predicted in the STM section

OnlyCand.df <- semi_join(`STM Preds`, CandidateA14.df, by = c("Electoral District" = "Electorate", "Party" = "Party"))
CandidateA14.df$`Party Vote Electorate` <- OnlyCand.df$Pred

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
Epsom14.df <- mutate(Epsom14.df, Pred = exp(predict.glm(Epsom.fit2, Epsom14.df))*0.99/sum(exp(predict.glm(Epsom.fit2, Epsom14.df)))) # Note the scaling here because the model isn't the best


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

Conservative.df <- mutate(Conservative.df, Pred = exp(Candidate.fit4$coef[1]+Candidate.fit4$coef[2]*log(`Party Vote Electorate`)+Candidate.fit4$coef[11]*log(`Vote to Use`))+ConservativeE11)

Mana.df <- mutate(Mana.df, Pred = exp(Candidate.fit4$coef[1]+Candidate.fit4$coef[2]*log(`Party Vote Electorate`)+Candidate.fit4$coef[11]*log(`Vote to Use`))+ManaE11)

# The Rest 2014 -----------------------------------------------------------

# Predicting for the model

RestA14P.df <- mutate(RestA14.df, Pred = exp(predict.glm(Candidate.fit4, RestA14.df)))
Epsom14.df <- Epsom14.df[,-c(16:17)]
Pred14.df <- bind_rows(RestA14P.df, New14.df, Epsom14.df, Ohariu14.df, Conservative.df, Mana.df)


# Writing CSVs ------------------------------------------------------------

setwd("~/Election Prediction Model/2014 Preds")
write.csv(`STM Preds`, "Electorate Party Vote 14 50% Pred.csv")
write.csv(Pred14.df, "Candidate Vote 14 50% Preds.csv")