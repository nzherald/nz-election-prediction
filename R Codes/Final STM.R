# Strong Transition Model Linear Strength:

## This is an edited version of the strong transition model rather than classifying the parties in three groups what is instead being done is that the threshold for voters to be considered strong voters is = Total Party Vote + 5% with an upper limit to be defined.


# Packages ----------------------------------------------------------------

library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(s20x)
library(R330)
library(DEoptim)

# Loading in Data ---------------------------------------------------------

# Read http://www.electoralcalculus.co.uk/strongmodel.html before continuing

setwd("~/Election Prediction Model/Previous Election Data/Strong Transition Model")

ElectorateData.df <- read_csv("Electorate Data for STM.csv")
Party.df <- read_csv("Party Change.csv") # This data frame will store nationwide change in strong/weak and loyalty values
Party.df <- arrange(Party.df, Party, Year) # Orders parties in chronological order so that later code works !!! VERY IMPORTANT DO NOT DELETE OR ELSE OPTIMISATION WILL NOT WORK !!!

# Constants for Optimisation ----------------------------------------------

# Parameters are as follows:
# param[1] - Upper limit for strength

pari = c(0.32)

# These next constants are the sum of the positive total changes -> these are here in order for optimisation to be speed up
T1999 <- 0.1665
T2002 <- 0.1843
T2005 <- 0.2029
T2008 <- 0.0967
T2011 <- 0.1297

# Function for STM --------------------------------------------------------

## Edit 13/06 Adjusted so that function is bootstrapping predictions -> will sig increase run time but have to live with it -> also made param 1 always = 0.2

STM <- function(param, Electorates, Parties){
  
  # The Following Lines mutate the data frame to calculate the strong and weak voters in each electorate
  
  Electorates <- mutate(Electorates, Type = "Real") # This is here because can't be bothered changing col numbers etc.
  Electorates <- mutate(Electorates, `Weak Raw` = if_else(`Total Vote`>param[1]-0.05,if_else(Percent>param[1],param[1]*Votes,Percent*Votes),if_else(Percent>`Total Vote`+0.05,(`Total Vote`+0.05)*Votes,Percent*Votes)))
  Electorates <- mutate(Electorates, `Strong Raw`= Percent*Votes-`Weak Raw`)
  Electorates <- mutate(Electorates, Weak = `Weak Raw`/Votes)
  Electorates <- mutate(Electorates, Strong = `Strong Raw`/Votes)
  
  # Annoying Loop to calculate Prev Weak, Prev Strong
  i = 1
  Electorates <- mutate(Electorates, `Prev Weak` = 0)
  Electorates <- mutate(Electorates, `Prev Strong` = 0)
  while (i<dim(Electorates)[1]+1){
    Electorates[i,14] <- ifelse(dim(subset(Electorates, Electorates[,1] == as.character(Electorates[i,1]) & Electorates[,2] == as.numeric(Electorates[i,2]-3) & Electorates[,3]==as.character(Electorates[i,3]))[,12])[1]==0, NA, subset(Electorates, Electorates[,1] == as.character(Electorates[i,1]) & Electorates[,2] == as.numeric(Electorates[i,2]-3) & Electorates[,3]==as.character(Electorates[i,3]))[,12])
    Electorates[i,15] <- ifelse(dim(subset(Electorates, Electorates[,1] == as.character(Electorates[i,1]) & Electorates[,2] == as.numeric(Electorates[i,2]-3) & Electorates[,3]==as.character(Electorates[i,3]))[,13])[1]==0,NA,subset(Electorates, Electorates[,1] == as.character(Electorates[i,1]) & Electorates[,2] == as.numeric(Electorates[i,2]-3) & Electorates[,3]==as.character(Electorates[i,3]))[,13])
    i = i+1
  }
  
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
  
  # Jammy code which gives previous weak and strong voters so loyalty can be calculated (do not alter!!!) unless know how to do what is desired
  
  Parties <- mutate(Parties, `Prev Weak` = 0)
  Parties[1,9] <- NA
  Parties[c(2:50),9] <- Parties[c(1:49),7]
  Parties[c(7,13,14,19,23,29,30,33,39,45),9] <- NA
  Parties <- mutate(Parties, `Prev Strong` = 0)
  Parties[1,10] <- NA
  Parties[c(2:50),10] <- Parties[c(1:49),8]
  Parties[c(7,13,14,19,23,29,30,33,39,45),10] <- NA
  
  # Loyalty of Weak voters (and strong if applicable)
  
  Parties <- mutate(Parties, Loyalty = ifelse(`T Chng`>0, 0, ifelse(`Prev Strong`>`Total Vote`,0,(`Total Vote`-`Prev Strong`)/`Prev Weak`)))
  Parties <- mutate(Parties, SLoyalty = ifelse(`Total Vote`>`Prev Strong`,1,`Total Vote`/`Prev Strong`))
  # Hard Coding Jim 2011 dummy vars
  Parties[22,12] <- 0
  
  # Estimating Lost Voters
  i = 1
  Electorates <- mutate(Electorates, Lost = 0)
  while(i<dim(Electorates)[1]+1){
    Electorates[i,16] <- ifelse(as.numeric(Electorates[i,8])>0, 0, (1-as.numeric(filter(Parties, Party==as.character(Electorates[i,3])&Year==as.numeric(Electorates[i,2]))[,11]))*as.numeric(Electorates[i,14])+(1-as.numeric(filter(Parties, Party==as.character(Electorates[i,3])&Year==as.numeric(Electorates[i,2]))[,12]))*as.numeric(Electorates[i,15]))
    i = i +1
  }
  
  # Now adding voters
  
  i = 385 # First electorate in 1999
  j = 1 # counter for electorate + year combinations
  Electorates <- mutate(Electorates, Gained = 0)
  Electorates <- mutate(Electorates, Pred = 0)
  Electorates <- mutate(Electorates, Index = c(1:dim(Electorates)[1]))
  # adding an index so that when I filter the data frame I can know which rows need replacement
  Electorates <- Electorates[,c(19,1:18)]
  # reordering so index is the first row
  while(j<323){ 
    Gainz <- filter(Electorates, Year==as.numeric(Electorates[i,3])& `Electoral District`==as.character(Electorates[i,2]))
    Remain <- sum(Gainz$Lost)
    ChngT <- ifelse(Gainz[1,3]==1999,T1999,ifelse(Gainz[1,3]==2002,T2002,ifelse(Gainz[1,3]==2005,T2005,ifelse(Gainz[1,3]==2008,T2008,ifelse(Gainz[1,3]==2011,T2011,0)))))
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
  
  # Now removing the electorates and years for which preds aren't being made so that RMSEP can be accurately calculated
  E1996 <- filter(Electorates, Year==1996)
  Electorates <- setdiff(Electorates, E1996)
  NAElecs <- filter(Electorates, is.na(Pred))
  Electorates <- setdiff(Electorates, NAElecs)
  Jim2011 <- filter(Electorates, Party=="Jim Anderton's Progressive"&Year==2011)
  Electorates <- setdiff(Electorates, Jim2011)
  
  Electorates <- mutate(Electorates, Error = (Percent-Pred))
  Errors <- c(0,0,0,0,0)
  m = 1
  while(m<6){
    Errors[m] <- sqrt(mean((filter(Electorates, Year == 1996+3*m)$Error)^2))
    m= m+1
  }
  assign("PostParty", Parties, envir = globalenv())
  assign("PostElec", Electorates, envir = globalenv())
  mean(Errors)
}

STM(param = 0.32, Electorates = ElectorateData.df, Parties = Party.df)