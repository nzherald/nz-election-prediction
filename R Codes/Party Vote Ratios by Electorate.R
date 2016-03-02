# Analysis of Individual Electorate Votes vs Electorate Type if Applicable

# Packages ----------------------------------------------------------------

library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)

# Loading in Data ---------------------------------------------------------

setwd("~/Election Prediction Model/Previous Election Data/Party Votes")

PVYE.df = read_csv("Party Vote All Years All Electorates.csv")

# Creating a Function to Calculate Ratio ----------------------------------

PartyRatio <- function(PVYE.df, TParty, RParty.df){
  TYear = 1996
  repeat {
    TPartyG.df = filter(PVYE.df, Party == TParty & Year == TYear & Maori == "FALSE")
    GEP = filter(PVYE.df, Party == TParty & Year == TYear & Maori == "TOTAL")$Percent
    TPartyG.df = mutate(TPartyG.df, Ratio = Percent/GEP)
    TPartyM.df = filter(PVYE.df, Party == TParty & Year == TYear & Maori == "TRUE")
    MEP = filter(PVYE.df, Party == TParty & Year == TYear & Maori == "TOTAL")$Percent
    TPartyM.df = mutate(TPartyM.df, Ratio = Percent/MEP)
    RParty.df = if(TYear>1996) {
      bind_rows(RParty.df, TPartyM.df, TPartyG.df)} else {
        bind_rows(TPartyM.df, TPartyG.df)
      }
    TYear = TYear+3
    if(TYear>2014) break
  }
  assign('RParty.df', RParty.df, .GlobalEnv)
}

# Calculating Rationes ----------------------------------------------------

TParty = "GREEN PARTY"
RParty.df = filter(PVYE.df, Year == 2009)
RParty.df = mutate(RParty.df, Ratio = 0)
Rationes.df = RParty.df
PartyRatio(PVYE.df, TParty)
