# Election Simulation Code

# This code should go as follows:
# Defining various functions and glm models:
# - Weighted Polling Average Calulation:
#   - House Effects Estimator
#   - National Polling Error Estimator
#   - Design Effect Estimator
# - Strong Transition Model for Electorate Party Vote
# - Candidate glm:
#   - Firstime candidates (where either candidate or party did not run last election)
#   - Normal candidates
#   - Hard coded Ohariu and Epsom electorates
# - Maori Electorate Party Vote estimator
# - Maori Electorate Candidate vote estimator


# Packages ----------------------------------------------------------------

library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(s20x)
library(R330)
library(mgcv)
library(mvtnorm)
library(Matrix)

# Data --------------------------------------------------------------------

setwd("~/Dropbox/0work/projects/nz/2017/election_model/nz-election-prediction/2017 Simulation")

HouseEffects.df <- read_csv("WtAve House Effects 17.csv")
Electorate17Data.df <- read_csv("Electorate Data for STM FOR 17.csv")
Party17.df <- read_csv("Post Party for 17.csv")
Candidate.df <- read_csv("General Election Candidate Data for 14.csv") # This is used to create the glms
Candidate17.df <- read_csv("Candidate 17 DataFrame INCOMPLETE.csv")
#MEPartyVote.df <- read_csv("ME Party Polling Data.csv")
GEPolls.df <- read_csv("Polls for 14pred.csv")
DesignE.df <- read_csv("Design Effects pred 17.csv")
NatPollE.df <- read_csv("Nat Error 17 pred.csv")
NatECovar.df <- read_csv("Nat Poll E CoVariance 17 pred.csv")
Covar.df <- read_csv("CoVariance ln 17 Pred.csv")
OhariuCovar.df <- read_csv("Ohariu CoVar 17.csv")
CandCovar.df <- read_csv("Candidate CoVariance 17 Pred.csv")
MEDesignE.df <- read_csv("ME Party DE.csv")
MENatE.df <- read_csv("MEPVNatPolling Error.csv")
MENateCoVar.df <- read_csv("MEPV Covariance.csv")
MEDesignECand.df <- read_csv("ME Candidate DE.csv")
MENatECand.df <- read_csv("MECNatPolling Error.csv")
MENateCandCoVar.df <- read_csv("MEC NatE Covariance.csv")
MEPVCovar.df <- read_csv("MEPV Covariance 17 Pred.csv")
MECCovar.df <- read_csv("MEC GLM Covar for 17.csv")
MECandidate.df <- read_csv("Maori Electorate Candidate Data.csv")
MECandidate17.df <- read_csv("Maori Candidate 17 INCOMPLETE.csv")
#MECPolls.df <- read_csv() # Enter When Have
#MEPartyPolls.df <- read_csv() # ditto

StoreSeats.df <- read_csv("Store Seats.csv")
StoreElecSeats.df <- read_csv("Store Elec Seats.csv")
StoreListSeats.df <- read_csv("Store List Seats.csv")
StorePV.df <- read_csv("Store PV.csv")
StoreElecPV.df <- read_csv("Store Elec PV.csv")
StoreMEPV.df <- read_csv("Store MEPV.csv")
StoreCand.df <- read_csv("Store Cand.csv")
StoreMECand.df <- read_csv("Store MECand.csv")
StoreCandWin.df <- rbind(StoreCand.df, StoreMECand.df)
StoreMECand.df <- read_csv("StoreMECand.csv")

# Weighted Polling Average ------------------------------------------------

# Function estimating and applying house effect to individuals polls
HouseEffect <- function(House, GEPolls){
  House <- mutate(House, SimEst = rnorm(dim(House)[1],Estimate,`Std Error`)+rnorm(dim(House)[1],Baseline,`Base Std Error`))
  House <- House[,-c(3:6)]
  House <- spread(House, Party, SimEst)
  PollInclude = unique(House$Pollster)
  i = 1
  while(i<=dim(GEPolls)[1]){
    k = 1
    if(GEPolls[i,1] %in% PollInclude){
      while(k<10){
        if(!is.na(GEPolls[i,k+3])){
          if(GEPolls[i,k+3] -  filter(House, as.character(Pollster)  == as.character(GEPolls[i,1]))[,k+1]>0.0005){
            GEPolls[i,k+3] <- GEPolls[i,k+3] -  filter(House, as.character(Pollster)  == as.character(GEPolls[i,1]))[,k+1]
          } else {
            GEPolls[i,k+3] <- 0.0005
          }
        }
        k = k+1
      }
    }
    i = i+1
  }
  assign("GEPollsSim.df", GEPolls, envir = globalenv())
}

# Function estimating Design Effect
DesignEffect <- function(Design){
  Design <- mutate(Design, DESim = rnorm(dim(Design)[1],DE,DEsd))
  assign("DesignSim.df", Design, envir = globalenv())
}

# Function Calculating Weighted Average
WeightAverage <- function(GEPolls){
  GEPolls <- filter(GEPolls, Poll!= "Election result")
  GEPolls <- mutate(GEPolls, `Raw Weight` = exp(-log(2)*(`Days Before`-DaysTo)/(1.96+0.2*DaysTo)))
  GEPolls <- mutate(GEPolls, `Weight` = `Raw Weight`/sum(`Raw Weight`))
  # Applying weights to each column
  i = 1
  while(i<10){
    GEPolls[,i+3] <- GEPolls[,i+3]*GEPolls[,14]
    i = i+1
  }
  # Calculating Weighted Average
  WeightAve <- data.frame(Party = colnames(GEPolls)[4:12], `Wt Ave` = colSums(GEPolls[,c(4:12)]))
  WeightAve$Wt.Ave <- log(WeightAve$Wt.Ave)
  assign("WeightAveSim.df", WeightAve, envir = globalenv())
}

# Below should be adjusted so national polling erorr is calcualted as a multivariate object

# Function estimating mean national polling error, and national polling error for the sim before applying it to the weighted average to get the adjusted polling average
AdjustedAverage <- function(NatE, Design, WtAve, Polls, NatECovar, CoVar){
  NatE <- mutate(NatE, NatEMuuSim = rnorm(dim(NatE)[1], `Muu Nat Error`, `Muu SD`))
  Support <- cbind(WtAve,`Muu Nat Error` = NatE$NatEMuuSim)
  Support <- mutate(Support, `Nom SD` = sqrt(exp(Wt.Ave)*(1-exp(Wt.Ave))/(log(dim(Polls)[1]+1)*1000)))
  Support <- cbind(Support, DESim = sqrt(Design$DESim) )
  Support <- mutate(Support, `Nat E SD` = `Nom SD`*DESim)
  Support <- mutate(Support, `log SD` = sqrt(Wt.Ave^2*(exp(`Nat E SD`^2)-1)))
  Support <- cbind(Support, `Nat E Sim` = diag(rmvnorm(dim(Support)[1],Support$`Muu Nat Error`, Support$`log SD`%*%t(Support$`log SD`)*cov2cor(as.matrix(NatECovar[,-1])), method = "svd")))
  Support <- mutate(Support, `Adj Average` = exp(diag(rmvnorm(dim(Support)[1], mean = Wt.Ave * `Nat E Sim`, as.matrix(forceSymmetric(`log SD`%*%t(`log SD`)*cov2cor(as.matrix(CoVar[,-1])))), method = "svd")))) # Code is filthy because machine precision
  # Testing if sum(PV)>1
  if(sum(Support$`Adj Average`)>1){
    Support <- mutate(Support, `Adj Average` = `Adj Average`/sum(`Adj Average`))
  }
  Adj <- Support[,c(1,9)]
  assign("Adjusted Party Vote.df", Adj, envir = globalenv())
}

# Strong Transition Electorate Projector ----------------------------------

STM <- function(param, Electorates, Parties, Adj){
  Parties$`T Chng` <- Adj[,2] - Parties$`Total Vote`
  Parties$`Total Vote` <- Adj[,2]
  ChngT <- sum(abs(Parties$`T Chng`))/2

  Electorates <- arrange(Electorates,`Electoral District`, `Party`)
  Electorates$`Total Vote` <- Parties$`Total Vote`
  Electorates$`T Chng` <- Parties$`T Chng`

  # Loyalty of Weak voters (and strong if applicable)

  Parties <- mutate(Parties, Loyalty = ifelse(`T Chng`>0, 0, ifelse(`Prev Strong`>`Total Vote`,0,(`Total Vote`-`Prev Strong`)/`Prev Weak`)))
  Parties <- mutate(Parties, SLoyalty = ifelse(`Total Vote`>`Prev Strong`,1,`Total Vote`/`Prev Strong`))

  # Estimating Lost Voters
  i = 1
  Electorates <- mutate(Electorates, Lost = 0)
  while(i<dim(Electorates)[1]+1){
    Electorates[i,9] <- ifelse(as.numeric(Electorates[i,6])>0, 0, (1-as.numeric(filter(Parties, Party==as.character(Electorates[i,3]))[,7]))*as.numeric(Electorates[i,7])+(1-as.numeric(filter(Parties, Party==as.character(Electorates[i,3]))[,8]))*as.numeric(Electorates[i,8]))
    i = i +1
  }

  # Now adding voters

  i = 1
  j = 1 # counter for electorate + year combinations
  Electorates <- mutate(Electorates, Gained = 0)
  Electorates <- mutate(Electorates, Pred = 0)
  Electorates <- mutate(Electorates, Index = c(1:dim(Electorates)[1]))
  # adding an index so that when I filter the data frame I can know which rows need replacement
  Electorates <- Electorates[,c(12,1:11)]
  # reordering so index is the first row
  while(j<65){
    Gainz <- filter(Electorates, `Electoral District`==as.character(Electorates[i,2]))
    Remain <- sum(Gainz$Lost)
    m = 1
    while(m<dim(Gainz)[1]+1){
      if(Gainz[m,7]<=0){
        m=m+1
      } else{
        Gainz[m,11] <- as.numeric(Gainz[m,11]) + Remain*as.numeric(Gainz[m,7])/as.numeric(ChngT)
        m = m+1
      }
    }
    # Now calculating predicted values

    Gainz <- mutate(Gainz, Pred = `Prev Percent` - Lost + Gained)
    Electorates[c(as.numeric(Gainz[1,1]):as.numeric(Gainz[dim(Gainz)[1],1])),11] <- Gainz[,11]
    Electorates[c(as.numeric(Gainz[1,1]):as.numeric(Gainz[dim(Gainz)[1],1])),12] <- Gainz[,12]
    j = j+1
    i = as.numeric(Gainz[dim(Gainz)[1],1])+1 #skips to start of next electorate
  }
    assign("STM Preds", Electorates, envir = globalenv())
}

# Candidate Vote GLM Model Fit ------------------------------------------------------

CandidateB14.df <- filter(Candidate.df, Year<2015) # Training Set

i=1
while(i<=2228){
  if(CandidateB14.df[i,15]==TRUE){
    CandidateB14.df[i,7] <- CandidateB14.df[i,9]
  }
  i = i+1
}

NewCand.df <- filter(CandidateB14.df, is.na(`Vote to Use`))
RestB14.df <-filter(CandidateB14.df, !is.na(`Vote to Use`))

FirstTimers.fit1 <- glm(`Candidate Vote`~log(`Party Vote Electorate`)+Party, family = gaussian(log), data = NewCand.df[-c(221),])

Ohariu.df <- filter(RestB14.df, Electorate == "Ohariu")
Epsom.df <- filter(RestB14.df, Electorate == "Epsom" & Year > 2007)
RestB14.df <- setdiff(RestB14.df, Ohariu.df)
RestB14.df <- setdiff(RestB14.df, Epsom.df)

Wigram.df <- filter(RestB14.df, Electorate == "Wigram" & Year > 2001 & Year <2009)
RestB14.df <- setdiff(RestB14.df, Wigram.df)

Candidate.fit4 <- glm(`Candidate Vote`~log(`Party Vote Electorate`)*Party+log(`Vote to Use`) + `Incumbent`, family = quasi(log), data = RestB14.df[-c(1709, 1837),])

Ohariu.fit <- glm(`Candidate Vote`~log(`Party Vote Electorate`)+log(`Vote to Use`)+Incumbent, family = gaussian(log), data = Ohariu.df)

Epsom.df <- mutate(Epsom.df, ACT = FALSE)
Epsom.df[c(5,8,11),16] <- TRUE
Epsom.df <- mutate(Epsom.df, National = FALSE)
Epsom.df[c(3,9,13),17] <- TRUE
Epsom.fit <- glm(`Candidate Vote`~log(`Party Vote Electorate`)*(ACT+National)+log(`Vote to Use`), family = gaussian(log), data = Epsom.df)


# Candidate Vote Functions ------------------------------------------------

CandPredict <- function(Candidates, PartyVote, CandCovar, OhariuCovar){
  Candidates <- arrange(Candidates, Electorate, Party)
  i = 1
  while(i<=dim(Candidates)[1]){
    Candidates$`Party Vote Electorate`[i] <- as.numeric(filter(PartyVote, `Electoral District` == as.character(Candidates$Electorate[i]) & Party == as.character(Candidates$Party[i]))$Pred)
    i = i+1
  }
  Ohariu17 <- filter(Candidates, Electorate == "Ohariu")
  Candidates <- setdiff(Candidates, Ohariu17)

  Ohariu17 <- mutate(Ohariu17, Muu = predict.glm(Ohariu.fit, Ohariu17))
  Ohariu17 <- mutate(Ohariu17, SE = predict.glm(Ohariu.fit, Ohariu17, se.fit = TRUE)$se.fit)
  OhariuCovar <- as.matrix(OhariuCovar[,which(names(OhariuCovar)%in%OhariuCovar$Party)])
  Ohariu17 <- mutate(Ohariu17, Pred = diag(rmvnorm(dim(Ohariu17)[1], mean = Muu, sigma = SE%*%t(SE)*cov2cor(OhariuCovar), method = "svd")))

  Epsom17 <- filter(Candidates, Electorate == "Epsom")
  Candidates <- setdiff(Candidates, Epsom17)

  # ADD NAT AND ACT PARTY DUMMY
  Epsom17 <- mutate(Epsom17, Muu = predict.glm(Epsom.fit, Epsom17))
  Epsom17 <- mutate(Epsom17, SE = predict.glm(Epsom.fit, Epsom17, se.fit = TRUE)$se.fit)

  FirstTime17 <- filter(Candidates, is.na(`Vote to Use`))
  Candidates <- setdiff(Candidates, FirstTime17)

  FirstTime17 <- mutate(FirstTime17, Muu = predict.glm(FirstTimers.fit1, FirstTime17))
  FirstTime17 <- mutate(FirstTime17, SE = predict.glm(FirstTimers.fit1, FirstTime17, se.fit = TRUE)$se.fit)

  Candidates <- mutate(Candidates, Muu = predict.glm(Candidate.fit4, Candidates))
  Candidates <- mutate(Candidates, SE = predict.glm(Candidate.fit4, Candidates, se.fit = TRUE)$se.fit)

  Candidates <- rbind(Candidates, Epsom17, FirstTime17)
  Candidates <- arrange(Candidates, Electorate, Party)

  Electorate_List <- list(unique(Candidates$Electorate))
  Candidates <- mutate(Candidates, Pred = 0)
  j = 1
  i = 1
  while(j<=length(Electorate_List[[1]])){
    ElecPred <- filter(Candidates, Electorate == Electorate_List[[1]][j])
    ElecCovar <- filter(CandCoVar, Party %in% ElecPred$Party)
    ElecCovar <- as.matrix(ElecCovar[,which(names(ElecCovar)%in%ElecCovar$Party)])
    ElecPred <- mutate(ElecPred, Pred = diag(rmvnorm(dim(ElecPred)[1], Muu, as.matrix(forceSymmetric(SE%*%t(SE)*cov2cor(ElecCovar))), method = "svd")))
    Candidates$Pred[i:(i+dim(ElecPred)[1]-1)] <- ElecPred$Pred
    i = i+dim(ElecPred)[1]
    j = j+1
  }
  Candidates <- rbind(Candidates, Ohariu17)
  Candidates <- mutate(Candidates, Pred = exp(Pred))
  Candidates <- arrange(Candidates, Electorate, Party)
  assign("CandSim", Candidates, envir = globalenv())
}

# Maori Electorates -------------------------------------------------------

MEparam <- c(0.1932,0.4484,0.2084,4.084) # Optimal paramaters

MENatErrorPV <- function(MEPVNatE, CoVar){
  MEPVNatEsim <- mutate(MEPVNatE, MuuNatESim = rnorm(dim(NatESim)[1], Mean, SD))
  assign("MEPVNatESim", MEPVNatEsim, envir = globalenv())
}

MEDEPV <- function(Design){
  DesignSim <- mutate(Design, DesignSim = rnorm(dim(Design)[1], DE, DEsd))
  assign("MEDEPV", DesignSim, envir = globalenv())
}

MEPVWtAve <- function(Polls, Results, param){
  Polls <- arrange(Polls, Electorate)
  Types <- rep(0,7)
  i = 1
  while(i<=7){
    Types[i] <- dim(filter(Polls, Electorate == unique(Results$Electorate)[i]))[1]
    i = i+1
  }
  Results <- cbind(Results, Type = Types)

  c1wts <- rep(0,7)
  i = 1
  while(i<=7){
    if(Results$Type[i] > 0){
        c1wts[i] <- sum(log(filter(Polls, Electorate == unique(Results$Electorate)[i])$Size)/log(filter(Polls, Electorate == unique(Results$Electorate)[i])$`Days Before`))
    }
    i = i+1
  }
  Results <- cbind(Results, c1wt = c1wts)

  c2wts = rep(0,7)
  i = 1
  while(i<=7){
    c2wts[i] <- if_else(dim(filter(Polls, Electorate!=unique(Results$Electorate)[i]))[1]==0,0,log(dim(filter(Polls, Electorate!=unique(Results$Electorate)[i]))[1]))
    i = i+1
  }
  Results <- cbind(Results, c2wt = c2wts)

  # Now spreading Polls
  Polls <- gather(Polls, Party, `Party Vote`, 7:15) # or whatever should be

  # Calculating Wt Ave Electorate
  i = 1
  Results <- mutate(Results, `Weight Ave` = 0)
  while(i<=dim(Results)[1]){
    if(Results$Type[i]>0){
      if(Results$Type[i]==1){
        Results$`Weight Ave`[i] = as.numeric(filter(Polls, Party == Results$Party[i] & Electorate == Results$Electorate[i])$`Party Vote`)
      } else{
        # Assuming only max two polls - if three or more polls will need to edit
        Mult <- log(filter(Polls, Party== Results$Party[i] & Electorate == Results$Electorate[i])$`Days Before`[1]-filter(Polls, Party== Results$Party[i] & Electorate == Results$Electorate[i])$`Days Before`[2])*log(filter(Polls, Party== Results$Party[i] & Electorate == Results$Electorate[i])$`Size`[2])/log(filter(Polls, Party== Results$Party[i] & Electorate == Results$Electorate[i])$`Size`[1])
        Results$`Weight Ave`[i] = param[3]*Mult*filter(Polls, Party== Results$Party[i] & Electorate == Results$Electorate[i])$`Party Vote`[2]+(1-param[3]*Mult)*filter(Polls, Party== Results$Party[i] & Electorate == Results$Electorate[i])$`Party Vote`[1]
      }
    }
    i = i +1
  }

  # This is for calculating weighted average of other electorates
  Polls <- mutate(Polls, scaleF = log(Size)/log(`Days Before`))
  Polls <- mutate(Polls, nonscalePVO = `Party Vote`*scaleF)

  Polls$scaleF[Polls$Electorate == "full"] <- Polls$scaleF[Polls$Electorate == "full"]*param[4]
  Polls$nonscalePVO[Polls$Electorate == "full"] <- Polls$nonscalePVO[Polls$Electorate == "full"]*param[4]

  Results <- mutate(Results, `Weight Ave Other` = 0)
  i = 1
  while(i<=dim(Results)[1]){
    Results$`Weight Ave Other`[i] = sum(filter(Polls, Electorate != Results$Electorate[i]& Party == Results$Party[i])$nonscalePVO)/sum(filter(Polls, Electorate != Results$Electorate[i]& Party == Results$Party[i])$scaleF)
    i = i+1
  }
  Results <- mutate(Results, Pred = param[1]*c1wt/(c1wt+c2wt)*`Wt Ave Elec` + param[2]*c2wt/(c1wt+c2wt)*`Wt Ave Other Elec` + (1-param[1]*c1wt/(c1wt+c2wt)-param[2]*c2wt/(c1wt+c2wt))*`Prev Elec`)

  Results <- filter(Results, Party != "Undecided/Not Stated")

  i = 1
  j = 1
  k = 1
  while(i<=7){
    j = 1
    while(j<=4){
      Elec <- filter(Results, Electorate == unique(Results$Electorate)[i] & Year == 2002+j*3)
      Elec$Pred[which(Elec$Pred<0)] <- 0
      Pvt <- sum(Elec$Pred)
      Elec$Pred <- Elec$Pred/Pvt
      Results$Pred[k:(k+dim(Elec)[1]-1)] <- Elec$Pred
      k = k+dim(Elec)[1]
      j = j+1
    }
    i = i+1
  }

  assign("MEPVWtAve", Results, envir = globalenv())
}

MEPVSim <- function(WtAve, CoVar, NatE, DE, NatECovar){
  WtAve <- cbind(WtAve, MuuNatE = NatE$MuuNatESim)
  WtAVe <- mutate(WtAVe, Pred = 0)
  i = 1
  while(i<=7){
    ElecPred <- WtAve[c((9*i-8):(9*i)),]
    ElecPred <- cbind(ElecPred, DE = DE$DesignSim)
    ElecPred <- mutate(ElecPred, SD = DE*sqrt(WtAve*(1-WtAve)/500))
    ElecPred <- mutate(ElecPred, NatESim = diag(rmvnorm(dim(ElecPred)[1], MuuNatE, SD%*%t(SD)*cov2cor(NatECoVar[,-1]))))
    ElecPred <- mutate(ElecPred, Pred = diag(rmvnorm(dim(ElecPred)[1], WtAve-NatESim, SD%*%t(SD)*cov2cor(CoVar[,-1]))))
    WtAve$Pred[c((9*i-8):(9*i))] <- ElecPred$Pred
    i = i+1
  }
  assign("MEPVSim", SimPV, envir = globalenv())
}

# Maori Candidate GLMs ----------------------------------------------------

MECandidate.df <- filter(MECandidate.df, Year!=1996)
MENoPolls.df <- filter(MECandidate.df, Year<2003)
MENoPollsFT.df <- filter(MENoPolls.df, is.na(`Vote to Use`))
MENoPolls.df <- setdiff(MENoPolls.df, MENoPollsFT.df)

NoPoll.fit1 <- glm(`Candidate Vote`~log(`Vote to Use`)+log(`Party Vote Electorate`), family = gaussian(log), data = MENoPolls.df) # Incumbent not sig

NoPollFT.fit <- glm(`Candidate Vote`~log(`Party Vote Electorate`), family = gaussian(log), data = MENoPollsFT.df)

MEYesPolls.df <- read_csv("Maori Cand with PV.csv")
MEYesPollsFT.df <- filter(MEYesPolls.df, is.na(`Vote to Use`))
MEYesPolls.df <- setdiff(MEYesPolls.df, MEYesPollsFT.df)

MEYES.fit <- glm(`Candidate Vote`~log(`Vote to Use`) + log(`Party Vote Electorate`)+ log(`Wt Ave`)+Incumbent, family = gaussian(log), data = MEYesPolls.df)
MEYESFT.fit <- glm(`Candidate Vote`~log(`Party Vote Electorate`)+ log(`Wt Ave`), family = gaussian(log), data = MEYesPollsFT.df[-7,])

# Maori Electorate Candidate Votes ----------------------------------------

MEDEC <- function(Design){
  DesignSim <- mutate(Design, DesignSim = rnorm(dim(Design)[1], DE, DEsd))
  assign("MEDEC", DesignSim, envir = globalenv())
}

MENatErrorC <- function(MECNatE){
  MECNatEsim <- mutate(MECNatE, MuuNatESim = rnorm(dim(MECNatE)[1], Muu, SDev))
  assign("MECNatESim", MECNatEsim, envir = globalenv())
}

MECWtAve <- function(Polls, Cand, param){
  Polls <- mutate(Polls, WT = log(param[1]*Size)/(log(param[2]*`Days Before`)))
  Polls <- mutate(Polls, WTCV = WT*`Cand Vote`)
  Cand <- mutate(Cand, WtAve = 0)
  Polls <- arrange(Polls, Electorate, Year, Party)
  Cand <- arrange(Cand, Electorate, Year, Party)
  i = 1
  while(i<=dim(Cand)[1]){
    Cand$WtAve[i] <- sum(filter(Polls, Party == Cand$Party[i] & Electorate == Cand$Electorate[i] & Year == Cand$Year[i])$WTCV)/sum(filter(Polls, Party == Cand$Party[i] & Electorate == Cand$Electorate[i]& Year == Cand$Year[i])$WT)
    i = i+1
  }
  assign("MECWtAve", Cand, envir = globalenv())
}

MECProject <- function(Cand, WtAve, Covar, Design, PartyVote, NatE, NatECovar){
  PartyVote <- setdiff(PartyVote, Cand)
  PartyVote <- arrange(PartyVote, Electorate, Party)
  Cand <- arrange(Cand, Electorate, Party)
  Cand$`Party Vote Electorate` <- PartyVote$Pred
  Cand <- cbind(Cand, DE = Design$DesignSim)
  Cand <- cbind(Cand, WeightAve = WtAve)
  Cand <- mutate(Cand, SD = DE*sqrt(WeightAve*(1-WeightAve)/500))
  Cand <- mutate(Cand, PollAve = 0)
  NatEsim <- diag(rmvnorm(dim(NatE)[1], MuuNatESim, SD%*%t(SD)*cov2cor(NatECovar[,-1])))
}
# Admin Functions ---------------------------------------------------------

SaintLague <- function(PartyVote){
  SL <- PartyVote
  i = 1
  while(i<=199){
    SLv <- SL$`Adj Average`/(2*i+1)
    SL <- cbind(SL, SLv)
    i = i+1
  }

  SLs <- t(SL)
  SLs <- SLs[,-1]
  SLv <- as.vector(as.matrix(SLs))
  SLs <- SLS >= sort(SLv, partieal = length(SLv)-119)[length(SLv)-119]
  Seats <- cbind(Party = SL$Party, Seats = t(colSum(SLs)))
  assign("TotalSeatsSim", Seats, envir = globalenv())
}

ElectorateSeats <- function(Candidate, Seats, MaoriCand, MSeats, TSeats){
  i = 1
  j = 1
  while(i<=64){
    Elec <- filter(Candidate, Electorate == unique(Candidate$Electorates)[i])
    Elec <- mutate(Elec, Win = as.numeric(Pred == max(Pred)))
    Seats[j:(j+dim(Elec)[1]-1),6] <- Elec$Pred
    i = i+1
    j = j+dim(Elec)[1]
  }
  i = 1
  j = 1
  while(i<=7){
    Elec <- filter(MaoriCand, Electorate == unique(MaoriCand$Electorates)[i])
    Elec <- mutate(Elec, Win = as.numeric(Pred == max(Pred)))
    MSeats[j:(j+dim(Elec)[1]-1),6] <- Elec$Pred
    i = i+1
    j = j+dim(Elec)[1]
  }
  AllSeats <- rbind(Seats, MSeats)

  i = 1
  TSeats <- mutate(TSeats, Seats = 0)
  while(i<=9){
    TSeats$Seats[i] <-  colSums(filter(AllSeats, Party == unique(TSeats$Party)[i]))[6]
    i = i+1
  }
  assign("ElecSeatsSim", TSeats, envir = globalenv())
  assign("CandWinSim", AllSeats, envir = globalenv())
}

ListSeats <- function(TotalSeats, ElectorateSeats){
  ListSeats <- TotalSeats
  ListSeats <- cbins(ListSeats, ElecSeats = ElectorateSeats$Seats)
  ListSeats <- mutate(ListSeats, LSeats = ifelse(Seats - ElecSeats<0,0, Seats- ElecSeats))
  assign("ListSeatsSim", ListSeats, envir = globalenv())
}

TotalSeats <- function(ElectorateSeats, ListSeats){
  TotalSeats <- ElectorateSeats
  TotalSeats$Seats <- ElectorateSeats$Seats + ListSeats$Seats
  assign("FinalSeatSims", TotalSeats, envir = globalenv())
}

# Simulations -------------------------------------------------------------
DaysTo = 0
NSim = 1
MaxSims = 10000
while(NSim <= MaxSims ){
  # Calulating Nationwide Party Vote
  HouseEffect(House = HouseEffects.df, GEPolls = GEPolls.df)
  WeightAverage(GEPolls = GEPollsSim.df)
  DesignEffect(Design = DesignE.df)
  AdjustedAverage(NatE = NatPollE.df, Design = DesignSim.df, WtAve = WeightAveSim.df, Polls = GEPollsSim.df, NatECovar = NatECovar.df, CoVar = Covar.df)
  STM(param = c(0.321,0.1), Electorates = Electorate17Data.df, Parties = Party17.df,  Adj = `Adjusted Party Vote.df`)
  CandPredict(Candidates = Candidate17.df, PartyVote = `STM Preds`, Covar = CandCovar.df, OhariuCovar = OhariuCovar.df)

  # Maori Electorate

  MEDEPV(Design = MEDesignE.df)
  MENatErrorPV(MEPVNateE = MENatE.df, CoVar = MENateCoVar.df)
  MEPVWtAve(Polls = MEPartyPolls.df, Results = , param = MEparam)
  MEPVSim(WtAve = MEPVWtAve, CoVar = MEPVCovar.df, NatE = MEPVNatESim, DE = MEDEPV, NatECovar = MENateCoVar.df)

  MEDEC(Design = MEDesignECand.df)
  MENatErrorC(MECNatE = MENatECand.df)
  MECWtAve(Polls = MECPolls.df, Cand = MECandidate17.df, param = c(23.07,0.1547))
  MECProject(Cand = MECandidate17.df, WtAve = MECWtAve, Covar = MECCovar.df, Design = MEDEC, PartyVote = MEPVSim, NatE = MECNatESim, NatECovar = MENateCandCoVar.df)

  SaintLague(PartyVote = `Adjusted Party Vote.df`)
  ElectorateSeats(Candidate = CandSim, Seats = StoreCand.df, MaoriCand = MaoriCandSim, MSeats = StoreMCand.df, TSeats = StoreElecSeats.df)
  ListSeats(TotalSeats = TotalSeatsSim, ElectorateSeats = ElecSeatsSim)
  TotalSeats(ElectorateSeats = ElecSeatsSim, ListSeats = ListSeatsSim)
  # Store Files Addition
  StoreSeats.df <- cbind(StoreSeats.df, FinalSeatSims$Seats)
  StoreElecSeats.df <- cbind(StoreElecSeats.df, ElecSeatsSim$Seats)
  StoreListSeats.df <- cbind(StoreListSeats.df, ListSeatsSim$Seats)
  StorePV.df <- cbind(StorePV.df, `Adjust Party Vote.df`$`Adj Average`)
  StoreElecPV.df <- cbind(StoreElecPV.df, STMPreds$Pred)
  StoreMEPV.df <- cbind(StoreMEPV.df, MEPVSim$Pred)
  StoreCand.df <- cbind(StoreCand.df, CandSim$Pred)
  StoreCandWin.df <- cbind(StoreCandWin.df, CandWinSim$Win)
  StoreMECand.df <- cbind(StoreMECand.df, MECandSim$Pred)#

  NSim <- NSim+1
}

# Write Up ----------------------------------------------------------------

# The code below produces summary dfs which will be used in visualtion

lowr = 0.05
upr = 0.95 # you can edit these to get which ever range you desire default at 90

# Party Vote Summary ------------------------------------------------------

# Nationwide Party Vote - using median as middle value as believe its better but you can change to mean if you desire
TotalPVSum.df <- StorePV.df[,c(1:2)]
TotalPVSum.df <- mutate(TotalPVSum.df, `median` = median(StorePV.df[,c(3:(2+MaxSims))]))
TotalPVSum.df <- mutate(TotalPVSum.df, lowr = 0)
TotalPVSum.df <- mutate(TotalPVSum.df, upr = 0)
i = 1
while(i<=9){
  TotalPVSum.df[i,4] <- sort(StorePV.df[i,c(3:(2+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  TotalPVSum.df[i,5] <- sort(StorePV.df[i,c(3:(2+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

# Electorate Party Vote
ElectoratePVSum.df <- StoreElecPV.df[,c(1:4)]
ElectoratePVSum.df <- mutate(ElectoratePVSum.df, `median` = median(StoreElecPV.df[,c(5:(4+MaxSims))]))
ElectoratePVSum.df <- mutate(ElectoratePVSum.df, lowr = 0)
ElectoratePVSum.df <- mutate(ElectoratePVSum.df, upr = 0)
i = 1
while(i<=dim(ElectoratePVSum.df)[1]){
  ElectoratePVSum.df[i,4] <- sort(StoreElecPV.df[i,c(5:(4+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  ElectoratePVSum.df[i,5] <- sort(StoreElecPV.df[i,c(5:(4+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

# Maori Electorate Party Vote
MEPVSum.df <- StoreMEPV.df[,c(1:4)]
MEPVSum.df <- mutate(MEPVSum.df, `median` = median(StoreMEPV.df[,c(5:(4+MaxSims))]))
MEPVSum.df <- mutate(MEPVSum.df, lowr = 0)
MEPVSum.df <- mutate(MEPVSum.df, upr = 0)
i = 1
while(i<=dim(MEPVSum.df)[1]){
  MEPVSum.df[i,4] <- sort(StoreMEPV.df[i,c(5:(4+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  MEPVSum.df[i,5] <- sort(StoreMEPV.df[i,c(5:(4+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

# Candidate Vote Summary --------------------------------------------------

CandSum.df <- StoreCand.df[,c(1:5)]

CandSum.df <- mutate(CandSum.df, `median` = median(StoreCand.df[,c(6:(5+MaxSims))]))
CandSum.df <- mutate(CandSum.df, lowr = 0)
CandSum.df <- mutate(CandSum.df, upr = 0)
i = 1
while(i<=dim(CandSum.df)[1]){
  CandSum.df[i,4] <- sort(StoreCand.df[i,c(6:(5+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  CandSum.df[i,5] <- sort(StoreCand.df[i,c(6:(5+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

MECandSum.df <- StoreMECand.df[,c(1:5)]

MECandSum.df <- mutate(MECandSum.df, `median` = median(StoreMECand.df[,c(6:(5+MaxSims))]))
MECandSum.df <- mutate(MECandSum.df, lowr = 0)
MECandSum.df <- mutate(MECandSum.df, upr = 0)
i = 1
while(i<=dim(MECandSum.df)[1]){
  MECandSum.df[i,4] <- sort(StoreMECand.df[i,c(6:(5+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  MECandSum.df[i,5] <- sort(StoreMECand.df[i,c(6:(5+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

AllCandSum <- rbind(CandSum.df, MECandSum.df)
AllCandSum <- mutate(AllCandSum, `% Win` = rowSums(StoreCandWin.df[,c(6:(5+MaxSims))])/MaxSims)


# Seat Summary ------------------------------------------------------------

SeatsSum.df <- StoreSeats.df[,c(1:2)]
SeatsSum.df <- mutate(SeatsSum.df, `median` = median(StoreSeats.df[,c(3:(2+MaxSims))]))
SeatsSum.df <- mutate(SeatsSum.df, lowr = 0)
SeatsSum.df <- mutate(SeatsSum.df, upr = 0)
i = 1
while(i<=9){
  SeatsSum.df[i,4] <- sort(StoreSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  SeatsSum.df[i,5] <- sort(StoreSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

ElecSeatsSum.df <- StoreElecSeats.df[,c(1:2)]
ElecSeatsSum.df <- mutate(ElecSeatsSum.df, `median` = median(StoreElecSeats.df[,c(3:(2+MaxSims))]))
ElecSeatsSum.df <- mutate(ElecSeatsSum.df, lowr = 0)
ElecSeatsSum.df <- mutate(ElecSeatsSum.df, upr = 0)
i = 1
while(i<=9){
  ElecSeatsSum.df[i,4] <- sort(StoreElecSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  ElecSeatsSum.df[i,5] <- sort(StoreElecSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

ListSeatsSum.df <- StoreListSeats.df[,c(1:2)]
ListSeatsSum.df <- mutate(ListSeatsSum.df, `median` = median(StoreListSeats.df[,c(3:(2+MaxSims))]))
ListSeatsSum.df <- mutate(ListSeatsSum.df, lowr = 0)
ListSeatsSum.df <- mutate(ListSeatsSum.df, upr = 0)
i = 1
while(i<=9){
  ListSeatsSum.df[i,4] <- sort(StoreListSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  ListSeatsSum.df[i,5] <- sort(StoreListSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}
# Misc Summary ------------------------------------------------------------

FivePercent.df <- data.frame(Party = StorePV.df$Party, `% > 5%` = rowSums(StorePV.df[,c(3:(2+MaxSims))]>=0.05))
HaveList.df <- data.frame(Party = StoreListSeats.df$Party, `% List` = rowSums(StoreListSeats.df[,c(3:(2+MaxSims))]>=1))

# Previous Summary Files --------------------------------------------------

setwd() # Set to wherever storing

# Add file names in here

PrevPVSum.df <- read_csv()
PrevElecPVSum.df <- read_csv()
PrevMEPVSum.df <- read_csv()
PrevCandSum.df <- read_csv()
PrevSeatSum.df <- read_csv()
PrevElecSeatSum.df <- read_csv()
PrevListSeatSum.df <- read_csv()
PrevFive.df <- read_csv()
PrevHaveList.df <- read_csv

# Change Summary ----------------------------------------------------------

ChangePVSum.df <- TotalPVSum.df
ChangePVSum.df[,c(3:5)] <- ChangePVSum.df[,c(3:5)] - PrevPVSum.df[,c(3:5)]
ChangeElecPVSum.df <- ElectoratePVSum.df
ChangeElecPVSum.df[,c(5:7)] <- ChangeElecPVSum.df[,c(5:7)] - PrevElecPVSum.df[,c(5:7)]
ChangeMEPVSum.df <- MEPVSum.df
ChangeMEPVSum.df[,c(5:7)] <- ChangeMEPVSum.df[,c(5:7)] - PrevMEPVSum.df[,c(5:7)]
ChangeCandSum.df <- AllCandSum
ChangeCandSum.df[,c(6:9)] <- ChangeCandSum.df[,c(6:9)] - PrevCandSum.df[,c(6:9)]
ChangeSeatSum.df <- SeatsSum.df
ChangeSeatSum.df[,c(3:5)] <- ChangeSeatSum.df[,c(3:5)] - PrevSeatSum.df[,c(3:5)]
ChangeElecSeatSum.df <- ElecSeatsSum.df
ChangeElecSeatSum.df[,c(3:5)] <- ChangeElecSeatSum.df[,c(3:5)] - PrevElecSeatSum.df[,c(3:5)]
ChangeListSeatSum.df <- ListSeatsSum.df
ChangeListSeatSum.df[,c(3:5)] <- ChangeListSeatSum.df[,c(3:5)] - PrevListSeatSum.df[,c(3:5)]
ChangeFive.df <- FivePercent.df
ChangeFive.df[,2] <- ChangeFive.df[,2] - PrevFive.df[,2]
ChangeHaveList.df <- HaveList.df
ChangeHaveList.df[,2] <- ChangeHaveList.df[,2] - PrevHaveList.df[,2]

# csv Writers -------------------------------------------------------------

setwd() # Set to where ever desire writing

write.csv(TotalPVSum.df, "Simulated PV.csv")
write.csv(ChangePVSum.df, "Change PV.csv")
write.csv(ElectoratePVSum.df, "Simulated Electorate PV.csv")
write.csv(ChangeElecPVSum.df, "Change Electorate PV.csv")
write.csv(MEPVSum.df, "Simulated ME PV.csv")
write.csv(ChangeMEPVSum.df, "Change ME PV.csv")
write.csv(AllCandSum, "Simulated Candidate.csv")
write.csv(ChangeCandSum.df, "Change Candidate.csv")
write.csv(SeatsSum.df, "Simulated Seats.csv")
write.csv(ChangeSeatSum.df, "Change Seats.csv")
write.csv(ElecSeatsSum.df, "Simulated Elec Seats.csv")
write.csv(ChangeElecSeatSum.df, "Change Elec Seats.csv")
write.csv(ListSeatsSum.df, "Simulated List Seats.csv")
write.csv(ChangeListSeatSum.df, "Change List Seats.csv")
write.csv(FivePercent.df, "Simulated Five Threshold.csv")
write.csv(ChangeFive.df, "Change Five Threshold.csv")
write.csv(HaveList.df, "Simulated Have List.csv")
write.csv(ChangeHaveList.df, "Change Have List.csv")
