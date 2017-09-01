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
library(mgcv)
library(mvtnorm)
library(Matrix)

# Data --------------------------------------------------------------------
GEPolls.df <- read_csv("PollsForGam.csv") #Ensure get updated
source("config.R")
UsePollsWithin100Days <- DaysTo < 70
# setwd("~/Election Prediction Model/GIT 2017")

HouseEffects.df <- read_csv("../common/WtAve House Effects 17.csv")
Electorate17Data.df <- read_csv("../common/Electorate Data for STM FOR 17.csv")
Party17.df <- read_csv("../common/Post Party for 17.csv")
Candidate.df <- read_csv("../common/General Election Candidate Data for 14.csv") # This is used to create the glms
Candidate17.df <- read_csv("../common/Candidate 17 DataFrame.csv")
# MEPartyVote.df <- read_csv("ME Party Polling Data 17.csv")
DesignE.df <- read_csv("../common/Design Effects pred 17.csv")
NatPollE.df <- read_csv("../common/Nat Error 17 pred.csv")
NatECovar.df <- read_csv("../common/Nat Poll E CoVariance 17 Pred.csv")
Covar.df <- read_csv("../common/CoVariance ln 17 Pred.csv")
OhariuCovar.df <- read_csv("../common/Ohariu CoVar 17.csv")
CandCovar.df <- read_csv("../common/Candidate CoVariance 17 Pred.csv")
MEDesignE.df <- read_csv("../common/ME Party DE.csv")
MENatE.df <- read_csv("../common/MEPVNatPolling Error.csv")
MENateCoVar.df <- read_csv("../common/MEPV Covariance.csv")
MEDesignECand.df <- read_csv("../common/ME Candidate DE.csv")
MENatECand.df <- read_csv("../common/MECNatPolling Error.csv")
MENateCandCoVar.df <- read_csv("../common/MEC NatE Covariance.csv")
MEPVCovar.df <- read_csv("../common/MEPV Covariance 17 Pred.csv")
MECCovar.df <- read_csv("../common/MEC GLM Covar for 17.csv")
MECandidate.df <- read_csv("../common/Maori Electorate Candidate Data.csv")
MECandidate17.df <- read_csv("../common/Maori Candidate 17.csv")
MECPolls.df <- read_csv("../common/Candidate Polling 17.csv") # Replace and update
MEPartyPolls.df <- read_csv("../common/ME Party Polling Data 17.csv") # Replace and update
MEResults.df <- read_csv("../common/ME Results.csv")
MEPVPrePolls.df <- read_csv("../common/MEPVforPrePolls.csv")
StoreSeats.df <- read_csv("../common/Store Seats.csv")
StoreElecSeats.df <- read_csv("../common/Store Elec Seats.csv")
StoreListSeats.df <- read_csv("../common/Store List Seats.csv")
StorePV.df <- read_csv("../common/Store PV.csv")
StoreElecPV.df <- read_csv("../common/Store Elec PV.csv")
StoreMEPV.df <- read_csv("../common/Store ME Elec PV.csv")
StoreCand.df <- read_csv("../common/Store Cand.csv")
StoreMECand.df <- read_csv("../common/Store ME Cand.csv")
StoreCand.df <- arrange(StoreCand.df, Electorate, Party)
StoreMECand.df <- arrange(StoreMECand.df, Electorate, Party)
StoreCandWin.df <- rbind(StoreCand.df, StoreMECand.df)
StoreCand1.df <- StoreCand.df
StoreMECand1.df <- StoreMECand.df
StoreElecSeats1.df <- StoreElecSeats.df


# Weighted Polling Average ------------------------------------------------

# Function estimating and applying house effect to individuals polls
HouseEffect <- function(House, GEPolls){
  House <- mutate(House, SimEst = rnorm(dim(House)[1],Estimate,StdE)+rnorm(dim(House)[1],Baseline,BaseStdE))
  House <- House[,-c(3:6)]
  House <- spread(House, Party, SimEst)
  PollInclude = unique(House$Pollster)
  i = 1
  while(i<=dim(GEPolls)[1]){
    if(GEPolls[i,1] %in% PollInclude){
      GEPolls[i,4:12] <- GEPolls[i,4:12] -  filter(House, Pollster  == as.character(GEPolls[i,1]))[,2:10]
    }
    i = i+1
  }
  GEPolls[,4:12][GEPolls[,4:12]<0.0005] <- 0.0005
  assign("GEPollsSim.df", GEPolls, envir = globalenv())
}

# Function estimating Design Effect
DesignEffect <- function(Design){
  Design <- mutate(Design, DESim = rnorm(dim(Design)[1],DE,DEsd))
  assign("DesignSim.df", Design, envir = globalenv())
}

# Function Calculating Weighted Average
WeightAverage <- function(GEPolls){
  GEPolls <- filter(GEPolls, Pollster!= "Election result")
  GEPolls <- filter(GEPolls, `Release Days`<100)
  GEPolls <- mutate(GEPolls, RawWeight = exp(-log(2)*(`Days Before`-DaysTo)/(1.96+0.2*DaysTo)))
  GEPolls <- mutate(GEPolls, Weight = RawWeight/sum(RawWeight))
  GEPolls[,4:12] <- as.data.frame(apply(GEPolls[,4:12],2,function(x) x*GEPolls[,14]))
  WeightAve <- data.frame(Party = colnames(GEPolls)[4:12], Wt.Ave = colSums(GEPolls[,c(4:12)]))
  WeightAve$Wt.Ave <- log(WeightAve$Wt.Ave)
  assign("WeightAveSim.df", WeightAve, envir = globalenv())
}

TrendLineCalc <- function(GEPolls){
  Party_list <- list(colnames(GEPolls[,c(4:dim(GEPolls)[2])]))
  Muu = rep(0,dim(GEPolls)[2]-3)
  SE = rep(0,dim(GEPolls)[2]-3)
  CurMuu = rep(0,dim(GEPolls)[2]-3)
  CurSE = rep(0,dim(GEPolls)[2]-3)
  i = 1
  while(i<=length(Party_list[[1]])){
    Alpha <- select(GEPolls, contains(Party_list[[1]][i]))
    Beta <- data.frame(Poll = GEPolls$Pollster, Vote = Alpha, Day = rep(max(GEPolls$`Days Before`), dim(Alpha)[1]) - GEPolls$`Days Before`)
    Party.fit <- gam(log(Beta[,2])~s(as.numeric(Day)), data = Beta, weights = ifelse(Poll=="Election result", log((2286190+2356536+2257336+2405620)/4), 1))
    Muu[i] <- predict.gam(Party.fit, newdata = data.frame(Pollster = "Election Result", Day = max(GEPolls$`Days Before`)))
    SE[i] <- predict.gam(Party.fit, newdata = data.frame(Pollster = "Election Result", Day = max(GEPolls$`Days Before`)), se.fit = TRUE)$se.fit
    CurMuu[i] <- predict.gam(Party.fit, newdata = data.frame(Pollster = "Election Result", Day = max(Beta$Day)))
    CurSE[i] <- predict.gam(Party.fit, newdata = data.frame(Pollster = "Election Result", Day = max(Beta$Day)), se.fit = TRUE)$se.fit
    i = i+1
  }
  Diff = if (UsePollsWithin100Days) {
    Diff = (rnorm(length(Muu), Muu, SE) - rnorm(length(CurMuu), CurMuu, CurSE))/log(min(GEPolls$`Days Before`)+1)
  } else {
    rnorm(length(Muu), Muu, SE)
  }
  assign("TrendAdjSim", Diff, envir = globalenv())
}

# Function estimating mean national polling error, and national polling error for the sim before applying it to the weighted average to get the adjusted polling average
AdjustedAverage <- function(NatE, Design, WtAve, Polls, NatECovar, CoVar, TrendAdj){
  NatE <- mutate(NatE, NatEMuuSim = rnorm(dim(NatE)[1], Muu, SD))
  Support <- cbind(WtAve, MuuNatE = NatE$NatEMuuSim)
  if (UsePollsWithin100Days) {
    Support$Wt.Ave <- Support$Wt.Ave + TrendAdj
    Support$Wt.Ave <- pmin(Support$Wt.Ave,0)
    Support <- mutate(Support, NomSD = sqrt(exp(Wt.Ave)*(1-exp(Wt.Ave))/(log(dim(Polls)[1]+1)*1000)))
  } else {
    Support$Wt.Ave = pmin(TrendAdj,0)
    Support <- mutate(Support, NomSD = sqrt(exp(Wt.Ave)*(1-exp(Wt.Ave))/(log(3)*1000)))
  }
  Support <- mutate(Support, NomSD = sqrt(exp(Wt.Ave)*(1-exp(Wt.Ave))/(log(dim(Polls)[1]+1)*1000)))
  Support <- cbind(Support, DESim = sqrt(Design$DESim) )
  Support <- mutate(Support, NatESD = NomSD*DESim)
  Support <- mutate(Support, logSD = sqrt(Wt.Ave^2*(exp(NatESD^2)-1)))
  Support <- cbind(Support, NatESim = diag(rmvnorm(dim(Support)[1],Support$MuuNatE, Support$logSD%*%t(Support$logSD)*cov2cor(as.matrix(NatECovar[,-1])), method = "svd")))
  Support <- mutate(Support, Adj.Average = exp(Wt.Ave * NatESim))
  if(sum(Support$Adj.Average)>1){
    Support <- mutate(Support, Adj.Average = Adj.Average/sum(Adj.Average))
  }
  if(sum(Support$Adj.Average)<0.95){
    Support <- mutate(Support, Adj.Average = Adj.Average*0.95/sum(Adj.Average))
  }
  Adj <- Support[,c(1,9)]
  assign("AdjustedPartyVote.df", Adj, envir = globalenv())
}

# Strong Transition Electorate Projector ----------------------------------

# Function which calculates lost voters feed to apply in STM Projector
LostFunc <- function(Electorate){
  if_else(Electorate$T.Chng>0,0,(1-Electorate$Loyalty)*Electorate$Prev.Weak+(1-Electorate$SLoyalty)*Electorate$Prev.Strong)
}

STM <- function(param, Electorates, Parties, Adj){
  Parties$T.Chng <- Adj[,2] - Parties$Total.Vote
  Parties$Total.Vote <- Adj[,2]
  ChngT <- sum(abs(Parties$T.Chng))/2
  
  Electorates <- arrange(Electorates,Electorate, `Party`)
  Electorates$Total.Vote <- Parties$Total.Vote
  Electorates$T.Chng <- Parties$T.Chng
  
  Parties <- mutate(Parties, Loyalty = ifelse(T.Chng>0, 0, ifelse(Prev.Strong>Total.Vote,0,(Total.Vote-Prev.Strong)/Prev.Weak)))
  Parties <- mutate(Parties, SLoyalty = ifelse(Total.Vote>Prev.Strong,1,Total.Vote/Prev.Strong))
  
  Electorates <- cbind(Electorates, Loyalty = Parties$Loyalty)
  Electorates <- cbind(Electorates, SLoyalty = Parties$SLoyalty)
  Electorates <- mutate(Electorates, Lost = LostFunc(Electorate = Electorates))
  Electorates <- Electorates %>% group_by(Electorate) %>% mutate(Gained = if_else(T.Chng>0,T.Chng*sum(Lost)/ChngT,0))
  Electorates <- mutate(Electorates, Pred  = Prev.Percent - Lost + Gained)
  Electorates$Pred[Electorates$Pred<=0.0005] <- 0.0005
  assign("STMPreds", Electorates, envir = globalenv())
}

# Candidate Vote GLM Model Fit ------------------------------------------------------

i=1
while(i<=2228){
  if(Candidate.df[i,15]==TRUE){
    Candidate.df[i,7] <- Candidate.df[i,9]
  }
  i = i+1
}

NewCand.df <- filter(Candidate.df, is.na(Vote.to.Use))
RestCand.df <-filter(Candidate.df, !is.na(Vote.to.Use))

FirstTimers.fit <- glm(Candidate.Vote~log(Party.Vote.Electorate)+Party, family = gaussian(log), data = NewCand.df[-c(221),])

Ohariu.df <- filter(RestCand.df, Electorate == "Ohariu")
Epsom.df <- filter(RestCand.df, Electorate == "Epsom" & Year > 2007)
RestCand.df <- setdiff(RestCand.df, Ohariu.df)
RestCand.df <- setdiff(RestCand.df, Epsom.df)

Wigram.df <- filter(RestCand.df, Electorate == "Wigram" & Year > 2001 & Year <2009)
RestCand.df <- setdiff(RestCand.df, Wigram.df)

Candidate.fit <- glm(Candidate.Vote~log(Party.Vote.Electorate)*Party+log(Vote.to.Use) + `Incumbent`, family = quasi(log), data = RestCand.df[-c(1709, 1837),])

Ohariu.fit <- glm(Candidate.Vote~log(Party.Vote.Electorate)+log(Vote.to.Use)+Incumbent, family = gaussian(log), data = Ohariu.df)

Epsom.df <- mutate(Epsom.df, ACT = FALSE)
Epsom.df[c(5,8,11),16] <- TRUE
Epsom.df <- mutate(Epsom.df, National = FALSE)
Epsom.df[c(3,9,13),17] <- TRUE
Epsom.fit <- glm(Candidate.Vote~log(Party.Vote.Electorate)*(ACT+National)+log(Vote.to.Use), family = gaussian(log), data = Epsom.df)


# Candidate Vote Functions ------------------------------------------------

CandPredict <- function(Candidates, PartyVote, CandCovar, OhariuCovar){
  Candidates <- arrange(Candidates, Electorate, Party)
  PartyVote <- semi_join(PartyVote, Candidates, by = c("Electorate", "Party"))
  Candidates$Party.Vote.Electorate <- PartyVote$Pred 
 
  Epsom17 <- filter(Candidates, Electorate == "Epsom")
  Candidates <- setdiff(Candidates, Epsom17)
  
  Epsom17 <- mutate(Epsom17, ACT = c(TRUE, rep(FALSE, dim(Epsom17)[1]-1)))
  Epsom17 <- mutate(Epsom17, National = c(FALSE))
  Epsom17[Epsom17$Party=="National Party",10] <- TRUE
  Epsom17 <- mutate(Epsom17, Muu = predict.glm(Epsom.fit, Epsom17))
  Epsom17 <- mutate(Epsom17, SE = predict.glm(Epsom.fit, Epsom17, se.fit = TRUE)$se.fit)
  
  FirstTime17 <- filter(Candidates, is.na(Vote.to.Use))
  Candidates <- setdiff(Candidates, FirstTime17)
  
  FirstTime17 <- mutate(FirstTime17, Muu = predict.glm(FirstTimers.fit, FirstTime17))
  FirstTime17 <- mutate(FirstTime17, SE = predict.glm(FirstTimers.fit, FirstTime17, se.fit = TRUE)$se.fit)
  
  Candidates <- mutate(Candidates, Muu = predict.glm(Candidate.fit, Candidates))
  Candidates <- mutate(Candidates, SE = sqrt(predict.glm(Candidate.fit, Candidates, se.fit = TRUE)$se.fit^2+predict.glm(Candidate.fit, Candidates, se.fit = TRUE)$residual.scale^2))

  
  Epsom17 <- Epsom17[,-c(9:10)]
  Candidates <- rbind(Candidates, Epsom17, FirstTime17)
  Candidates <- arrange(Candidates, Electorate, Party)
  
  Electorate_List <- list(unique(Candidates$Electorate))
  Candidates <- mutate(Candidates, Pred = 0)
  j = 1
  i = 1
  while(j<=length(Electorate_List[[1]])){
    ElecPred <- filter(Candidates, Electorate == Electorate_List[[1]][j])
    ElecCovar <- filter(CandCovar, Party %in% ElecPred$Party)
    ElecCovar <- as.matrix(ElecCovar[,which(names(ElecCovar)%in%ElecCovar$Party)])
    ElecPred <- mutate(ElecPred, Pred = diag(rmvnorm(dim(ElecPred)[1], Muu, as.matrix(forceSymmetric(SE%*%t(SE)*cov2cor(ElecCovar))), method = "svd")))
    ElecPred$Pred <- exp(ElecPred$Pred)
    if(sum(ElecPred$Pred)>1){
      ElecPred$Pred <- ElecPred$Pred/sum(ElecPred$Pred)
      } else {
      if(sum(ElecPred$Pred)<0.95){
      ElecPred$Pred <- 0.95*ElecPred$Pred/sum(ElecPred$Pred)
        }
    }
    Candidates$Pred[i:(i+dim(ElecPred)[1]-1)] <- ElecPred$Pred
    i = i+dim(ElecPred)[1]
    j = j+1
  }
  Candidates <- arrange(Candidates, Electorate, Party)
  assign("CandSim", Candidates, envir = globalenv())
}

# Maori Electorates -------------------------------------------------------

MEparam <- c(0.1932,0.4484,0.2084,4.084) # Optimal paramaters

MENatErrorPV <- function(MEPVNatE){
  MEPVNatEsim <- mutate(MEPVNatE, MuuNatESim = rnorm(dim(MEPVNatE)[1], Mean, SDev))
  assign("MEPVNatESim", MEPVNatEsim, envir = globalenv())
}

MEDEPV <- function(Design){
  DesignSim <- mutate(Design, DesignSim = rnorm(dim(Design)[1], DE, DEsd))
  assign("MEDEPVsim", DesignSim, envir = globalenv())
}

MEPVWtAve <- function(Polls, Results, param){
  Results <- arrange(Results, Party, Electorate)
  Types <- rep(0,7)
  i = 1
  Electorate_List <- unique(filter(Results, Electorate != "Full")$Electorate)
  while(i<=7){
    Types[i] <- dim(filter(Polls, Electorate == Electorate_List[i]))[1]
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
  Polls <- gather(Polls, Party, `Party Vote`, 7:c(dim(Polls)[2]))
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
  Results <- mutate(Results, Pred = param[1]*c1wt/(c1wt+c2wt)*`Weight Ave` + param[2]*c2wt/(c1wt+c2wt)*`Weight Ave Other` + (1-param[1]*c1wt/(c1wt+c2wt)-param[2]*c2wt/(c1wt+c2wt))*`Prev Elec`)
  
  Results <- filter(Results, Party != "Undecided/Not Stated")
  
  i = 1
  j = 1
  k = 1
  while(i<=7){
    j = 1
      Elec <- filter(Results, Electorate == unique(Results$Electorate)[i])
      Elec$Pred[which(Elec$Pred<0)] <- 0
      Pvt <- sum(Elec$Pred)
      Elec$Pred <- Elec$Pred/Pvt
      Results$Pred[k:(k+dim(Elec)[1]-1)] <- Elec$Pred
      k = k+dim(Elec)[1]
    i = i+1
  }
  
  assign("MEPVWtAvesim", Results, envir = globalenv())
}

MEPVfunc <- function(WtAve, Polls, NatE, DE, NatECovar){
  WtAve <- arrange(WtAve, Electorate, Party)
  WtAve <- filter(WtAve, Party != "Independent/Other")
  WtAve <- cbind(WtAve, MuuNatE = NatE$MuuNatESim)
  WtAVe <- mutate(WtAve, Pred = 0)
  i = 1
  while(i<=7){
    ElecPred <- WtAve[c((9*i-8):(9*i)),]
    ElecPred <- cbind(ElecPred, DE = DE$DesignSim)
    ElecPred <- mutate(ElecPred, SD = DE*sqrt(Pred*(1-Pred)/(log(dim(Polls)[1]+1)*500)))
    ElecPred <- mutate(ElecPred, NatESim = diag(rmvnorm(dim(ElecPred)[1], MuuNatE, SD%*%t(SD)*cov2cor(as.matrix(NatECovar[,-1])), method ="svd")))
    ElecPred <- mutate(ElecPred, Pred = Pred-NatESim)
    ElecPred[ElecPred$Pred<0,]$Pred <- runif(dim(ElecPred[ElecPred$Pred<0,])[1],0,0.005)
    ElecPred$Pred <- ElecPred$Pred/sum(ElecPred$Pred)
    WtAve$Pred[c((9*i-8):(9*i))] <- ElecPred$Pred
    i = i+1
  }
  assign("MEPVSim", WtAve, envir = globalenv())
}

MEPVfuncNoPolls <- function(WtAve, Polls, NatE, DE, NatECovar){
  WtAve <- arrange(WtAve, Electorate, Party)
  WtAve <- filter(WtAve, Party != "Independent/Other")
  WtAve <- cbind(WtAve, MuuNatE = NatE$MuuNatESim)
  WtAve <- mutate(WtAve, Pred = 0)
  i = 1
  while(i<=7){
    ElecPred <- WtAve[c((9*i-8):(9*i)),]
    ElecPred <- cbind(ElecPred, DE = DE$DesignSim)
    ElecPred <- mutate(ElecPred, SD = DE*sqrt(FWtAve*(1-FWtAve)/500))
    ElecPred <- mutate(ElecPred, NatESim = diag(rmvnorm(dim(ElecPred)[1], MuuNatE, SD%*%t(SD)*cov2cor(as.matrix(NatECovar[,-1])), method ="svd")))
    ElecPred <- mutate(ElecPred, Pred = FWtAve-NatESim)
    ElecPred[ElecPred$Pred<0,]$Pred <- runif(dim(ElecPred[ElecPred$Pred<0,])[1],0,0.005)
    ElecPred$Pred <- ElecPred$Pred/sum(ElecPred$Pred)
    WtAve$Pred[c((9*i-8):(9*i))] <- ElecPred$Pred
    i = i+1
  }
  assign("MEPVSim", WtAve, envir = globalenv())
}


# Maori Candidate GLMs ----------------------------------------------------

MECandidate.df <- filter(MECandidate.df, Year!=1996)
MENoPolls.df <- filter(MECandidate.df, Year<2003)
MENoPollsFT.df <- filter(MENoPolls.df, is.na(`Vote.to.Use`))
MENoPolls.df <- setdiff(MENoPolls.df, MENoPollsFT.df)

NoPoll.fit1 <- glm(Candidate.Vote~log(Vote.to.Use)+log(Party.Vote.Electorate), family = gaussian(log), data = MENoPolls.df)

NoPollFT.fit <- glm(Candidate.Vote~log(Party.Vote.Electorate), family = gaussian(log), data = MENoPollsFT.df)

MEYesPolls.df <- read_csv("../common/Maori Cand with PV.csv")
MEYesPollsFT.df <- filter(MEYesPolls.df, is.na(`Vote.to.Use`))
MEYesPolls.df <- setdiff(MEYesPolls.df, MEYesPollsFT.df)

MEYES.fit <- glm(Candidate.Vote~log(Vote.to.Use) + log(Party.Vote.Electorate)+ log(PollAve)+Incumbent, family = gaussian(log), data = MEYesPolls.df)
MEYESFT.fit <- glm(Candidate.Vote~log(Party.Vote.Electorate)+ log(PollAve), family = gaussian(log), data = MEYesPollsFT.df[-7,])

# Maori Electorate Candidate Votes ----------------------------------------

MEDEC <- function(Design){
  DesignSim <- mutate(Design, DesignSim = rnorm(dim(Design)[1], DE, DEsd))
  assign("MEDECsim", DesignSim, envir = globalenv())
}

MENatErrorC <- function(MECNatE){
  MECNatEsim <- mutate(MECNatE, MuuNatESim = rnorm(dim(MECNatE)[1], Muu, SDev))
  assign("MECNatESim", MECNatEsim, envir = globalenv())
}

MECWtAve <- function(Polls, Cand, param){
  Polls <- gather(Polls, Party, `Cand Vote`, 8:20)
  Polls <- mutate(Polls, WT = log(param[1]*Size)/(log(param[2]*`Days Before`)))
  Polls <- mutate(Polls, WTCV = WT*`Cand Vote`)
  Cand <- mutate(Cand, WtAve = 0)
  Polls <- arrange(Polls, Electorate, Year, Party)
  Cand <- arrange(Cand, Electorate, Year, Party)
  i = 1
  while(i<=dim(Cand)[1]){
    Cand$WtAve[i] <- sum(filter(Polls, Party == Cand$Party[i] & Electorate == Cand$Electorate[i])$WTCV)/sum(filter(Polls, Party == Cand$Party[i] & Electorate == Cand$Electorate[i])$WT)
    i = i+1
  }
  assign("MECWtAvesim", Cand, envir = globalenv())
}

MECProject <- function(Cand, WtAve, Covar, Design, PartyVote, NatE, NatECovar){
  PartyVote <- semi_join(PartyVote, Cand,  by = c("Party", "Electorate"))
  PartyVote <- arrange(PartyVote, Electorate, Party)
  Cand <- arrange(Cand, Electorate, Party)
  Cand$`Party Vote Electorate` <- PartyVote$Pred
  Cand <- cbind(Cand, WeightAve = WtAve$WtAve)
  Cand <- mutate(Cand, DE = 0)
  i = 1
  while(i<=dim(Cand)[1]){
    Cand$DE[i] <- filter(Design, Party == Cand$Party[i])$DesignSim
    i = i+1
  }
  Cand <- mutate(Cand, SD = DE*sqrt(WeightAve*(1-WeightAve)/500))
  Cand <- mutate(Cand, PollAve = 0)
  SD <- c(0,0,0,0)
  i = 1
  while(i<=length(unique(Cand$Party))){
    SD[i] <- base::mean(!is.na(filter(Cand, Party == unique(Cand$Party)[i])$SD))
    i = i+1
  }
  NatE <- mutate(NatE, NatESim = diag(rmvnorm(dim(NatE)[1], MuuNatESim, SD%*%t(SD)*cov2cor(as.matrix(NatECovar[,-1])), method = "svd")))
  i = 1
  while(i<=dim(Cand)[1]){
    Cand$PollAve[i] <- Cand$WeightAve[i] - filter(NatE, Party == Cand$Party[i])$NatESim
    if(!is.na(Cand$PollAve[i]) && Cand$PollAve[i]<=0){
      Cand$PollAve[i] <- 0.0005
    }
    i = i+1
  }
  
  CandFT <- filter(Cand, is.na(`Vote to Use`))
  CandP <- setdiff(Cand, CandFT)
  CandFTN <- filter(CandFT, is.na(PollAve))
  CandPN <- filter(CandP, is.na(PollAve))
  CandFT <- setdiff(CandFT, CandFTN)
  CandP <- setdiff(CandP, CandPN)
  CandFT <- mutate(CandFT, muuPred = predict.glm(MEYESFT.fit, CandFT))
  CandFT <- mutate(CandFT, se = predict.glm(MEYESFT.fit, CandFT, se.fit = TRUE)$se.fit)
  CandP <- mutate(CandP, muuPred  = predict.glm(MEYES.fit, CandP))
  CandP <- mutate(CandP, se  = predict.glm(MEYES.fit, CandP, se.fit = TRUE)$se.fit)
  CandFTN <- mutate(CandFTN, muuPred = predict.glm(NoPollFT.fit, CandFTN))
  CandFTN <- mutate(CandFTN, se = predict.glm(NoPollFT.fit, CandFTN, se.fit = TRUE)$se.fit)
  CandPN <- mutate(CandPN, muuPred  = predict.glm(NoPoll.fit1, CandPN))
  CandPN <- mutate(CandPN, se  = predict.glm(NoPoll.fit1, CandPN, se.fit = TRUE)$se.fit)
  Candsim <- rbind(CandP, CandFT, CandPN, CandFTN)
  Candsim <- mutate(Candsim, Pred = 0)
  Candsim <- arrange(Candsim, Electorate, Party)
  Electorate_List <- unique(Candsim$Electorate)
  i = 1
  k = 1
  while(i<=length(Electorate_List)){
    ElecPred <- filter(Candsim, Electorate == Electorate_List[i])
    ElecCovar <- filter(Covar, Party %in% ElecPred$Party)
    ElecCovar <- as.matrix(ElecCovar[,which(names(ElecCovar)%in%ElecCovar$Party)])
    ElecPred <- mutate(ElecPred, Pred = exp(diag(rmvnorm(dim(ElecPred)[1], muuPred, se%*%t(se)*cov2cor(ElecCovar), method = "svd"))))
    if(sum(ElecPred$Pred)>1){
      ElecPred$Pred <- ElecPred$Pred/sum(ElecPred$Pred)
      } else{
      if(sum(ElecPred$Pred)<0.95){
        ElecPred$Pred <- 0.95*ElecPred$Pred/sum(ElecPred$Pred)
      }
    }
    Candsim$Pred[k:(k+dim(ElecPred)[1]-1)] <- ElecPred$Pred
    k = k+dim(ElecPred)[1]
    i = i+1
  }
  assign("MECandSim", Candsim, envir = globalenv())
}

MECProjectNoPolls <- function(Cand, Covar, PartyVote){
  PartyVote <- semi_join(PartyVote, Cand,  by = c("Party", "Electorate"))
  PartyVote <- arrange(PartyVote, Electorate, Party)
  Cand <- arrange(Cand, Electorate, Party)
  Cand$Party.Vote.Electorate <- PartyVote$Pred
  
  CandFTN <- filter(Cand, is.na(Vote.to.Use))
  CandPN <- setdiff(Cand, CandFTN)
  CandFTN <- mutate(CandFTN, muuPred = predict.glm(NoPollFT.fit, CandFTN))
  CandFTN <- mutate(CandFTN, se = predict.glm(NoPollFT.fit, CandFTN, se.fit = TRUE)$se.fit)
  CandPN <- mutate(CandPN, muuPred  = predict.glm(NoPoll.fit1, CandPN))
  CandPN <- mutate(CandPN, se  = predict.glm(NoPoll.fit1, CandPN, se.fit = TRUE)$se.fit)
  Candsim <- rbind(CandPN, CandFTN)
  Candsim <- mutate(Candsim, Pred = 0)
  Candsim <- arrange(Candsim, Electorate, Party)
  Electorate_List <- unique(Candsim$Electorate)
  i = 1
  k = 1
  while(i<=length(Electorate_List)){
    ElecPred <- filter(Candsim, Electorate == Electorate_List[i])
    ElecCovar <- filter(Covar, Party %in% ElecPred$Party)
    ElecCovar <- as.matrix(ElecCovar[,which(names(ElecCovar)%in%ElecCovar$Party)])
    ElecPred <- mutate(ElecPred, Pred = exp(diag(rmvnorm(dim(ElecPred)[1], muuPred, se%*%t(se)*cov2cor(ElecCovar), method = "svd"))))
   if(sum(ElecPred$Pred)>1){
      ElecPred$Pred <- ElecPred$Pred/sum(ElecPred$Pred)
      } else{
      if(sum(ElecPred$Pred)<0.95){
        ElecPred$Pred <- 0.95*ElecPred$Pred/sum(ElecPred$Pred)
      }
    }
    Candsim$Pred[k:(k+dim(ElecPred)[1]-1)] <- ElecPred$Pred
    k = k+dim(ElecPred)[1]
    i = i+1
  }
  assign("MECandSim", Candsim, envir = globalenv())
}

# Admin Functions ---------------------------------------------------------

SaintLague <- function(PartyVote){
  SL <- PartyVote
  i = 1
  while(i<=199){
    SLv <- SL$Adj.Average/(2*i+1)
    SL <- cbind(SL, SLv)
    i = i+1
  }
  SLs <- SL[,-c(1:2)]
  SLv <- as.vector(as.matrix(SLs))
  SLs <- SLs >= sort(SLv, partial = length(SLv)-119)[length(SLv)-119]
  Seats <- data.frame(Party = PartyVote$Party, Seats = base::rowSums(SLs))
  assign("TotalSeatsSim", Seats, envir = globalenv())
}

ElectorateSeats <- function(Candidate, Seats, MaoriCand, MSeats, TSeats){
  Candidate <- arrange(Candidate, Electorate, Party)
  i = 1
  j = 1
  Electorate_List <- unique(Candidate$Electorate)
  while(i<=64){
    Elec <- filter(Candidate, Electorate == Electorate_List[i])
    Elec <- mutate(Elec, Win = as.numeric(Pred == max(Pred)))
    Seats[j:(j+dim(Elec)[1]-1),6] <- Elec$Win
    i = i+1
    j = j+dim(Elec)[1]
  }
  
  MaoriCand <- arrange(MaoriCand, Electorate, Party)
  i = 1
  j = 1
  ME_List <- unique(MaoriCand$Electorate)
  while(i<=7){
    Elec <- filter(MaoriCand, Electorate == ME_List[i])
    Elec <- mutate(Elec, Win = as.numeric(Pred == max(Pred)))
    MSeats[j:(j+dim(Elec)[1]-1),6] <- Elec$Win
    i = i+1
    j = j+dim(Elec)[1]
  }
  
  AllSeats <- rbind(Seats, MSeats)
  
  i = 1
  TSeats <- mutate(TSeats, Seats = 0)
  while(i<=9){
    TSeats$Seats[i] <-  colSums(filter(AllSeats, Party == unique(TSeats$Party)[i])[,6])
    i = i+1
  }
  assign("ElecSeatsSim", TSeats, envir = globalenv())
  assign("CandWinSim", AllSeats, envir = globalenv())
}

ListSeats <- function(TotalSeats, ElectorateSeats){
  ListSeats <- TotalSeats
  ListSeats <- cbind(ListSeats, ElecSeats = ElectorateSeats$Seats)
  ListSeats <- mutate(ListSeats, LSeats = ifelse(Seats - ElecSeats<0,0, Seats- ElecSeats))
  assign("ListSeatsSim", ListSeats[,-c(2:3)], envir = globalenv())
}

TotalSeats <- function(ElectorateSeats, ListSeats){
  TotalSeats <- ElectorateSeats
  TotalSeats$Seats <- ElectorateSeats$Seats + ListSeats$LSeats
  assign("FinalSeatSims", TotalSeats, envir = globalenv())
}

# Simulations -------------------------------------------------------------
NSim = 1
ptm <- proc.time()
while(NSim < MaxSims+1){
  # Calulating Nationwide Party Vote
  HouseEffect(House = HouseEffects.df, GEPolls = GEPolls.df)
  WeightAverage(GEPolls = GEPollsSim.df)
  DesignEffect(Design = DesignE.df)
  TrendLineCalc(GEPolls = GEPollsSim.df)
  AdjustedAverage(NatE = NatPollE.df, Design = DesignSim.df, WtAve = WeightAveSim.df, Polls = GEPollsSim.df, NatECovar = NatECovar.df, CoVar = Covar.df, TrendAdj = TrendAdjSim)
  STM(param = c(0.321,0.1), Electorates = Electorate17Data.df, Parties = Party17.df,  Adj = AdjustedPartyVote.df)
  CandPredict(Candidates = Candidate17.df, PartyVote = STMPreds, CandCovar = CandCovar.df, OhariuCovar = OhariuCovar.df)
  
  MEDEPV(Design = MEDesignE.df)
  MENatErrorPV(MEPVNatE = MENatE.df)
  #MEPVWtAve(Polls = MEPartyPolls.df, Results = MEResults.df, param = MEparam)
  #MEPVfunc(WtAve = MEPVPrePolls.df, Polls =  MEPartyPolls.df, NatE = MEPVNatESim, DE = MEDEPVsim, NatECovar = MENateCoVar.df)
  MEPVfuncNoPolls(WtAve = MEPVPrePolls.df, Polls =  MEPartyPolls.df, NatE = MEPVNatESim, DE = MEDEPVsim, NatECovar = MENateCoVar.df)
  
  #MEDEC(Design = MEDesignECand.df)
  #MENatErrorC(MECNatE = MENatECand.df)
  #MECWtAve(Polls = MECPolls.df, Cand = 0MECandidate17.df, param = c(23.07,0.1547))
  #MECProject(Cand = MECandidate17.df, WtAve = MECWtAvesim, Covar = MECCovar.df, Design = MEDECsim, PartyVote = MEPVSim, NatE = MECNatESim, NatECovar = MENateCandCoVar.df)
  MECProjectNoPolls(Cand = MECandidate17.df, PartyVote = MEPVSim, Covar = MECCovar.df)
  
  SaintLague(PartyVote = AdjustedPartyVote.df)
  ElectorateSeats(Candidate = CandSim, Seats = StoreCand1.df, MaoriCand = MECandSim, MSeats = StoreMECand1.df, TSeats = StoreElecSeats1.df)
  ListSeats(TotalSeats = TotalSeatsSim, ElectorateSeats = ElecSeatsSim)
  TotalSeats(ElectorateSeats = ElecSeatsSim, ListSeats = ListSeatsSim)
  
  StoreSeats.df <- cbind(StoreSeats.df, FinalSeatSims$Seats)
  StoreElecSeats.df <- cbind(StoreElecSeats.df, ElecSeatsSim$Seats)
  StoreListSeats.df <- cbind(StoreListSeats.df, ListSeatsSim$LSeats)
  StorePV.df <- cbind(StorePV.df, AdjustedPartyVote.df$Adj.Average)
  StoreElecPV.df <- cbind(StoreElecPV.df, STMPreds$Pred)
  StoreMEPV.df <- cbind(StoreMEPV.df, MEPVSim$Pred)
  CandSim <- arrange(CandSim, Electorate, Party)
  StoreCand.df <- cbind(StoreCand.df, CandSim$Pred)
  StoreCandWin.df <- cbind(StoreCandWin.df, CandWinSim$V6)
  MECandSim <- arrange(MECandSim, Electorate, Party)
  StoreMECand.df <- cbind(StoreMECand.df, MECandSim$Pred)
  
  NSim <- NSim+1
}
ptm - proc.time()
# Write Up ----------------------------------------------------------------

# The code below produces summary dfs which will be used in visualtion

lowr = 0.05
upr = 0.95 # you can edit these to get which ever range you desire default at 90

# Party Vote Summary ------------------------------------------------------

# Nationwide Party Vote - using median as middle value as believe its better but you can change to mean if you desire
TotalPVSum.df <- StorePV.df[,c(1:2)]
TotalPVSum.df <- mutate(TotalPVSum.df, `median` = apply(StorePV.df[,c(3:(2+MaxSims))], 1, median))
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
ElectoratePVSum.df <- mutate(ElectoratePVSum.df, `median` =  apply(StoreElecPV.df[,c(5:(4+MaxSims))], 1, median))
ElectoratePVSum.df <- mutate(ElectoratePVSum.df, lowr = 0)
ElectoratePVSum.df <- mutate(ElectoratePVSum.df, upr = 0)
i = 1
while(i<=dim(ElectoratePVSum.df)[1]){
  ElectoratePVSum.df[i,6] <- sort(StoreElecPV.df[i,c(5:(4+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  ElectoratePVSum.df[i,7] <- sort(StoreElecPV.df[i,c(5:(4+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

# Maori Electorate Party Vote
MEPVSum.df <- StoreMEPV.df[,c(1:4)]
MEPVSum.df <- mutate(MEPVSum.df, `median` =  apply(StoreMEPV.df[,c(4:(3+MaxSims))], 1, median))
MEPVSum.df <- mutate(MEPVSum.df, lowr = 0)
MEPVSum.df <- mutate(MEPVSum.df, upr = 0)
i = 1
while(i<=dim(MEPVSum.df)[1]){
  MEPVSum.df[i,6] <- sort(StoreMEPV.df[i,c(4:(3+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  MEPVSum.df[i,7] <- sort(StoreMEPV.df[i,c(4:(3+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

# Candidate Vote Summary --------------------------------------------------

CandSum.df <- StoreCand.df[,c(1:5)]

CandSum.df <- mutate(CandSum.df, `median` =  apply(StoreCand.df[,c(6:(5+MaxSims))], 1, median))
CandSum.df <- mutate(CandSum.df, lowr = 0)
CandSum.df <- mutate(CandSum.df, upr = 0)
i = 1
while(i<=dim(CandSum.df)[1]){
  CandSum.df[i,7] <- sort(StoreCand.df[i,c(6:(5+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  CandSum.df[i,8] <- sort(StoreCand.df[i,c(6:(5+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

MECandSum.df <- StoreMECand.df[,c(1:5)]

MECandSum.df <- mutate(MECandSum.df, `median` =  apply(StoreMECand.df[,c(6:(5+MaxSims))], 1, median))
MECandSum.df <- mutate(MECandSum.df, lowr = 0)
MECandSum.df <- mutate(MECandSum.df, upr = 0)
i = 1
while(i<=dim(MECandSum.df)[1]){
  MECandSum.df[i,7] <- sort(StoreMECand.df[i,c(6:(5+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  MECandSum.df[i,8] <- sort(StoreMECand.df[i,c(6:(5+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

AllCandSum <- rbind(CandSum.df, MECandSum.df)
AllCandSum <- mutate(AllCandSum, `% Win` = rowSums(StoreCandWin.df[,c(6:(5+MaxSims))])/MaxSims)


# Seat Summary ------------------------------------------------------------

SeatsSum.df <- StoreSeats.df[,c(1:2)]
SeatsSum.df <- mutate(SeatsSum.df, `median` = apply(StoreSeats.df[,c(3:(2+MaxSims))], 1, median))
SeatsSum.df <- mutate(SeatsSum.df, lowr = 0)
SeatsSum.df <- mutate(SeatsSum.df, upr = 0)
i = 1
while(i<=9){
  SeatsSum.df[i,4] <- sort(StoreSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  SeatsSum.df[i,5] <- sort(StoreSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

ElecSeatsSum.df <- StoreElecSeats.df[,c(1:2)]
ElecSeatsSum.df <- mutate(ElecSeatsSum.df, `median` = apply(StoreElecSeats.df[,c(3:(2+MaxSims))], 1, median))
ElecSeatsSum.df <- mutate(ElecSeatsSum.df, lowr = 0)
ElecSeatsSum.df <- mutate(ElecSeatsSum.df, upr = 0)
i = 1
while(i<=9){
  ElecSeatsSum.df[i,4] <- sort(StoreElecSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  ElecSeatsSum.df[i,5] <- sort(StoreElecSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}

ListSeatsSum.df <- StoreListSeats.df[,c(1:2)]
ListSeatsSum.df <- mutate(ListSeatsSum.df, `median` = apply(StoreListSeats.df[,c(3:(2+MaxSims))], 1, median))
ListSeatsSum.df <- mutate(ListSeatsSum.df, lowr = 0)
ListSeatsSum.df <- mutate(ListSeatsSum.df, upr = 0)
i = 1
while(i<=9){
  ListSeatsSum.df[i,4] <- sort(StoreListSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*lowr)[MaxSims*lowr]
  ListSeatsSum.df[i,5] <- sort(StoreListSeats.df[i,c(3:(2+MaxSims))], partial = MaxSims*upr)[MaxSims*upr]
  i = i+1
}
# Misc Summary ------------------------------------------------------------

FivePercent.df <- data.frame(Party = StorePV.df$Party, `% > 5%` = rowSums(StorePV.df[,c(3:(2+MaxSims))]>=0.05)/MaxSims)
HaveList.df <- data.frame(Party = StoreListSeats.df$Party, `% List` = rowSums(StoreListSeats.df[,c(3:(2+MaxSims))]>=1)/MaxSims)

# Previous Summary Files --------------------------------------------------

# setwd() # Set to wherever storing 

# Add file names in here

# PrevPVSum.df <- read_csv()
# PrevElecPVSum.df <- read_csv()
# PrevMEPVSum.df <- read_csv()
# PrevCandSum.df <- read_csv()
# PrevSeatSum.df <- read_csv()
# PrevElecSeatSum.df <- read_csv()
# PrevListSeatSum.df <- read_csv()
# PrevFive.df <- read_csv()
# PrevHaveList.df <- read_csv

# Change Summary ----------------------------------------------------------

ChangePVSum.df <- TotalPVSum.df
# ChangePVSum.df[,c(3:5)] <- ChangePVSum.df[,c(3:5)] - PrevPVSum.df[,c(3:5)]
# ChangeElecPVSum.df <- ElectoratePVSum.df
# ChangeElecPVSum.df[,c(5:7)] <- ChangeElecPVSum.df[,c(5:7)] - PrevElecPVSum.df[,c(5:7)]
# #ChangeMEPVSum.df <- MEPVSum.df
# #ChangeMEPVSum.df[,c(5:7)] <- ChangeMEPVSum.df[,c(5:7)] - PrevMEPVSum.df[,c(5:7)]
# ChangeCandSum.df <- AllCandSum
# ChangeCandSum.df[,c(6:9)] <- ChangeCandSum.df[,c(6:9)] - PrevCandSum.df[,c(6:9)]
# ChangeSeatSum.df <- SeatsSum.df
# ChangeSeatSum.df[,c(3:5)] <- ChangeSeatSum.df[,c(3:5)] - PrevSeatSum.df[,c(3:5)]
# ChangeElecSeatSum.df <- ElecSeatsSum.df
# ChangeElecSeatSum.df[,c(3:5)] <- ChangeElecSeatSum.df[,c(3:5)] - PrevElecSeatSum.df[,c(3:5)]
# ChangeListSeatSum.df <- ListSeatsSum.df
# ChangeListSeatSum.df[,c(3:5)] <- ChangeListSeatSum.df[,c(3:5)] - PrevListSeatSum.df[,c(3:5)]
# ChangeFive.df <- FivePercent.df
# ChangeFive.df[,2] <- ChangeFive.df[,2] - PrevFive.df[,2]
# ChangeHaveList.df <- HaveList.df
# ChangeHaveList.df[,2] <- ChangeHaveList.df[,2] - PrevHaveList.df[,2]
# Party Vote Calculation Explainer ----------------------------------------

MuuHouseEffect <- function(House, GEPolls){
  House <- mutate(House, SimEst = Estimate + Baseline)
  House <- House[,-c(3:6)]
  House <- spread(House, Party, SimEst)
  PollInclude = unique(House$Pollster)
  i = 1
  while(i<=dim(GEPolls)[1]){
    if(GEPolls[i,1] %in% PollInclude){
      GEPolls[i,4:12] <- GEPolls[i,4:12] -  filter(House, Pollster  == as.character(GEPolls[i,1]))[,2:10]
    }
    i = i+1
  }
  GEPolls[,4:12][GEPolls[,4:12]<0.0005] <- 0.0005
  assign("GEPollsMuu.df", GEPolls, envir = globalenv())
}

MuuWeightAverage <- function(GEPolls){
  GEPolls <- filter(GEPolls, Pollster!= "Election result")
  GEPolls <- filter(GEPolls, `Release Days`<94)
  GEPolls <- mutate(GEPolls, RawWeight = exp(-log(2)*(`Days Before`-DaysTo)/(1.96+0.2*DaysTo)))
  GEPolls <- mutate(GEPolls, Weight = RawWeight/sum(RawWeight))
  GEPolls[,4:12] <- as.data.frame(apply(GEPolls[,4:12],2,function(x) x*GEPolls[,14]))
  WeightAve <- data.frame(Party = colnames(GEPolls)[4:12], Wt.Ave = colSums(GEPolls[,c(4:12)]))
  WeightAve$Wt.Ave <- log(WeightAve$Wt.Ave)
  assign("MuuWeightAve.df", WeightAve, envir = globalenv())
}

MuuTrendLineCalc <- function(GEPolls){
  Party_list <- list(colnames(GEPolls[,c(4:dim(GEPolls)[2])]))
  Muu = rep(0,dim(GEPolls)[2]-3)
  SE = rep(0,dim(GEPolls)[2]-3)
  CurMuu = rep(0,dim(GEPolls)[2]-3)
  CurSE = rep(0,dim(GEPolls)[2]-3)
  i = 1
  while(i<=length(Party_list[[1]])){
    Alpha <- select(GEPolls, contains(Party_list[[1]][i]))
    Beta <- data.frame(Poll = GEPolls$Pollster, Vote = Alpha, Day = rep(max(GEPolls$`Days Before`), dim(Alpha)[1]) - GEPolls$`Days Before`)
    Party.fit <- gam(log(Beta[,2])~s(as.numeric(Day)), data = Beta, weights = ifelse(Poll=="Election result", log((2286190+2356536+2257336+2405620)/4), 1))
    Muu[i] <- predict.gam(Party.fit, newdata = data.frame(Pollster = "Election Result", Day = max(GEPolls$`Days Before`)))
    SE[i] <- predict.gam(Party.fit, newdata = data.frame(Pollster = "Election Result", Day = max(GEPolls$`Days Before`)), se.fit = TRUE)$se.fit
    CurMuu[i] <- predict.gam(Party.fit, newdata = data.frame(Pollster = "Election Result", Day = max(Beta$Day)))
    CurSE[i] <- predict.gam(Party.fit, newdata = data.frame(Pollster = "Election Result", Day = max(Beta$Day)), se.fit = TRUE)$se.fit
    i = i+1
  }
  MuuDiff = (Muu - CurMuu)/log(min(GEPolls$`Days Before`)+1)
  assign("MuuTrendAdj", MuuDiff, envir = globalenv())
}

MuuHouseEffect(House = HouseEffects.df, GEPolls = GEPolls.df)
MuuWeightAverage(GEPolls = GEPollsMuu.df)
MuuTrendLineCalc(GEPolls = GEPollsMuu.df)

PVExplain.df <- data.frame(Party = StorePV.df$Party, Weight.Average = exp(MuuWeightAve.df$Wt.Ave), Trendline.Adjustment = (exp(MuuWeightAve.df$Wt.Ave + MuuTrendAdj)-exp(MuuWeightAve.df$Wt.Ave)), National.Polling.Error.Estimate = (exp(NatPollE.df$Muu*(MuuWeightAve.df$Wt.Ave + MuuTrendAdj))-exp(MuuWeightAve.df$Wt.Ave + MuuTrendAdj)), median = TotalPVSum.df$median)

# csv Writers -------------------------------------------------------------

#setwd() # Set to where ever desire writing

write.csv(TotalPVSum.df, "Simulated PV.csv")
#write.csv(ChangePVSum.df, "Change PV.csv")
write.csv(ElectoratePVSum.df, "Simulated Electorate PV.csv")
#write.csv(ChangeElecPVSum.df, "Change Electorate PV.csv")
write.csv(MEPVSum.df, "Simulated ME PV.csv")
#write.csv(ChangeMEPVSum.df, "Change ME PV.csv")
write.csv(AllCandSum, "Simulated Candidate.csv")
#write.csv(ChangeCandSum.df, "Change Candidate.csv")
write.csv(SeatsSum.df, "Simulated Seats.csv")
#write.csv(ChangeSeatSum.df, "Change Seats.csv")
write.csv(ElecSeatsSum.df, "Simulated Elec Seats.csv")
#write.csv(ChangeElecSeatSum.df, "Change Elec Seats.csv")
write.csv(ListSeatsSum.df, "Simulated List Seats.csv")
#write.csv(ChangeListSeatSum.df, "Change List Seats.csv")
#write.csv(FivePercent.df, "Simulated Five Threshold.csv")
#write.csv(ChangeFive.df, "Change Five Threshold.csv")
#write.csv(HaveList.df, "Simulated Have List.csv")
#write.csv(ChangeHaveList.df, "Change Have List.csv")
#write.csv(PVExplain.df, "Explained Party Vote.csv")  
