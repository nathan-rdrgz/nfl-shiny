# Global script ----------------------------------------------------------------
library(DT)
library(rvest)
library(shiny)
library(Rglpk)
library(plotly)
library(xgboost)
library(ggplot2)
library(data.table)
library(shinythemes)

source("functions.R")

# Get main raw data table ------------------------------------------------------
DT <- GetPBPData()

# make penalty variables
DT[, offPenalty := ifelse(penalty == 1 & posteam == penalty_team, 1, 0)]
DT[, defPenalty := ifelse(penalty == 1 & defteam == penalty_team, 1, 0)]

# Get list of all teams
teamList <<- DT[!is.na(posteam), unique(posteam)]
teamList <<- teamList[order(teamList)]

# Stat variables
passingStats   <<- GetWklyPassers(DT[posteam==teamList[1]])[, unique(variable)]
rushingStats   <<- GetWklyRushers(DT[posteam==teamList[1]])[, unique(variable)]
receivingStats <<- GetWklyReceivers(DT[posteam==teamList[1]])[,unique(variable)]

# get defense stats
defDT <<- GetDefDT(DT)
defStatsCategories <<- defDT[, unique(variable)]

# Rotoguru data ----------------------------------------------------------------
currentWeek <<- DT[, max(week)]
rgDT <<- getRGData(currentWeek)
 
RGteamList <<- rgDT[, unique(Team)]
RGteamList <<- RGteamList[order(RGteamList)]
  
  