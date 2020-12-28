# Global script ----------------------------------------------------------------
library(DT)
library(rvest)
library(shiny)
library(plotly)
library(xgboost)
library(ggplot2)
library(data.table)
library(shinythemes)

source("functions.R")

# Get main raw data table ------------------------------------------------------
DT <- GetPBPData()

# Get list of all teams
teamList <<- DT[!is.na(posteam), unique(posteam)]
teamList <<- teamList[order(teamList)]

# Stat variables
passingStats   <<- GetPassers(DT[posteam==teamList[1]])[, unique(variable)]
rushingStats   <<- GetRushers(DT[posteam==teamList[1]])[, unique(variable)]
receivingStats <<- GetReceivers(DT[posteam==teamList[1]])[, unique(variable)]
