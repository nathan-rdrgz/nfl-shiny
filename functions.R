# functions 
GetPBPData <- function(dataYR = 2020){
  # set empty list to hold all years requested
  rdsList <- list()
  
  # loop through all years
  for (i in 1:length(dataYR)){
    baseURL <- 'https://raw.githubusercontent.com/guga31bb/'
    yearURL <- 'nflfastR-data/master/data/play_by_play_'
    rdsURL  <- url(paste0(baseURL, yearURL, dataYR[i], '.rds'))
    rdsDT   <- readRDS(rdsURL)
    DT      <- as.data.table(rdsDT)
    rdsList[[i]] <- DT
  }
  
  return(rbindlist(rdsList, use.names = T))
}

getStatCategories <- function(x){
  switch(x,
         'Passing' = passingStats, 
         'Receiving' = receivingStats, 
         'Rushing' = rushingStats) 
}

getStatDT <- function(pos_type,x){
  dt <- switch(pos_type,
         'Passing' = GetPassers(x), 
         'Receiving' = GetReceivers(x), 
         'Rushing' = GetRushers(x)) 
  return(dt)
}
# Individual Data --------------------------------------------------------------
GetReceivers <- function(x, melted = T){
  x <- copy(x)
  x <- x[!is.na(receiver_id) & season_type == 'REG', 
         .(plays = .N,
           total_air_yards = round(sum(air_yards, na.rm = T), 2),
           avg_air_yards = round(mean(air_yards, na.rm = T), 2),
           success_rate = round(mean(success, na.rm = T), 2),
           yards_gained = round(sum(yards_gained, na.rm = T), 2),
           epa = round(mean(epa, na.rm = T), 2)
         ), 
         .(name = receiver, week, posteam, season)]
  if(melted){
    x[, N := .N, name]
    x <- melt.data.table(x, 
                         id.vars = c('name', 'week', 'posteam', 'season', 'N'))
  }
  return(x)
}
GetRushers   <- function(x, melted = T){
  x <- copy(x)
  x <- x[!is.na(rusher_id) & season_type == 'REG', 
         .(plays = .N,
           success_rate = round(mean(success, na.rm = T), 2),
           yards_gained = round(sum(yards_gained, na.rm = T), 2),
           epa = round(mean(epa, na.rm = T), 2)
         ), 
         .(name = rusher, week, posteam, season)]
  if(melted){
    x[, N := .N, name]
    x <- melt.data.table(x, 
                         id.vars = c('name', 'week', 'posteam', 'season', 'N'))
    }
  return(x)
}
GetPassers   <- function(x, melted = T){
  x <- copy(x)
  x <- x[!is.na(passer_id) & season_type == 'REG', 
         .(plays = .N,
           total_air_yards = round(sum(air_yards, na.rm = T), 2),
           avg_air_yards = round(mean(air_yards, na.rm = T), 2),
           success_rate = round(mean(success, na.rm = T), 2),
           yards_gained = round(sum(yards_gained, na.rm = T), 2),
           epa = round(mean(qb_epa, na.rm = T), 2)
         ), 
         .(name = passer, week, posteam, season)]
  if(melted){
    x[, N := .N, name]
    x <- melt.data.table(x, 
                         id.vars = c('name', 'week', 'posteam', 'season', 'N'))
  }
  return(x)
}
# Plot functions ---------------------------------------------------------------

