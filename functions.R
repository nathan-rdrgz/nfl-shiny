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

GetWklyStatDT <- function(pos_type,x){
  dt <- switch(pos_type,
         'Passing' = GetWklyPassers(x), 
         'Receiving' = GetWklyReceivers(x), 
         'Rushing' = GetWklyRushers(x)) 
  return(dt)
}

GetSeasonStatDT <- function(pos_type,x){
  dt <- switch(pos_type,
               'Passing' = GetSeasonPassers(x), 
               'Receiving' = GetSeasonReceivers(x), 
               'Rushing' = GetSeasonRushers(x)) 
  return(dt)
}

# Weekly Data ------------------------------------------------------------------
GetWklyReceivers <- function(x, melted = T){
  x <- copy(x)
  x <- x[!is.na(receiver_id) & season_type == 'REG', 
         .(plays = .N,
           total_air_yards = round(sum(air_yards, na.rm = T), 2),
           yac = sum(yards_after_catch, na.rm = T),
           avg_air_yards = round(mean(air_yards, na.rm = T), 2),
           success_rate = round(mean(success, na.rm = T), 2),
           yards_gained = round(sum(yards_gained, na.rm = T), 2),
           epa = round(mean(epa, na.rm = T), 2)
         ), 
         .(name = receiver, week, posteam, season, receiver_id)]
  if(melted){
    x[, N := .N, receiver_id]
    x <- melt.data.table(x, 
                         id.vars = c('name', 'week', 'posteam', 'season', 'N',
                                     'receiver_id'))
  }
  return(x)
}
GetWklyRushers   <- function(x, melted = T){
  x <- copy(x)
  x <- x[!is.na(rusher_id) & season_type == 'REG', 
         .(plays = .N,
           success_rate = round(mean(success, na.rm = T), 2),
           yards_gained = round(sum(yards_gained, na.rm = T), 2),
           touchdowns = sum(ifelse(rush_attempt == 1, touchdown, 0)),
           epa = round(mean(epa, na.rm = T), 2)
         ), 
         .(name = rusher, week, posteam, season,rusher_id)]
  if(melted){
    x[, N := .N, rusher_id]
    x <- melt.data.table(x, 
                         id.vars = c('name', 'week', 'posteam', 'season', 'N',
                                     'rusher_id'))
    }
  return(x)
}
GetWklyPassers   <- function(x, melted = T){
  x <- copy(x)
  x <- x[!is.na(passer_id) & season_type == 'REG', 
         .(plays = .N,
           total_air_yards = round(sum(air_yards, na.rm = T), 2),
           avg_air_yards = round(mean(air_yards, na.rm = T), 2),
           success_rate = round(mean(success, na.rm = T), 2),
           yards_gained = round(sum(yards_gained, na.rm = T), 2),
           qb_hits = sum(qb_hit, na.rm = T),
           tds = sum(ifelse(pass_attempt == 1, touchdown, 0), na.rm = T),
           interceptions = sum(ifelse(pass_attempt==1,interception, 0),na.rm=T),
           epa = round(mean(qb_epa, na.rm = T), 2)
         ), 
         .(name = passer, week, posteam, season, passer_id)]
  if(melted){
    x[, N := .N, passer_id]
    x <- melt.data.table(x, 
                         id.vars = c('name', 'week', 'posteam', 'season', 'N',
                                     'passer_id'))
  }
  return(x)
}

# Season Data ------------------------------------------------------------------
GetSeasonReceivers <- function(x){
  x <- copy(x)
  x <- x[!is.na(receiver_id) & season_type == 'REG', 
         .(plays = .N,
           total_air_yards = round(sum(air_yards, na.rm = T), 2),
           avg_air_yards = round(mean(air_yards, na.rm = T), 2),
           yac = sum(yards_after_catch, na.rm = T),
           success_rate = round(mean(success, na.rm = T), 2),
           yards_gained = round(sum(yards_gained, na.rm = T), 2),
           epa = round(mean(epa, na.rm = T), 2)
         ), 
         .(name = receiver, posteam, season)]
  return(x)
}
GetSeasonRushers   <- function(x){
  x <- copy(x)
  x <- x[!is.na(rusher_id) & season_type == 'REG', 
         .(plays = .N,
           success_rate = round(mean(success, na.rm = T), 2),
           yards_gained = round(sum(yards_gained, na.rm = T), 2),
           touchdowns = sum(ifelse(rush_attempt == 1, touchdown, 0)),
           epa = round(mean(epa, na.rm = T), 2)
         ), 
         .(name = rusher, posteam, season)]
  return(x)
}
GetSeasonPassers   <- function(x){
  x <- copy(x)
  x <- x[!is.na(passer_id) & season_type == 'REG', 
         .(plays = .N,
           total_air_yards = round(sum(air_yards, na.rm = T), 2),
           avg_air_yards = round(mean(air_yards, na.rm = T), 2),
           success_rate = round(mean(success, na.rm = T), 2),
           yards_gained = round(sum(yards_gained, na.rm = T), 2),
           qb_hits = sum(qb_hit, na.rm = T),
           tds = sum(ifelse(pass_attempt == 1, touchdown, 0), na.rm = T),
           interceptions = sum(ifelse(pass_attempt==1,interception, 0),na.rm=T),
           epa = round(mean(qb_epa, na.rm = T), 2)
         ), 
         .(name = passer, posteam, season)]
  return(x)
}
# Plot functions ---------------------------------------------------------------

