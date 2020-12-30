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
  x <- x[!is.na(passer_id) & season_type == 'REG'&pass_attempt == 1, 
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
  x <- x[!is.na(receiver_id) & season_type == 'REG' & pass_attempt ==1, 
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
  x <- x[!is.na(rusher_id) & season_type == 'REG' & rush_attempt == 1 &
           penalty == 0, 
         .(plays = .N,
           success_rate = round(mean(success, na.rm = T), 2),
           yards_gained = round(sum(yards_gained, na.rm = T), 2),
           median_yards_gained = median(yards_gained, na.rm = T),
           touchdowns = sum(touchdown, na.rm = T),
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
MainPlot <- function(xDT, inclTrend){
  # plot
  p <- ggplot(data = xDT, 
              mapping = aes(x = week, y = value, colour = name)) +
    labs(x = 'Week', y = as.character(xDT[1, variable])) +
    geom_point() +
    geom_line() +
    theme_bw()
  if (inclTrend) p <- p + stat_smooth(method = 'lm', se = F)
  p <- ggplotly(p)
  return(p)
}

# Rotoguru data ----------------------------------------------------------------
namesDT <<- data.table(
  roto_guru_names = c("ari", "atl", "bal", "buf",
                      "car", "chi", "cin", "cle",
                      "dal", "den", "det", "gnb",
                      "hou", "ind", "jac", "kan",
                      "lac", "lar", "lvr", "mia",
                      "min", "nor", "nwe", "nyg",
                      "nyj", "phi", "pit", "sea",
                      "sfo", "tam", "ten", "was"),
  fanduel_names   = c("ARI", "ATL", "BAL", "BUF",
                      "CAR", "CHI", "CIN", "CLE",
                      "DAL", "DEN", "DET", "GB",  
                      "HOU", "IND", "JAC", "KC",
                      "LAC", "LAR", "LV",  "MIA",
                      "MIN", "NO",  "NE",  "NYG",
                      "NYJ", "PHI", "PIT", "SEA",
                      "SF",  "TB",  "TEN", "WAS"),
  FullNames = c("Arizona"     ,"Atlanta"    ,"Baltimore"   ,"Buffalo"      
                ,"Carolina"   ,"Chicago"    ,"Cincinnati"  ,"Cleveland"  
                ,"Dallas"     ,"Denver"     ,"Detroit"     ,"Green Bay"  
                ,"Houston"    ,"Indianapolis","Jacksonville","Kansas City"
                ,"LA Chargers","LA Rams"     ,"Las Vegas"   ,"Miami"        
                ,"Minnesota"  ,"New Orleans" ,"New England" ,"New York G"    
                ,"New York J" ,"Philadelphia","Pittsburgh"  ,"Seattle"    
                ,"San Francisco" ,"Tampa Bay","Tennessee"   ,"Washington")
)

rgWeekly <- function(wk = 1){
  lk<- paste0('http://rotoguru1.com/cgi-bin/fyday.pl?week=',wk,'&game=fd&scsv=1')
  x <- read_html(lk)
  y <- html_nodes(x, 'pre')
  z <- html_text(y[[1]])
  z <- fread(z)
  if ("data.table" %in% class(z)){
    return(z)
  } else {
    print(z)
  }
}
getRGData <- function(finalWeek){
  wklyLS <- list()
  for (i in 1:finalWeek){
    y <- rgWeekly(i)
    wklyLS[[i]] <- y
  }
  
  x <- rbindlist(wklyLS)
  x <- x[order(Name, Team, Week)]
  
  # rename
  setnames(x, old = c('FD points', 'FD salary', 'h/a'),
           new = c('points', 'salary', 'h_a'))
  # remove vars
  x[, Year := NULL]
  x[, GID := NULL]
  x[, h_a := ifelse(h_a == 'h', 1, 0)]
  x[, first_name:= tstrsplit(Name, ', ', fixed=TRUE, keep=2L)]
  x[, last_name := tstrsplit(Name, ', ',fixed=TRUE, keep=1L)]
  x[, initial_name := paste0(substr(first_name, 1, stop = 1),'.',last_name)]
  x[, initial_name:=ifelse(Pos == 'Def', Name, initial_name)]
  x[, first_name:=NULL]
  x[, last_name:=NULL]
  x <- merge.data.table(x,
                        y=namesDT[,.(roto_guru_names, team=fanduel_names)],
                        by.x='Team',by.y='roto_guru_names',all.x=T)
  return(x)
}
cleanRGData <- function(DT){
  #browser()
  DT <- copy(DT)
  DT[, team_offense_pts:=sum(points[!Pos%in%c('Def','PK')],na.rm=T),.(Week, Team)]
  DT[, team_offense_sal:=sum(salary[!Pos%in%c('Def','PK')],na.rm=T),.(Week, Team)]
  DT[, pct_team_offense := salary/team_offense_sal]
  
  # previous weekly stats
  DT[, prev_h_a := c(NA, h_a[-.N]), by=.(Name, Team)]
  DT[, prev_wk := c(NA, Week[-.N]), by=.(Name, Team)]
  DT[, prev_wk_played := ifelse(Week - prev_wk == 1, 1, 0)]
  DT[, prev_wk_pts := c(NA, points[-.N]), .(Name, Team)]
  DT[, prev_wk_pts2:= c(NA, prev_wk_pts[-.N]), .(Name, Team)]
  DT[, prev_wk_avg := frollapply(
    prev_wk_pts, n = 4,mean, fill = NA, na.rm = TRUE),.(Name, Team)]
  DT[, prev_wk_max := frollapply(
    prev_wk_pts, n = 3,max, fill = NA, na.rm = TRUE),.(Name, Team)]
  DT[, prev_wk_var := frollapply(
    prev_wk_pts, n = 3,var, fill = NA, na.rm = TRUE),.(Name, Team)]
  
  DT[, prev_wk_sal := c(NA, salary[-.N]), .(Name, Team)]
  DT[, prev_wk_sal2:= c(NA, prev_wk_sal[-.N]), .(Name, Team)]
  DT[, prev_team_offense_pts := c(NA, team_offense_pts[-.N]), .(Name, Team)]
  DT[, prev_team_offense_sal := c(NA, team_offense_sal[-.N]), .(Name, Team)]
  DT[, prev_team_offense_pts_pct := round(prev_wk_pts/prev_team_offense_pts, 2)]
  DT[, prev_team_offense_sal_pct := round(prev_wk_sal/prev_team_offense_sal, 2)]
  
  # Team Def
  defDT <- DT[Pos == 'Def', .(Week, Team,
                              oppt_def_salary = salary,
                              oppt_prev_def_h_a = ifelse(prev_h_a == 1, 0, 1),
                              oppt_prev_wk_def_pts = prev_wk_pts,
                              oppt_prev_wk_def_sal = prev_wk_sal,
                              oppt_pct_team_offense = pct_team_offense)][
                                order(Team, Week)]
  defDT[, oppt_def_salary_avg := frollapply(
    oppt_def_salary, n = 4,median, fill = NA, na.rm = TRUE),.(Team)]
  defDT[, oppt_def_salary_max := frollapply(
    oppt_def_salary, n = 4,max, fill = NA, na.rm = TRUE),.(Team)]
  defDT[, oppt_def_salary_var := frollapply(
    oppt_def_salary, n = 4,var, fill = NA, na.rm = TRUE),.(Team)]
  
  # merge
  DT2 <- merge.data.table(x = DT, y = defDT,
                          by.x = c('Week', 'Oppt'), 
                          by.y = c('Week', 'Team'),
                          all.x = T)
  DT2 <- DT2[order(Name, Team, Week)]
  
  # New % variables --------------------------------------------------------------
  DT2[, pct_chng_pts := (prev_wk_pts - prev_wk_pts2)/prev_wk_pts2, .(Name, Team)]
  DT2[, pct_chng_pts := round(pct_chng_pts, 2)*100]
  DT2[!is.finite(pct_chng_pts), pct_chng_pts := 0]
  DT2[, pct_chng_sal := (prev_wk_sal - prev_wk_sal2)/prev_wk_sal2, .(Name, Team)]
  DT2[, pct_chng_sal := round(pct_chng_sal, 2)*100]
  DT2[!is.finite(pct_chng_sal), pct_chng_sal := 0]
  
  DT2[, QB := ifelse(Pos == 'QB', 1, 0)]
  DT2[, RB := ifelse(Pos == 'RB', 1, 0)]
  DT2[, WR := ifelse(Pos == 'WR', 1, 0)]
  DT2[, TE := ifelse(Pos == 'TE', 1, 0)]
  DT2[, DEF:= ifelse(Pos == 'Def',1, 0)]
  DT2 <- DT2[salary != 0]
  DT2[is.na(salary), salary := median(salary, na.rm = T), .(Name, Team)]
  return(DT2)
}


fillMissingNames <- function(DT, unknownNames, rgNames){
  for(i in 1:length(rgNames)){
    DT[is.na(Name.y),Name.y:=ifelse(Name.x==unknownNames[i],rgNames[i],Name.y)]
  }
  return(DT)
}


unknownNames <<- c("Gallman Jr., Wayne"     ,"Parham Jr., Donald"    
                   ,"Los Angeles Chargers"   ,"Gordon III, Melvin"    
                   ,"Fuller V, Will"         ,"Moore, DJ"              
                   ,"Jones Jr., Marvin"      ,"Robinson II, Allen"    
                   #,"Mariota, Marcus"        ,"Ateman, Marcell"        
                   #,"Williams, Tyrell"       ,"O'Leary, Nick"        
                   #,"Rogers, Chester"        
                   ,"Westbrook-Ikhine, Nick"
                   ,"Bowden Jr., Lynn"       ,"Gurley II, Todd"      
                   ,"Harris, DeMichael"      ,"Mahomes, Patrick"      
                   #,"Moore, Matt"            ,"Williams, Damien"      
                   ,"Jones II, Ronald"       #,"Rosen, Josh"            
                   #,"Logan, T.J."            
                   ,"Metcalf, DK"          
                   ,"Haskins Jr., Dwayne"    ,"Minshew II, Gardner"    
                   ,"Chark Jr., DJ"          ,"Cole Sr., Keelan",
                   'Snead IV, Willie', 'Ingram II, Mark',
                   'Los Angeles Rams',
                   'Henderson Jr., Darrell',
                   'Jefferson Jr., Van',
                   'Snell Jr., Benny',
                   'McFarland Jr., Anthony',
                   'McCloud III, Ray-Ray',
                   'Atlanta Falcons')
rgNames <<- c('Gallman, Wayne', 'Parham, Donald',
              'LA Chargers', 'Gordon, Melvin',
              'Fuller, Will', 'Moore, D.J.',
              'Jones, Marvin', 'Robinson, Allen',
              #'Mariota, Marcus', '',
              #'Williams, Tyrell', '',
              #'', '',
              'Westbrook, Nick',
              'Bowden, Lynn', 'Gurley, Todd',
              "Harris, De'Michael", 'Mahomes II, Patrick',
              #,"Moore, Matt" , "Williams, Damien"
              'Jones, Ronald', #'Rosen, Josh',
              #'Logan, T.J.',
              'Metcalf, D.K.'
              ,"Haskins, Dwayne", "Minshew, Gardner"    
              ,"Chark, D.J.", "Cole, Keelan",
              'Snead, Willie', 'Ingram, Mark',
              'LA Rams',
              'Henderson, Darrell',
              'Jefferson, Van',
              'Snell, Benny',
              'McFarland, Anthony',
              'McCloud, Ray-Ray',
              '')
