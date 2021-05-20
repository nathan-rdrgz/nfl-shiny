shiny::shinyServer(function(session, input, output) {
  
  # update selected stat categories every time selection is updated ------------
  observe({
    statChoices <- getStatCategories(input$skillPos)
    updateSelectInput(session, 
                 "stat",
                 label = 'Select Stat',
                 choices = statChoices,
                 selected = statChoices[1])
  })
  
  # Main plot for displaying stat trends ---------------------------------------
  filtered_data <- reactive({
    #browser()
    # Inputs
    gbTime <- input$garbage_time
    minGames   <- input$minGames
    positions  <- input$skillPos
    statToPlot <- input$stat
    teamToPlot <- input$teamList
    homeTeam   <- input$home_team
    threeDowns <- input$first_three
    
    # filtered data
    if(gbTime){
      DT2 <- DT[game_seconds_remaining > 120 & (wp < .85 | wp > .15)]
    } else {
      DT2 <- copy(DT)
    }
    if (threeDowns) DT2 <- DT2[down %in% 1:3]
    
    posDT <- GetWklyStatDT(pos_type = positions, x = DT2)
    posDT <- posDT[N >= minGames]
    posDT <- posDT[variable == statToPlot]
    posDT <- posDT[posteam %chin% teamToPlot]
    posDT
  })
  
  output$main_plot <- renderPlotly({
    MainPlot(xDT = filtered_data(), inclTrend  = input$trend_line)
  })
  
  output$sumary_table <- DT::renderDT(
    expr = {
      filtered_data()[, .(name, week, N, season, team = posteam, 
                          variable, value)]
      },
    filter = "top",
    options = list(pageLength = 25,
                   dom = 'ltp'))
  
  # Season summary table -------------------------------------------------------
  output$season_summary <- renderDT({
    posDT <- GetSeasonStatDT(pos_type = input$skillPos, x = DT)
  },    
  filter = "top"
  )
  
  # Rotoguru optimization ------------------------------------------------------
  observeEvent(
    input$getOptimal, 
    {
      #browser()
       withProgress(message = 'Calculating...', value = 0, {
       
       # Inputs
       rg_teamList <- input$rgteamList
       minPoints2  <- input$minPoints
       minGamesPlayed <- input$min_games_played
       
       # Weekly Predictions
       # Grab salary data from Fanduel
       newLOC <- 'data/'
       newCSV <- 'FanDuel-NFL-2021-01-09-53152-players-list.csv'
       newCSV <- paste0(newLOC, newCSV)
       
      # # Merge with FanDuel names
       newDT  <- fread(newCSV)
       newDT <- merge.data.table(newDT, 
                                 namesDT, 
                                 by.x = "Team", 
                                 by.y = "fanduel_names")
       setnames(newDT,
                old = c('Position', 'Salary', 'Opponent'),
                new = c('Pos', 'salary', 'Oppt'))
       newDT[,Week:=currentWeek+1]
       newDT[,h_a :=ifelse(paste0(Oppt,'@',Team) == Game, 1, 0)]
       newDT[,Name:=ifelse(Pos!='D',
                           paste0(`Last Name`,', ',`First Name`),Nickname)]
       newDT[,Pos :=ifelse(Pos=='D','Def', Pos)]
       newDT[,points := NA]
       
       # MatchOppt
       newDT[, Team := roto_guru_names]
       newDT[, roto_guru_names := NULL]
       newDT <- merge.data.table(newDT, namesDT, by.x="Oppt",by.y="fanduel_names")
       newDT[, Oppt := roto_guru_names]
       newDT[, roto_guru_names := NULL]
       
       # Match names to old data
       library(fuzzyjoin)
       oldNames<-rgDT[rgDT[, .I[Week == max(Week)], by=.(Name,Pos)]$V1]
       oldNames<-unique(oldNames[, .(Name)]) #!Team%chin%rg_teamList
       newNames<-newDT[, .(Week, Name, `First Name`, `Last Name`, Pos,
                         Team, h_a, Oppt, points, salary, Played)]
       allNamesDT<-as.data.table(
         regex_left_join(x = newNames[Played>minGamesPlayed],
                         y=oldNames, by = c('Name')))
       allNamesDT[, Played := NULL]
       
       allNamesDT[is.na(Name.y),.N]
       allNamesDT[is.na(Name.y),]
       #View(allNamesDT)
       
       # FIX NAMING ISSUE
       allNamesDT <- fillMissingNames(allNamesDT, unknownNames, rgNames)
       allNamesDT[is.na(Name.y),]
       # match opponent data
       allNamesDT2 <- allNamesDT[!is.na(Name.y)]
       allNamesDT2[, `:=`(Name = Name.y, Name.x = NULL)][, Name.y:=NULL]
       allNamesDT2[, `First Name`:=NULL]
       allNamesDT2[, `Last Name`:=NULL]
       setcolorder(allNamesDT2,
                   c("Week","Name","Pos","Team", "h_a",
                     "Oppt","points","salary"))
       allNamesDT2 <- rbindlist(list(rgDT, allNamesDT2), fill = T)
       allNamesDT2 <- allNamesDT2[order(Name, Team, Week)]
       # make new variables
       allNamesDT2 <- cleanRGData(allNamesDT2)
       allNamesDT2[, min_pts := min(points, na.rm = T), .(Name, Team)]
       allNamesDT2[, median_pts := median(points, na.rm = T), .(Name, Team)]
       
       # make predictions -------------------------------------------------------
       wk14DT <- allNamesDT2[Week == currentWeek+1]
       
       xgbModel <- readRDS('models/fantasy_model_3.rds')
       colsForModel <- xgbModel$feature_names
       xgResults <- predict(xgbModel, 
                            newdata = as.matrix(wk14DT[, ..colsForModel]))
       resultsDT <- data.table(Name = wk14DT$Name,
                               Pos = wk14DT$Pos,
                               Team = wk14DT$Team,
                               salary = wk14DT$salary,
                               Predicted = round(xgResults, 2),
                               `Minimum Points` = wk14DT$min_pts,
                               `Median Points` = wk14DT$median_pts)
       playersToAvoid <- newDT[Played<=minGamesPlayed | 
                                 `Injury Indicator`%in% c("IR", "O"), Name]
       output$all_preds <- renderDT({
         resultsDT[!Name %chin% playersToAvoid][!Team %in%rg_teamList]
         }, filter = 'top')
       
       # perform optimzation ----------------------------------------------------
       #browser()
       resultsDT2 <- resultsDT[!Name %chin% playersToAvoid]
       resultsDT2 <- resultsDT2[!Team %in%rg_teamList]
       #resultsDT2 <- resultsDT2[`Minimum Points`> minPoints2]
       resultsDT2[, pred_rank := frank(-Predicted), .(Pos)]
       resultsDT2[, team_pct := sum(Predicted, na.rm = T), .(Team)]
       resultsDT2[, pred_team_pct := round(Predicted/team_pct, 2)]
       
       # number of variables
       num.players <- length(resultsDT2$Name)
       
       # objective:
       obj <- resultsDT2$Predicted
       
       # the vars are represented as booleans
       var.types <- rep("B", num.players)
       
       # the constraints
       matrix <- rbind(as.numeric(resultsDT2$Pos == "QB"), # num QB
                       as.numeric(resultsDT2$Pos == "RB"), # num RB
                       as.numeric(resultsDT2$Pos == "RB"), # num RB
                       as.numeric(resultsDT2$Pos == "WR"), # num WR
                       as.numeric(resultsDT2$Pos == "WR"), # num WR
                       as.numeric(resultsDT2$Pos == "TE"), # num TE
                       as.numeric(resultsDT2$Pos == "TE"), # num TE
                       as.numeric(resultsDT2$Pos %in% c("RB", "WR", "TE")),  # Num RB/WR/TE
                       as.numeric(resultsDT2$Pos == "Def"),# num DEF
                       #diag(final$riskNormalized),         # player's risk
                       resultsDT2$salary)                       # total cost
       direction <- c("==",
                      ">=",
                      "<=",
                      ">=",
                      "<=",
                      ">=",
                      "<=",
                      "==",
                      "==",
                      #rep("<=", num.players),
                      "<=")
       rhs <- c(1, # Quartbacks
                2, # RB Min
                3, # RB Max
                3, # WR Min
                4, # WR Max
                1, # TE Min
                1, # TE Max
                7, # RB/WR/TE
                1, # Defense
                input$salary_cap)
       
       sol <- Rglpk_solve_LP(obj = obj, mat = matrix, dir = direction, rhs = rhs,
                             types = var.types, max = T)
       
       resultsDT2[sol$solution==1,]
       optimizedDT <- resultsDT2[sol$solution==1,.(sum(Predicted), sum(salary))]
       output$optimal_results <-  DT::renderDT(
         {resultsDT2[sol$solution==1,.(Name, Pos, Team, Predicted, 
                                       Salary = salary, 
                                       `Minimum Points`,
                                       `Median Points`)][order(-Predicted)]
           })
       
       })
      
      
  })
  # Output table for raw data of selected columns ------------------------------
  # output$allData <- DT::renderDT(
  #   expr = { DT[!is.na(posteam) & play_type %in% c('run', 'pass'), 
  #               .(week, posteam, defteam, play_type,
  #                 passer, rusher, receiver, 
  #                 yards_gained, air_yards, yac = yards_after_catch,
  #                 epa = round(epa, 2), success, pass_location, run_location, 
  #                 spread_line, total_line, divisional_game = div_game, 
  #                 penalty, penalty_team, penalty_type)] 
  #     }, 
  #   filter = "top",
  #   options = list(pageLength = 25,
  #                  dom = 'ltp')
  #   )
  
  # Defense stats -----------------------------------------------------------
  filtered_def_data <- reactive({
    # Inputs
    defTeams <- input$defTeamList
    defStat  <- input$defStat 

    defDT2 <- defDT[team %chin% defTeams & variable == defStat]
    defDT2
  })
  
  output$def_plot <- renderPlotly({
    inclTrend2 <- input$def_trend_line
    p2 <- ggplot(data = filtered_def_data(), 
                mapping = aes(x = week, 
                              y = value, 
                              colour = team, 
                              group= team,
                              text = paste0('Week: ',week,'\n',
                                            variable, ': ',value,'\n',
                                            'Team: ',team,'\n',
                                            'Oppt: ',Oppt))) +
      labs(x = 'Week', y = as.character(filtered_def_data()[1, variable])) +
      geom_point() +
      geom_line(size=1) +
      theme_bw()
    if (inclTrend2) p2 <- p2 + stat_smooth(method = 'lm', se = F)
    p2 <- ggplotly(p2, tooltip = 'text')
    return(p2)
  })
  })