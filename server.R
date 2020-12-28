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
  output$main_plot <- renderPlotly({
    # Inputs
    minGames  <- input$minGames
    inclTrend <- input$trend_line
    positions <- input$skillPos
    statToPlot <-input$stat
    teamToPlot <- input$teamList
    
    # filtered data
    posDT <- getStatDT(pos_type = positions, x = DT)
    posDT <- posDT[N >= minGames]
    posDT <- posDT[variable == statToPlot]
    posDT <- posDT[posteam %chin% teamToPlot]
    # plot
    p <- ggplot(data = posDT, 
                mapping = aes(x = week, y = value, colour = name)) +
      geom_point() +
      geom_line() +
      theme_bw()
    if (inclTrend) p <- p + stat_smooth(method = 'lm', se = F)
    p <- ggplotly(p)
    p
  })
  # Output table for raw data of selected columns ------------------------------
  output$allData <- DT::renderDT(
    expr = { DT[!is.na(posteam) & play_type %in% c('run', 'pass'), 
                .(week, posteam, defteam, play_type,
                  passer, rusher, receiver, 
                  yards_gained, air_yards, yards_after_catch,
                  epa = round(epa,2), success, pass_location, run_location, 
                  spread_line, total_line, div_game, penalty, penalty_type)] }, 
    filter = "top",
    options = list(pageLength = 25,
                   dom = 'ltp')
    )
  
  })