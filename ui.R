
navbarPage(
  "Wasted Productivity",
  theme = shinythemes::shinytheme("sandstone"),
  #position = "fixed-top",
  # nflfastR data --------------------------------------------------------------
  tabPanel("Offense - nflfastR",
           "",
           sidebarLayout(
             sidebarPanel(width = 4,
                          selectInput(
                            inputId = 'skillPos',
                            label = 'Select Offensive Category (also affects 
                            Season Summary Tab)',
                            choices = c('Passing', 'Receiving', 'Rushing'),
                            selected = 'Passing',
                            multiple = FALSE),
                          selectizeInput(
                            inputId  = 'teamList', 
                            label    = 'Select teams',
                            choices  = teamList,
                            selected = teamList[1:2],
                            multiple = TRUE),
                          selectInput(
                            inputId = 'stat',
                            label = 'Select Stat',
                            choices = passingStats,
                            selected = passingStats[1]),
                          checkboxInput(
                            inputId = 'trend_line',
                            label = 'Include trend line',
                            value = FALSE
                          ),
                          checkboxInput(
                            inputId = 'first_three',
                            label = 'Include plays that are only on the first 
                            three downs',
                            value = FALSE
                          ),
                          checkboxInput(
                            inputId = 'garbage_time',
                            label = 'Remove plays from garbage time (last 2 
                            mins of game and win probabiilty less than 85% or 
                            greater than 15%)',
                            value = FALSE
                          ),
                          sliderInput(
                            inputId = 'minGames',
                            label = 'Filter minimum number of games played',
                            min = 1,
                            max = ifelse(currentWeek-1<=0,1,currentWeek-1),
                            step = 1,
                            value = ifelse(currentWeek > 17,
                                           10, 
                                           ifelse(currentWeek-2<=0,
                                                  1,
                                                  currentWeek-2))
                            )
           ),
           mainPanel(
             plotlyOutput('main_plot'),
             h2('Game by game stats which build the above plot.'),
             br(),
             DT::dataTableOutput('sumary_table')
             )
           )),
  tabPanel("Offense Season Summary - nflfastr", "", 

           DT::dataTableOutput(('season_summary'))),
  tabPanel("Defense - nflfastR",
           "",
           sidebarLayout(
             sidebarPanel(selectizeInput(
               inputId  = 'defTeamList', 
               label    = 'Select Defensive unit',
               choices  = teamList,
               selected = teamList[1:2],
               multiple = TRUE),
               selectInput(
                 inputId = 'defStat',
                 label = 'Select Stat',
                 choices = defStatsCategories,
                 selected = defStatsCategories[1]),
               checkboxInput(inputId = 'def_trend_line', 
                             label = 'Include Trend line',
                             value = FALSE)),
             mainPanel(plotlyOutput('def_plot')))
           ),
  # rotoguru fanduel data ------------------------------------------------------
  tabPanel('FanDuel Optimizations',
    sidebarLayout(
      sidebarPanel(
        width = 4,
       # teams to exclude
       selectizeInput(
         inputId  = 'rgteamList', 
         label    = 'Select teams to exclude from your lineups, if any',
         choices  = RGteamList,
         selected = NULL,
         multiple = TRUE),
       # min avg points to exclude
       # sliderInput(
       #   inputId = 'minPoints',
       #   label = 'Set miniumum points threshold, i.e., 
       #   remove players who have not scored above a x points in a single game',
       #   min = 0,
       #   max = 10,
       #   step = 1,
       #   value = 0
       # ),
       # Min games played removed
       sliderInput(
         inputId = 'min_games_played',
         label = 'Include only players who have played a minumum number of games',
         min = 1,
         max = currentWeek - 2,
         step = 1,
         value = 1
       ),
       # min salaries to remove
       # Salary Cap
       sliderInput(
         inputId = 'salary_cap',
         label = 'Indicate the salary cap limit you wish to optimize with',
         min = 40000,
         max = 60000,
         step = 500,
         value = 59000
       ),
       # optimize button
       actionButton(
         inputId = 'getOptimal', 
         label = "CLick to get 'Optimal' Lineup")
    ),
    mainPanel(
      # Predicted points, Actual points, salary cap %
      # table of optimized lineup
      #h1(
      #  paste0(
      #    'Predictions and lineup optimization will be back for the \'21 season'
      #    )),
       h2(paste0('Optimal Lineup* - Sunday (2022-01-02) Playoffs Main Slate')),
       p('*May not consider injured or suspended players'),
       p('*Only an initial guide towards creating a DFS team'),
       DT::dataTableOutput('optimal_results'),
       # All Predictions
       h2('All Predictions'),
       DT::dataTableOutput('all_preds')
      
    ))
    ),
  # Raw Data tab ---------------------------------------------------------------
  #tabPanel("Raw data", "",
  #         DT::dataTableOutput('allData')
  #         ),
  
  # About tab ------------------------------------------------------------------
  tabPanel('About', 
           br(),
           br(),
           br(),
           br(),
           p('• Built using nflfastR and RotoGuru data'),
           br(),
           p(paste0('• Predictions derived from simple xgboost model using',
                    '\n',
                    'rolling point metrics and salaries of individual player,'
                    ,'\n',
                    'teams and opposing defenses.')),
           br(),
           p('• Lineups constructed using linear optimzation.'),
           br(),
           p('• Author: Nathan Rodriguez')
           ,a(href="https://www.linkedin.com/in/nathanjr/", "LinkedIn")
           ,br()
           ,a(href="https://www.instagram.com/nathanjrodriguezz/", "Instagram")
           ,br()
           ,a(href="https://twitter.com/nathan_rdrgz", "Twitter")
  ))
