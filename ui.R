
navbarPage(
  "nflfastR",
  theme = shinythemes::shinytheme("sandstone"),
  tabPanel("Offense",
           "",
           sidebarLayout(
             sidebarPanel(width = 4,
                          selectInput(
                            inputId = 'skillPos',
                            label = 'Select Offensive Category',
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
                            inputId = 'garbage_time',
                            label = 'Remove plays from garbage time (last 2 
                            mins of game and win probabiilty less than 80% or 
                            greater than 20%)',
                            value = FALSE
                          ),
                          sliderInput(
                            inputId = 'minGames',
                            label = 'Filter minimum number of games played',
                            min = 1,
                            max = 14,
                            step = 1,
                            value = 5
                            )
           ),
           mainPanel(
             plotlyOutput('main_plot')
             )
           )),
  tabPanel("Summary", "",
           DT::dataTableOutput('sumary_table')),
  tabPanel("Table", "Raw data",
           DT::dataTableOutput('allData')
           )
  )
