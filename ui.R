library(rhandsontable)

shinyUI(
  navbarPage("Fantasy Baseball Discombobulator!!  (BETA)",
             tabPanel("Draft",htmlOutput("nextPick"),
                      fluidRow(
                        column(6,p(textOutput("pickTimeElapsed"))),
                        column(3,offset = 3, actionButton("RefreshDraft","Refresh Draft"))
                      ),
                      sidebarLayout(
                        sidebarPanel(
                          DT::dataTableOutput("dataAvail"),
                          width = 4),
                        mainPanel(rHandsontableOutput("data"),
                                  h2("Roto Point Projection"),
                                  DT::dataTableOutput("rotoRank"),
                                  h2("Roto Stats Projection"),
                                  DT::dataTableOutput("rotoTotal"))
                      )),
             tabPanel("Forecast Picks",
                      selectInput("forecastTeam","Select Team to Forecast",choices=c("Next Pick",teams),selected=MyTeam),
                      sliderInput("forecastPicks", "Potential Picks to Forecast", value = 20, min = 5, max = 50, step = 5),
                      #checkboxInput("chartShowForecastedRoster", "Show Forecasted Players", TRUE),
                      tableOutput("mockPicks")),
             tabPanel("Roster ALL",
                      checkboxInput("chartShowForecastedRoster", "Show Forecasted Players", TRUE),
                      tableOutput("rosterData")),
             tabPanel("Roster TEAM",
                      selectInput("rosterTeam","Select Team to show Picks",choices=teams,selected=MyTeam),
                      DT::dataTableOutput("rosterHitters"),
                      DT::dataTableOutput("rosterPitchers")),
             tabPanel("Results",
                      column(3,DT::dataTableOutput("draftData")),
                      column(9,tableOutput("draftForecasted"),
                             br(),hr(),br(),
                             plotOutput("draftTotalChart"))
             ),
             tabPanel("Avail Hitters",
                      h2("Hitters Available"),
                      DT::dataTableOutput("dataAvailHitters")),
             tabPanel("Avail Pitchers",
                      h2("Pitchers Available"),
                      DT::dataTableOutput("dataAvailPitchers")),
             tabPanel("All Available",
                      h2("All Available"),
                      DT::dataTableOutput("dataAvailALL")),
             tabPanel("Settings",
                      h2("Draft Settings"),
                      textInput("userName","Fantrax Username", value = userName),
                      passwordInput("passWord","Fantrax Password", value = passWord),
                      textInput("leagueId","Fantrax League Id", value = leagueId),
                      selectizeInput("myTeam","Select My Team",choices=teams,selected=MyTeam),
                      actionButton("saveSettings","Save Settings")
                      )

  )
)