library(rhandsontable)

shinyUI(
  navbarPage("Fantasy Baseball Discombobulator!!  (BETA)",
             tabPanel("Draft",htmlOutput("nextPick"),
                      p(textOutput("pickTimeElapsed")),
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
             tabPanel("Roster",
                      checkboxInput("chartShowForecastedRoster", "Show Forecasted Players", TRUE),
                      tableOutput("rosterData")),
             tabPanel("Results",
                      column(3,DT::dataTableOutput("draftData")),
                      column(9,tableOutput("draftForecasted"),
                             br(),hr(),br(),
                             plotOutput("draftTotalChart"))
             ),
             tabPanel("Available Tables",
                      h2("Hitters Available"),
                      DT::dataTableOutput("dataAvailHitters"),
                      h2("Pitchers Available"),
                      DT::dataTableOutput("dataAvailPitchers"),
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