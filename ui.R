fluidPage(
  setBackgroundColor(
    color = c("#f0f0e4"),
  ),
  
  
  tags$head(
    tags$style(HTML("
      
      
      body {
      
        font-family: Tahoma, sans-serif;
      
      }

      .navbar {
      
        background-color: #4a3a3b;
        
      }
      .navbar-default .navbar-nav > .active > a{
         color: #ffffff;
        background-color: #4a3a3b;
      
      }
      .navbar-default .navbar-nav > .active > a:focus {
        color: #ffffff;
        background-color: #4a3a3b;
        border-bottom: 3px solid #f0f0e4;
      }
      
      .navbar-default .navbar-nav > .active > a:hover {
        color: #ffffff;
        background-color: #4a3a3b;
      }
      
      .navbar-default .navbar-brand,
      .navbar-default .navbar-brand:hover,
      .navbar-default .navbar-brand:focus {
        color: #FFF;
      }
      
      .navbar-default .navbar-nav > li > a,
      .navbar-default .navbar-nav > li > a:hover{
        color: #FFF;
      }
      
      .navbar-default .navbar-toggle .icon-bar {
        background-color: #FFF;
      }
      
      .navbar-default .navbar-toggle:hover,
      .navbar-default .navbar-toggle:focus {
        background-color: #4a4a3b;
      }
      
      #sidebar{
        background-color: #f0f0e4;
        border: 0.5px solid #4a3a3b;
      }

    "))
  ),

  navbarPage(
    "Footytistics",
    fluid = TRUE,
    position = "fixed-top",
    collapsible = TRUE,
    tabPanel(
      "Main",
      div(
        style = "font-weight: bold;font-size:38px;
        text-align:center;margin: 50px 0 20px 0;
        color:#4a3a3b",
        textOutput("titleText")
      ),
      div(
        style = "font-style: italic;color: #4a3a3b;
        font-size:18px;text-align:center;margin: 0px 0 0px 0;",
        textOutput("subTitleText")
      ),
      sidebarLayout(
        div(
          style = "margin-top: 50px;color: #4a3a3b
          font-family: Georgia",
          sidebarPanel(id = "sidebar",
            htmlOutput("plotSelector"),
            htmlOutput("leagueSelector"),
            htmlOutput("teamSelector"),
            conditionalPanel(condition = "input.plotOption == 'Compare Forwards'",
                             htmlOutput("playerOneSelector")
            ),
            conditionalPanel(condition = "input.plotOption == 'Compare Forwards'",
                             htmlOutput("leagueTwoSelector")
            ),
            conditionalPanel(condition = "input.plotOption == 'Compare Forwards'",
                             htmlOutput("teamTwoSelector")
            ),
            conditionalPanel(condition = "input.plotOption == 'Compare Forwards'",
                             htmlOutput("playerTwoSelector")
            ),
            conditionalPanel(condition = "input.plotOption == 'Forwards Profile' | input.plotOption == 'Compare Forwards'",
                             htmlOutput("attributeSelector")
            ),
            actionButton("showPlot", label = "Show Plot"),
            conditionalPanel(condition = "input.plotOption == 'Squad Profile'",
              div(
                style = "font-style: italic;font-size:12px;text-align:center;margin: 10px 0 10px 0;",
                textOutput("sideNote")
              ) 
            )
          )
        ),
        mainPanel(
          conditionalPanel(condition = "input.plotOption == 'Attacking Contribution'",
                           girafeOutput("interactivePlot"),
                           actionButton("reset", label = "Reset selection"),
                           tableOutput("datatab")
          ),
          # conditionalPanel(condition = "input.plotOption == 'Squad Profile'",
          #                  girafeOutput("ageinteractivePlot"),
          #                  tableOutput("agedatatab")
          # ),
          conditionalPanel(condition = "input.plotOption != 'Attacking Contribution'",
                           plotOutput("plot"),
          ),
          div(
            textOutput("metricsText"),
            style = "font-size: 10px;font-weight: bold;
            color: #4a3a3b"
          ),
          
          # plotOutput("plot"),
          # conditionalPanel(
          #   condition = "input.plotOption == 'Forwards Profile' && input.attributeOption == 'Goal contributions'|| input.plotOption == 'Compare Forwards' && input.attributeOption == 'Goal contributions'",
          #   DTOutput("dataTable")
          # ),
          DTOutput("dataTable")
          # conditionalPanel(
          #   condition = "input.plotOption == 'Forwards Profile' && input.attributeOption == 'Team Leaders'",
          #   div(
          #     style = "font-style: italic;font-size:12px;text-align:center;margin: 10px 0 10px 0;",
          #     textOutput("aboutNote")
          #   )
          # )
        )
      ),
      div(
        style = "font-style: italic;font-size:18px;text-align:center;margin: 20px 0 20px 0;",
        textOutput("footerText")
      )
    ),
    tabPanel(
      "About"
    )
  )
  
)
