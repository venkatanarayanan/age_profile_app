fluidPage(
  setBackgroundColor(
    color = c("#fffff0"),
    #   # gradient = "linear",
    #   # direction = "bottom"
  ),
  navbarPage(
    "Navbar Example",
    fluid = TRUE,
    position = "fixed-top",
    collapsible = TRUE,
    tabPanel(
      "Main",
      div(
        style = "font-weight: bold;font-size:38px;text-align:center;margin: 50px 0 20px 0;",
        textOutput("titleText")
      ),
      div(
        style = "font-style: italic;font-size:18px;text-align:center;margin: 0px 0 0px 0;",
        textOutput("subTitleText")
      ),
      sidebarLayout(
        div(
          style = "margin-top: 50px;",
          sidebarPanel(
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
            div(
              style = "font-style: italic;font-size:12px;text-align:center;margin: 10px 0 10px 0;",
              textOutput("sideNote")
            ),
          )
        ),
        mainPanel(
          conditionalPanel(condition = "input.plotOption == 'Attacking Contribution'",
                           girafeOutput("interactivePlot"),
                           actionButton("reset", label = "Reset selection"),
                           tableOutput("datatab")
          ),
          plotOutput("plot"),
          conditionalPanel(
            condition = "input.plotOption == 'Forwards Profile' && input.attributeOption == 'Goal contributions'|| input.plotOption == 'Compare Forwards' && input.attributeOption == 'Goal contributions'",
            DTOutput("dataTable")
          ),
          conditionalPanel(
            condition = "input.plotOption == 'Forwards Profile' && input.attributeOption == 'Team Leaders'",
            div(
              style = "font-style: italic;font-size:12px;text-align:center;margin: 10px 0 10px 0;",
              textOutput("aboutNote")
            )
          )
        )
      ),
      div(
        style = "font-style: italic;font-size:18px;text-align:center;margin: 20px 0 20px 0;",
        textOutput("footerText")
      )
    ),
    tabPanel(
      "About",
      "This is the About Page"
    )
  )
  
)
