fluidPage(
  HTML('<meta name="viewport" content="width=1024">'),
  # setBackgroundColor(
  #   color = c("#383838"),
  #   # gradient = "linear",
  #   # direction = "bottom"
  # ),
  div(
    style = "font-weight: bold;font-size:38px;text-align:center;margin: 20px 0 20px 0;",
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
        htmlOutput("leagueSelector"),
        htmlOutput("plotSelector"),
        htmlOutput("teamSelector"),
        conditionalPanel(condition = "input.plotOption == 'Forwards Profile'",
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
      plotOutput("plot"),
      DTOutput("dataTable"),
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
)
