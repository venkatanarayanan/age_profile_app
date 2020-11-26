fluidPage(
  
  HTML('<meta name="viewport" content="width=1024">'),
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
      style = "margin-top: 50px",
      sidebarPanel(
        htmlOutput("leagueSelector"),
        htmlOutput("plotSelector"),
        htmlOutput("teamSelector"),
        actionButton("showPlot", label = "Show Plot"),
        div(
          style = "font-style: italic;font-size:12px;text-align:center;margin: 10px 0 10px 0;",
          textOutput("sideNote")
        ),
      )
    ),
    mainPanel(
      plotOutput("plot"),
      div(
        style = "font-style: italic;font-size:12px;text-align:center;margin: 10px 0 10px 0;",
        textOutput("aboutNote")
      )
    )
  ),
  div(
    style = "font-style: italic;font-size:18px;text-align:center;margin: 20px 0 20px 0;",
    textOutput("footerText")
  )
)
