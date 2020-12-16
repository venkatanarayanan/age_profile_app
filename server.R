function(input, output, session) {
  output$titleText <- renderText({
    "Squad Profile of Different teams"
  })
  
  output$subTitleText <- renderText({
    "Big Five European leagues = 2020/21"
  })
  
  output$footerText <- renderText({
    "Thanks to fbref.com & Statsbomb for the data provided. If you like my work, give me a follow\nin twitter @VenkyReddevil"
  })
  
  output$sideNote <- renderText({
    "Note:\n'Mean Age' in the plot refers to Weighted mean age.\nValues might slightly differ from the fbref values as\n
    I have not included the 'days' and only included 'years' while calculating the mean.\nPlease feel free to suggest anything"
  })
  
  metricsText <- eventReactive(input$showPlot, {
    
    if(input$plotOption == "Forwards Profile" && 
       input$attributeOption == "Team Leaders" &&
       length(input$plotOption) > 0 &&
       length(input$attributeOption) > 0) {
    
      "Goals p90 = Goals per 90 minutes | NP xG p90 = Non-penalty Expected Goals per 90 minutes | Asts p90 = Assists per 90 minutes | xA p90 = Expected Assists per 90 minutes"
      
    } else {
      
    }
    
    
  })
  
  output$metricsText <- renderText({ metricsText() })
  
  reactive_text <- eventReactive( input$showPlot, {

    "The above plot is called a 'Radar Plot'. The plot has three circles - minimum(inner), average(mid), maximum(outer).\nEach player is
    characterized by the metric. For example, if a player is in the outermost circle(maximum) for goals, the player can\n
    be termed as a 'Good Goalscorer'. Please note that this app is in a developmental stage and will be constantly\n
    updated to provide the best features."

  })
  
  output$aboutNote <- renderText({ reactive_text() })
  

  output$plotSelector <- renderUI({
    
    selectInput(
      inputId = "plotOption",
      label = "Select the type of viz: ",
      choices = c("Age Groups Comparison",
                  "Squad Profile", "Forwards Profile", "Compare Forwards", "Attacking Contribution"),
      selected = TRUE
    )
    
  })
  
  output$leagueSelector <- renderUI({
    selectInput(
      inputId = "league",
      label = if( input$plotOption == "Compare Forwards" && length(input$plotOption) > 0  ) 
        { "Select Player 1 League: " } else { "Select the League:" },
      choices = leagueNames,
      selected = TRUE
    )
  })
  
  output$teamSelector <- renderUI({

      
    if(input$plotOption != "Attacking Contribution" && length(input$plotOption) > 0 &&
       input$plotOption != "Age Groups Comparison" && length(input$plotOption) > 0) {
      
      teamList <- subset(big_5_combined$Squad,big_5_combined$league == input$league)
      
      selectInput(
        inputId = "team",
        label = if( input$plotOption == "Compare Forwards" && length(input$plotOption) > 0 ) { "Select Player 1 Team: " } else { "Select the Team:" },
        choices = teamList,
        selected = TRUE
      ) 
      
    }
      
  })

  output$playerOneSelector <-  renderUI({
    
    if( input$plotOption == "Compare Forwards" && length(input$plotOption) > 0 ){
      
      data <- subset(big_5_combined$Player, big_5_combined$Squad == input$team & big_5_combined$Pos == "FW")
      
      selectInput(
        inputId = "playerOne",
        label = "Select Player 1:",
        choices = data,
        selected = TRUE
      )  
      
    }
  })
  
  
  output$leagueTwoSelector <-  renderUI({
    
    if(input$plotOption == "Compare Forwards" && length(input$plotOption) > 0){
      
      selectInput(
        inputId = "leagueTwo",
        label = "Select Player 2 League:",
        choices = leagueNames,
        selected = TRUE
      )  
      
    }
  })
  
  output$teamTwoSelector <-  renderUI({
    
    req(input$leagueTwo)
    
    if(input$plotOption == "Compare Forwards" && length(input$plotOption) > 0){
      
      data <- big_5_combined %>%
        filter(league == input$leagueTwo) %>%
        select(Squad)
      
      selectInput(
        inputId = "teamTwo",
        label = "Select Team 2:",
        choices = data,
        selected = TRUE
      )  
      
    }
  })
  
  output$playerTwoSelector <-  renderUI({
    
    req(input$playerOne)
    req(input$teamTwo)
    req(input$leagueTwo)
    
    if( input$plotOption == "Compare Forwards" && length(input$plotOption) > 0 ){
      
      data <- subset(big_5_combined$Player,
                     big_5_combined$Squad == input$teamTwo & big_5_combined$Pos == "FW" &
                       big_5_combined$Player != input$playerOne)
      
      selectInput(
        inputId = "playerTwo",
        label = "Select Player 2:",
        choices = data,
        selected = TRUE
      )  
      
    }
  })
  
  output$attributeSelector <-  renderUI({
    
    if( ( length(input$plotOption) > 0 && input$plotOption == "Forwards Profile" ) | ( length(input$plotOption) > 0 && input$plotOption == "Compare Forwards" ) ){
      
      selectInput(
        inputId = "attributeOption",
        label = "Select the attribute: ",
        choices = c("Team Leaders","Goal contributions"),
        selected = TRUE
      )
    } 
    
  })
  
  ############
  
  scatterPlot <- reactive({
    
    
    req(input$league)
    
    data <- big_5_combined %>%
      mutate(Squad = iconv(Squad, "LATIN1", "ASCII//TRANSLIT"),
             Player = iconv(Player, "LATIN1", "ASCII//TRANSLIT")) %>%
      filter(league == input$league,
             Pos %in% c("MF", "FW"),
             Min > 350) %>%
      ungroup() %>%
      select(Player, Min, MP, Starts, Gls, Ast, Glsp90, Astp90)
    
    ggplot(data)+
      geom_point_interactive(aes(x = Glsp90,
                                 y = Astp90,
                                 tooltip = Player,
                                 data_id = Player),
                             size = 3)
      
    
  })
  
  
  selected_state <- reactive({
    input$interactivePlot_selected
  })
  output$console <- renderPrint({
    input$interactivePlot_hovered
  })
  
  output$interactivePlot <- renderGirafe({
    
    
    girafe(code = print(scatterPlot()),
           width_svg = 6, height_svg = 5,
           options = list(
             opts_hover(css = "fill:#FF3333;
                               stroke:black;
                               cursor:pointer",
                        reactive = TRUE),
             opts_selection(type = "multiple",
                            css = "fill:#FF3333;
                                   stroke:black;")
           ))
    
  })
  
  
  output$datatab <- renderTable({
    out <- big_5_combined[big_5_combined$Player %in% selected_state(),]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    out
  })
  
  
  
  ###########
  
  ###########
  
  # agePlot <- reactive({
  #   
  #   req(input$team)
  #   
  #   data = big_5_combined %>%
  #     filter(Squad == input$team)
  #   
  #   ggplot(data) +
  #     geom_point_interactive(aes(x = Age, 
  #                    y = percent_mins,
  #                    fill = Pos,
  #                    data_id = Pos),
  #                pch = 21,
  #                size = 5) +
  #     geom_text_repel(aes(label = Player,
  #                         x = Age,
  #                         y = percent_mins),
  #                     color = "ivory1",
  #                     size = 3,
  #                     fontface = "bold",
  #                     family = "Tahoma",
  #                     box.padding = unit(0.5, "lines")) +
  #     geom_vline(xintercept = first(subset(big_5_combined$mean_league_age,
  #                                          big_5_combined$league == input$league)),
  #                linetype = 2,
  #                alpha = 0.5,
  #                color = "ivory1") +
  #     scale_fill_manual(values= c("#f0e442", "indianred1",
  #                                 "#88ccee", "mediumseagreen")) +
  #     scale_y_continuous(labels = function(x) paste0(x, "%"),
  #                        limits = c(0,100),
  #                        breaks = seq(0, 100, 25)) +
  #     labs(caption = "Twitter: @VenkyReddevil",
  #          title = paste0(input$team," | ",
  #                         "<i style = 'color: darkorange'>Mean Squad Age = </i>", 
  #                         "<i style = 'color: darkorange'>", 
  #                         round(first(subset(big_5_combined$mean_squad_age,
  #                                            big_5_combined$Squad == input$team)),
  #                               2),
  #                         "</i>"),
  #          subtitle = "<span style = 'color: #88ccee'>Goalkeepers</span> ,
  #            <span style = 'color: #f0e442'> Defenders</span> , 
  #            <span style = 'color: mediumseagreen'> Midfielders</span> ,
  #            <span style = 'color: indianred1'> Forwards</span> | 
  #            The dotted line indicates the Weighted Mean League Age",
  #          x = "Age",
  #          y = "% of team minutes played this season (so far)") +
  #     theme(plot.background = element_rect(fill = "gray22",
  #                                          color = "gray22"),
  #           panel.background = element_rect(fill = "gray22"),
  #           plot.margin = margin(0, 0, 0, 0, "cm"),
  #           panel.border = element_rect(fill = NA, color = "ivory1", size = 1),
  #           text = element_text(color = "ivory1", family = "Tahoma", face = "bold"),
  #           strip.text = element_text(color = "ivory1", family = "Tahoma",
  #                                     face = "bold", size = 18),
  #           strip.background = element_rect(fill = "gray22"),
  #           panel.grid.major.x = element_blank(),
  #           panel.grid.major.y = element_blank(),
  #           axis.title = element_text(color = "ivory1",
  #                                     size = 10),
  #           panel.grid.minor = element_blank(),
  #           legend.position = "none",
  #           legend.title = element_blank(),
  #           legend.key = element_blank(),
  #           axis.text = element_text(color = "ivory1",
  #                                    size = 10),
  #           legend.background = element_rect(fill = "gray22"),
  #           legend.text = element_text(size = 14,
  #                                      family = "Tahoma",
  #                                      face = "bold",
  #                                      color = "ivory1"),
  #           plot.title = element_markdown(size = 20, face = "bold",
  #                                         family = "Tahoma",
  #                                         margin = margin(5, 0, 5,0, unit = "pt")),
  #           plot.subtitle = element_markdown(size = 12, face = "bold",
  #                                            family = "Tahoma", color = "ivory1"),
  #           plot.caption = element_text(face = "bold.italic", family="Tahoma", 
  #                                       size = 10))
  #   
  # })
  # 
  # 
  # selected_state <- reactive({
  #   input$ageinteractivePlot_selected
  # })
  # output$console <- renderPrint({
  #   input$ageinteractivePlot_hovered
  # })
  # 
  # output$ageinteractivePlot <- renderGirafe({
  #   
  #   
  #   girafe(code = print(agePlot()),
  #          width_svg = 10, height_svg = 7,
  #          options = list(
  #            opts_hover(css = "stroke:black;
  #                              cursor:pointer",
  #                       reactive = TRUE),
  #            opts_selection(type = "multiple",
  #                           css = "fill:#ffffff;
  #                                  stroke:black;
  #                                  r: 5pt")
  #          ))
  #   
  # })
  # 
  # 
  # output$agedatatab <- renderTable({
  #   
  #   req(input$team)
  #   
  #   out <- big_5_combined %>%
  #     filter(Pos %in% selected_state(),
  #            Squad == input$team)
  #   if( nrow(out) < 1 ) return(NULL)
  #   row.names(out) <- NULL
  #   out
  # })
  # 
  
  
  ###########
  
  ##########

  # ageGroupPlot <- reactive({
  #
  #   req(input$league)
  #
  #   data = big_5_combined %>%
  #     filter(league == input$league) %>%
  #     select(Player, Squad, Age, Min, age_group) %>%
  #     group_by(Squad, age_group) %>%
  #     summarise(total_mins = sum(Min)) %>%
  #     group_by(Squad) %>%
  #     mutate(percent_mins = round(total_mins / sum(total_mins) * 100, 2))
  #
  #   ggplot(data,
  #          aes(x = age_group,
  #              y = percent_mins,
  #              fill = age_group)) +
  #     geom_bar_interactive(stat = "identity") +
  #     scale_y_continuous(limits = c(0,80),
  #                        breaks = seq(0,80,20)) +
  #     facet_wrap(~Squad, ncol = 5, nrow = 4) +
  #     scale_fill_manual_interactive(
  #       values = c("Under-23" = "darksalmon", "23-25 years" = "indianred1",
  #                   "26-29 years" = "coral3", ">= 30 years" = "burlywood1"),
  #       data_id = c("Under-23" =  "Under-23", "23-25 years" = "23-25 years",
  #                   "26-29 years" = "26-29 years", ">= 30 years" = ">= 30 years")
  #     ) +
  #     coord_cartesian(clip = "off") +
  #     labs(x = "",
  #          y = "Percentage of minutes played",
  #          title = "Premier League 2020/21 - Squad Age Profile",
  #          subtitle = "( Percentage of minutes played by every age group ) - Players' age was recorded at the start of the season",
  #          caption = "@VenkyReddevil") +
  #     theme_minimal() +
  #     theme(panel.background = element_rect(fill = "gray27"),
  #           plot.background = element_rect(fil = "gray27"),
  #           panel.border = element_rect(fill = NA, color =  "navajowhite3"),
  #           panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           axis.text.x = element_blank(),
  #           text = element_text(family = "Rockwell", color = "navajowhite2"),
  #           strip.text = element_text(color = "navajowhite2",
  #                                     size =14, face ="bold.italic"),
  #           axis.text = element_text(color = "navajowhite2"),
  #           axis.title.y = element_text(margin = margin(0,10,0,10), size = 14),
  #           legend.title = element_blank(),
  #           legend.position = "top",
  #           # legend.margin = margin(20,0,0,0),
  #           legend.text = element_text(size = 14),
  #           plot.title = element_text(size = 20, face = "bold"),
  #           plot.subtitle = element_text(size = 12, face = "italic"),
  #           plot.caption = element_text(face = "italic"))
  #
  # })
  #
  #
  # selected_keys <- reactive({
  #   input$ageGroupInteractivePlot_key_selected
  # })
  #
  # output$ageGroupInteractivePlot <- renderGirafe({
  #
  #
  #   girafe(code = print(ageGroupPlot()),
  #          width_svg = 10, height_svg = 7,
  #          options = list(
  #            opts_hover(css = "stroke:black;
  #                              cursor:pointer",
  #                       reactive = TRUE),
  #            opts_selection(type = "single"),
  #            opts_selection_key(css = "stroke:black;r:5pt;"),
  #            opts_hover(css = "fill:wheat;stroke:black;stroke-width:3px;cursor:pointer;"),
  #            opts_hover_key(css = "stroke:black;r:5pt;cursor:pointer;")
  #          ))
  #
  # })
  #
  #
  # output$agegroupdatatab <- renderDT({
  #
  #   req(input$league)
  #
  #   out <- big_5_combined %>%
  #     filter(age_group %in% selected_keys(),
  #            league == input$league) %>%
  #     select(Squad, Player, Age, Min, Starts, MP) %>%
  #     arrange(Squad)
  #
  #   if( nrow(out) < 1 ) return(NULL)
  #   row.names(out) <- NULL
  #
  #   datatable(out, rownames = FALSE) %>%
  #     formatStyle(c('Squad', 'Player', 'Age', 'Min', 'Starts', 'MP'),
  #                 backgroundColor = '#f0f0e4')
  #
  # })

  ##########

  plot <- eventReactive(input$showPlot, {

    if (input$plotOption == "Age Groups Comparison") {

    data = big_5_combined %>%
      filter(league == input$league) %>%
      select(Player, Squad, Age, Min) %>%
      mutate(age_group = ifelse(Age < 23, "Under-23",
                                ifelse(Age >= 23 & Age < 26, "23-25 years",
                                       ifelse(Age >= 26 & Age < 30, "26-29 years",
                                              ">= 30 years")))) %>%
      group_by(Squad, age_group) %>%
      summarise(total_mins = sum(Min)) %>%
      group_by(Squad) %>%
      mutate(percent_mins = round(total_mins / sum(total_mins) * 100, 2))

    data$age_group = factor(data$age_group,
                            levels = c("Under-23", "23-25 years",
                                       "26-29 years", ">= 30 years"))

    ggplot(data,
           aes(x = age_group,
               y = percent_mins,
               fill = age_group)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(limits = c(0,80),
                         breaks = seq(0,80,20)) +
      facet_wrap(~Squad, ncol = 5, nrow = 4) +
      scale_fill_manual(values = c("darksalmon", "indianred1", "coral3", "burlywood1")) +
      coord_cartesian(clip = "off") +
      labs(x = "",
           y = "Percentage of minutes played",
           title = "Premier League 2020/21 - Squad Age Profile",
           subtitle = "( Percentage of minutes played by every age group ) - Players' age was recorded at the start of the season",
           caption = "@VenkyReddevil") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "gray27"),
            plot.background = element_rect(fil = "gray27"),
            panel.border = element_rect(fill = NA, color =  "navajowhite3"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            text = element_text(family = "Rockwell", color = "navajowhite2"),
            strip.text = element_text(color = "navajowhite2",
                                      size =14, face ="bold.italic"),
            axis.text = element_text(color = "navajowhite2"),
            axis.title.y = element_text(margin = margin(0,10,0,10), size = 14),
            # panel.spacing = unit(2, "lines"),
            legend.title = element_blank(),
            # legend.position = "bottom",
            legend.margin = margin(20,0,0,0),
            legend.text = element_text(size = 14),
            plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 12, face = "italic"),
            plot.caption = element_text(face = "italic"))

    }

    else if(input$plotOption == "Squad Profile"){

      data = big_5_combined %>%
        filter(Squad == input$team)

      ggplot(data) +
        geom_point(aes(x = Age,
                       y = percent_mins,
                       fill = Pos),
                   pch = 21,
                   size = 5) +
        geom_text_repel(aes(label = Player,
                            x = Age,
                            y = percent_mins),
                        color = "ivory1",
                        size = 3,
                        fontface = "bold",
                        family = "Tahoma",
                        box.padding = unit(0.5, "lines")) +
        geom_vline(xintercept = first(subset(big_5_combined$mean_league_age,
                                             big_5_combined$league == input$league)),
                   linetype = 2,
                   alpha = 0.5,
                   color = "ivory1") +
        scale_fill_manual(values= c("#f0e442", "indianred1",
                                    "#88ccee", "mediumseagreen")) +
        scale_y_continuous(labels = function(x) paste0(x, "%"),
                           limits = c(0,100),
                           breaks = seq(0, 100, 25)) +
        labs(caption = "Twitter: @VenkyReddevil",
             title = paste0(input$team," | ",
                            "<i style = 'color: darkorange'>Mean Squad Age = </i>",
                            "<i style = 'color: darkorange'>",
                            round(first(subset(big_5_combined$mean_squad_age,
                                               big_5_combined$Squad == input$team)),
                                  2),
                            "</i>"),
             subtitle = "<span style = 'color: #88ccee'>Goalkeepers</span> ,
             <span style = 'color: #f0e442'> Defenders</span> ,
             <span style = 'color: mediumseagreen'> Midfielders</span> ,
             <span style = 'color: indianred1'> Forwards</span> |
             The dotted line indicates the Weighted Mean League Age",
             x = "Age",
             y = "% of team minutes played this season (so far)") +
        theme(plot.background = element_rect(fill = "gray22",
                                             color = "gray22"),
              panel.background = element_rect(fill = "gray22"),
              # plot.margin = margin(5, 5, 5, 5, "pt"),
              panel.border = element_rect(fill = NA, color = "ivory1", size = 1),
              text = element_text(color = "ivory1", family = "Tahoma", face = "bold"),
              strip.text = element_text(color = "ivory1", family = "Tahoma",
                                        face = "bold", size = 18),
              strip.background = element_rect(fill = "gray22"),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.title = element_text(color = "ivory1",
                                        size = 10),
              panel.grid.minor = element_blank(),
              legend.position = "none",
              legend.title = element_blank(),
              legend.key = element_blank(),
              axis.text = element_text(color = "ivory1",
                                       size = 10),
              legend.background = element_rect(fill = "gray22"),
              legend.text = element_text(size = 14,
                                         family = "Tahoma",
                                         face = "bold",
                                         color = "ivory1"),
              plot.title = element_markdown(size = 20, face = "bold",
                                            family = "Tahoma",
                                            margin = margin(5, 0, 5,0, unit = "pt")),
              plot.subtitle = element_markdown(size = 12, face = "bold",
                                               family = "Tahoma", color = "ivory1"),
              plot.caption = element_text(face = "bold.italic", family="Tahoma",
                                          size = 10))
    }
    
    else if(input$plotOption == "Forwards Profile" && input$attributeOption == "Team Leaders") {

      data = big_5_combined %>%
        filter(Pos == "FW",
               Squad == input$team,
               Min >= 350) %>%
        ungroup() %>%
        select(Player, Glsp90, Astp90, npxGp90, xAp90) %>%
        rename(group = Player) %>%
        mutate_at(vars(-group),
                  funs(rescale)) %>%
        relocate(group, Glsp90, npxGp90) %>%
        rename("Goals p90" = "Glsp90",
               "NP\nxG\np90" = "npxGp90",
               "Asts p90" = "Astp90",
               "xA\np90" = "xAp90")

      ggradar2(data,
               webtype = "mini",
               grid.label.size = 4,
               axis.label.size = 3,
               group.point.size = 3,
               group.line.width = 1.5,
               background.circle.colour = "gray",
               background.circle.transparency = 0.2,
               legend.text.size= 10,
               label.gridline.min = "",
               label.gridline.mid = "",
               label.gridline.max = "",
               gridline.min.linetype = 1,
               gridline.max.linetype = 1,
               gridline.mid.linetype = 1,
               axis.label.offset = 1.15,
               group.colours = c("#dd4124", "#ed8b00", "#00496f"),
               gridline.min.colour = "#4a3a3b",
               gridline.mid.colour = "#4a3a3b",
               gridline.max.colour = "#4a3a3b",
               grid.line.width = 0.5) +
      facet_wrap(~group, ncol = 3) +
      labs(title = paste0("A comparison of the different Forwards - ", input$team),
           subtitle = "( 2020/21 - Minimum 350 minutes )",
           caption = "Visualization by Venkatanarayanan/@VenkyReddevil") +
      theme(plot.background = element_rect(fill = "#f0f0e4",
                                           color = "#f0f0e4"),
            panel.background = element_rect(fill = "#f0f0e4",
                                            color = "#f0f0e4"),
            panel.spacing.x=unit(2, "lines"),
            panel.spacing.y=unit(2, "lines"),
            text = element_text(color = "gray22", family = "Tahoma", face = "bold"),
            strip.text = element_text(color = "gray22", family = "Tahoma",
                                      face = "bold", size = 14),
            legend.position = "none",
            strip.background = element_blank(),
            plot.title = element_text(size = 20, face = "bold",
                                      family = "Tahoma", hjust = 0.5,
                                      margin = margin(10, 0, 10,0, unit = "pt"),
                                      color = "#4a3a3b"),
            plot.subtitle = element_text(size = 12, face = "bold.italic",
                                         family = "Tahoma", hjust = 0.5),
            plot.caption = element_text(face = "bold.italic", family="Tahoma", 
                                        size = 10, hjust = 0.5))

    }

   
    
    else if(input$plotOption == "Forwards Profile" && input$attributeOption == "Goal contributions") {
    
      
      data <- big_5_combined %>%
        filter(Min >= 350) %>%
        select(league, Player, Pos, Squad, Glsp90, Astp90, npxGp90, xAp90) %>%
        group_by(league) %>%
        mutate_at(vars(5:8),
                  ~round(percent_rank(.) * 100, 2)) %>%
        filter(Pos == "FW",
               Squad == input$team) %>%
        gather(key, value, -c("league", "Player", "Pos", "Squad"))
      
      
      ggplot(data,
             aes(x = key,
                 y = value,
                 fill = Player)) +
        geom_bar(stat = "identity",
                 width = 1,
                 alpha = 0.5) +
        geom_hline(yintercept = 100,
                   color = "navajowhite3",
                   alpha = 1) +
        geom_hline(yintercept = seq(0, 90, 10),
                   color = "navajowhite3",
                   alpha = 0.2) +
        geom_vline(xintercept = seq(1.5,4.5, 1),
                   color = "navajowhite3") +
        scale_y_continuous(expand = c(0,0),
                           breaks = seq(0,100,10)) +
        facet_grid(~Player, space = "fixed") +
        coord_polar(clip = "off") +
        labs(title= "Percentile ranks in the league - 2020/21",
             caption = "Visual by Venkat / @VenkyReddevil") +
        theme(plot.background = element_rect(fill = "gray22"),
              panel.background = element_rect(fill = "gray22"),
              panel.border = element_rect(fill = NA, color = "gray22", size = 1),
              text = element_text(color = "ivory1", family = "Tahoma", face = "bold"),
              strip.text = element_text(color = "ivory1", family = "Tahoma",
                                        face = "bold", size = 18),
              strip.background = element_rect(fill = "gray22"),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.spacing=unit(0,'npc'),
              panel.grid.minor = element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position = "none",
              legend.title = element_blank(),
              legend.key = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(color = "ivory1"),
              legend.background = element_rect(fill = "gray22"),
              legend.text = element_text(size = 14,
                                         family = "Tahoma",
                                         face = "bold",
                                         color = "ivory1"),
              plot.title = element_text(size = 20, face = "bold",
                                        family = "Tahoma", hjust = 0.5,
                                        margin = margin(10, 0, 10,0, unit = "pt")),
              plot.subtitle = element_text(size = 12, face = "bold.italic",
                                           family = "Tahoma", hjust = 0.5),
              plot.caption = element_text(face = "bold.italic", family="Tahoma",
                                          size = 10, hjust = 0.5))
      
    }
    
    
    else if(input$plotOption == "Compare Forwards" && input$attributeOption == "Goal contributions") {
      
      
      data <- big_5_combined %>%
        filter(Min >= 350) %>%
        select(league, Player, Pos, Squad, Glsp90, Astp90, npxGp90, xAp90) %>%
        group_by(league) %>%
        mutate_at(vars(5:8),
                  ~round(percent_rank(.) * 100, 2)) %>%
        filter(Player %in% c(input$playerOne, input$playerTwo)) %>%
        gather(key, value, -c("league", "Player", "Pos", "Squad"))
      
      
      ggplot(data,
             aes(x = key,
                 y = value,
                 fill = Player)) +
        geom_bar(stat = "identity",
                 width = 1,
                 alpha = 0.5) +
        geom_hline(yintercept = 100,
                   color = "navajowhite3",
                   alpha = 1) +
        geom_hline(yintercept = seq(0, 90, 10),
                   color = "navajowhite3",
                   alpha = 0.2) +
        geom_vline(xintercept = seq(1.5,4.5, 1),
                   color = "navajowhite3") +
        scale_y_continuous(expand = c(0,0),
                           breaks = seq(0,100,10)) +
        facet_grid(~Player, space = "fixed") +
        coord_polar(clip = "off") +
        labs(title= "Percentile ranks in the league - 2020/21",
             caption = "Visual by Venkat / @VenkyReddevil") +
        theme(plot.background = element_rect(fill = "gray22"),
              panel.background = element_rect(fill = "gray22"),
              panel.border = element_rect(fill = NA, color = "gray22", size = 1),
              text = element_text(color = "ivory1", family = "Tahoma", face = "bold"),
              strip.text = element_text(color = "ivory1", family = "Tahoma",
                                        face = "bold", size = 18),
              strip.background = element_rect(fill = "gray22"),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.spacing=unit(0,'npc'),
              panel.grid.minor = element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position = "none",
              legend.title = element_blank(),
              legend.key = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(color = "ivory1"),
              legend.background = element_rect(fill = "gray22"),
              legend.text = element_text(size = 14,
                                         family = "Tahoma",
                                         face = "bold",
                                         color = "ivory1"),
              plot.title = element_text(size = 20, face = "bold",
                                        family = "Tahoma", hjust = 0.5,
                                        margin = margin(10, 0, 10,0, unit = "pt")),
              plot.subtitle = element_text(size = 12, face = "bold.italic",
                                           family = "Tahoma", hjust = 0.5),
              plot.caption = element_text(face = "bold.italic", family="Tahoma",
                                          size = 10, hjust = 0.5))
      
    }
    
  })
  
  output$plot <- renderPlot({
    plot()
  }, bg = "transparent")

  dataTable <- eventReactive(input$showPlot,{
    
    if (input$plotOption == "Forwards Profile" && 
        input$attributeOption == "Goal contributions" &&
        length(input$plotOption) > 0 &&
        length(input$attributeOption) > 0){
      data <- big_5_combined %>%
        filter(Min >= 350,
               Pos == "FW",
               Squad == input$team) %>%
        select(Player, Glsp90, npxGp90, Astp90, xAp90)
      
      datatable(data, rownames = FALSE) %>%
        formatStyle(c('Player', 'Glsp90', 'Astp90', 'xAp90', 'npxGp90', 'Squad'),
                    backgroundColor = "#f0f0e4")
      
    } else if(input$plotOption == "Compare Forwards" &&
              input$attributeOption == "Goal contributions" &&
              length(input$plotOption) > 0 &&
              length(input$attributeOption) > 0) {
      
      data <- big_5_combined %>%
        filter(Player %in% c(input$playerOne, input$playerTwo)) %>%
        select(Player, Glsp90, npxGp90, Astp90, xAp90)
      
      datatable(data)
      
    } else {
      
    }

   

  })
  
  output$dataTable <- renderDT({
    
    dataTable()

 
  })
}