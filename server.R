function(input, output) {
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
    I have not included the 'days' and only included 'years' while calculating the mean.\nSome letters are also
    not encoded properly. I am working on the same and the issue will be resolved soon\n.Please feel free to suggest anything"
  })
  
  output$leagueSelector <- renderUI({
    selectInput(
      inputId = "league",
      label = "Select the League: ",
      choices = leagueNames,
      selected = TRUE
    )
  })
  
  output$teamSelector <- renderUI({
    
    teamList <- subset(iconv(big_5_combined$Squad, "LATIN1", "ASCII//TRANSLIT"),
                       big_5_combined$league == input$league)
    
    selectInput(
      inputId = "team",
      label = "Select the team: ",
      choices = teamList,
      selected = TRUE
    )
  })
  
  output$plotSelector <- renderUI({
    
    selectInput(
      inputId = "plotOption",
      label = "Select the type of viz: ",
      choices = c("Squad Profile"),#, "Players' xG vs Goals", "Forwards Profile"),
      selected = TRUE
    )
    
  })
  
  plot <- eventReactive(input$showPlot, {

    if(input$plotOption == "Squad Profile"){
      
      data = big_5_combined %>% filter(iconv(Squad, "LATIN1", "ASCII//TRANSLIT") == input$team)
      
      ggplot(data) +
        geom_point(aes(x = Age, 
                       y = percent_mins,
                       color = Pos),
                   size = 4) +
        geom_text_repel(aes(label = iconv(Player, "LATIN1", "ASCII//TRANSLIT"),
                            x = Age,
                            y = percent_mins),
                        color = "ivory1",
                        size = 3,
                        box.padding = unit(0.5, "lines")) +
        geom_vline(xintercept = first(subset(big_5_combined$mean_league_age,
                                             big_5_combined$league == input$league)),
                   linetype = 2,
                   alpha = 0.5,
                   color = "ivory1") +
        geom_text(aes(label = "Mean league age",
                      x = first(subset(big_5_combined$mean_league_age,
                                       big_5_combined$league == input$league)),
                      y = 20),
                  size = 3,
                  angle = 90,
                  color = "ivory1",
                  alpha = 0.5) +
        scale_color_manual(values = c("#3cb371", "#f9e211",
                                      "#88ccee", "#d04e59")) +
        labs(caption = "Visualisation by Venkatanarayanan / @VenkyReddevil",
             title = input$team,
             subtitle = paste0("Mean Squad Age", " = ", round(first(subset(big_5_combined$mean_squad_age,
                                                                           big_5_combined$Squad == input$team)),
                                                              2)),
             x = "Age",
             y = "% of team minutes played this season (so far)") +
        theme(plot.background = element_rect(fill = "gray22"),
              panel.background = element_rect(fill = "gray22"),
              panel.border = element_rect(fill = NA, color = "ivory1", size = 1),
              text = element_text(color = "ivory1", family = "Georgia", face = "bold"),
              strip.text = element_text(color = "ivory1", family = "Georgia",
                                        face = "bold", size = 18),
              strip.background = element_rect(fill = "gray22"),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.title = element_text(color = "ivory1",
                                        size = 10),
              panel.grid.minor = element_blank(),
              legend.position = "right",
              legend.title = element_blank(),
              legend.key = element_blank(),
              axis.text = element_text(color = "ivory1",
                                       size = 10),
              legend.background = element_rect(fill = "gray22"),
              legend.text = element_text(size = 14,
                                         family = "Georgia",
                                         face = "bold",
                                         color = "ivory1"),
              plot.title = element_text(size = 20, face = "bold",
                                        family = "Georgia", hjust = 0.5,
                                        margin = margin(10, 0, 10,0, unit = "pt")),
              plot.subtitle = element_text(size = 12, face = "bold.italic",
                                           family = "Georgia", hjust = 0.5),
              plot.caption = element_text(face = "bold.italic", family="Georgia", 
                                          size = 10))
    }
    
    # else if(input$plotOption == "Forwards Profile") {
    #   
    #   
    #   data = big_5_combined %>%
    #     mutate(Player = iconv(Player, "LATIN1", "ASCII//TRANSLIT")) %>%
    #     filter(Pos == "FW",
    #            Squad == input$team,
    #            Min >= 350) %>%
    #     select(Player, Glsp90, Astp90, npxGp90, xAp90) %>%
    #     rename(group = Player) %>%
    #     mutate_at(vars(-group),
    #               funs(rescale))
    # 
    #   ggradar(data,
    #           grid.label.size = 4,
    #           axis.label.size = 4,
    #           values.radar = c("0%", "50%","100%"),
    #           group.point.size = 5,
    #           group.line.width = 1.5,
    #           legend.text.size= 10) +
    #   facet_wrap(~group) +
    #   theme(legend.position = "none")
    # 
    # }
    # 
    # else {
    #   
    #   data = big_5_combined %>%
    #     filter(iconv(Squad, "LATIN1", "ASCII//TRANSLIT") == input$team,
    #            grepl("FW|MF", Pos),
    #            Gls > 0) %>%
    #     select(Player, Squad, Gls, xG) %>%
    #     gather(key, value, -c(Player, Squad)) %>%
    #     mutate(value = ifelse(key == "Gls", value, -value))
    #   
    #   ggplot(data,
    #          aes(x = iconv(Player, "LATIN1", "ASCII//TRANSLIT"),
    #              y = value,
    #              fill = key)) +
    #     geom_bar(stat = "identity", width = .6) +
    #     # geom_vline(xintercept) +
    #     coord_flip() +
    #     scale_y_continuous(breaks = seq(-6,6,2),
    #                        labels = abs(seq(-6,6,2))) +
    #     labs(x = "Player",
    #          y = "xG vs Goals") +
    #     theme(plot.background = element_rect(fill = "gray22"),
    #           panel.background = element_rect(fill = "gray22"),
    #           panel.border = element_rect(fill = NA, color = "ivory1", size = 1),
    #           text = element_text(color = "ivory1", family = "Georgia", face = "bold"),
    #           strip.text = element_text(color = "ivory1", family = "Georgia",
    #                                     face = "bold", size = 18),
    #           strip.background = element_rect(fill = "gray22"),
    #           panel.grid.major.x = element_blank(),
    #           panel.grid.major.y = element_blank(),
    #           axis.title = element_text(color = "ivory1",
    #                                     size = 10),
    #           panel.grid.minor = element_blank(),
    #           legend.position = "right",
    #           legend.title = element_blank(),
    #           legend.key = element_blank(),
    #           axis.text = element_text(color = "ivory1",
    #                                    size = 10),
    #           legend.background = element_rect(fill = "gray22"),
    #           legend.text = element_text(size = 14,
    #                                      family = "Georgia",
    #                                      face = "bold",
    #                                      color = "ivory1"),
    #           plot.title = element_text(size = 20, face = "bold",
    #                                     family = "Georgia", hjust = 0.5,
    #                                     margin = margin(10, 0, 10,0, unit = "pt")),
    #           plot.subtitle = element_text(size = 12, face = "bold.italic",
    #                                        family = "Georgia", hjust = 0.5),
    #           plot.caption = element_text(face = "bold.italic", family="Georgia", 
    #                                       size = 10))
    #   
    # }
   
    
  })
  
  output$plot <- renderPlot({
      plot()
    })  
}