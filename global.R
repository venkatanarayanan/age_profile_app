library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(extrafont)
library(ggrepel)
library(tidyverse)
library(magrittr)
library(scales)
library(ggradar2)
library(DT)
library(ggtext)
library(gghighlight)
library(ggiraph)
library(ggnewscale)

big_5_combined <- read.csv("big_5_data.csv")

big_5_combined %<>%
  filter(!is.na(league))

big_5_combined <- big_5_combined[,-1]

big_5_combined$Pos <- str_replace(big_5_combined$Pos, "DF,FW", "DF")
big_5_combined$Pos <- str_replace(big_5_combined$Pos, "DF,MF", "DF")
big_5_combined$Pos <- str_replace(big_5_combined$Pos, "MF,FW", "MF")
big_5_combined$Pos <- str_replace(big_5_combined$Pos, "MF,DF", "MF")
big_5_combined$Pos <- str_replace(big_5_combined$Pos, "FW,DF", "FW")
big_5_combined$Pos <- str_replace(big_5_combined$Pos, "FW,MF", "FW")


big_5_combined %<>%
  group_by(Squad) %>%
  mutate(category = ifelse(Gls == max(Gls), "Most Goals",
                           ifelse(Min == max(Min), "Most Minutes",
                                  ifelse(Ast == max(Ast), "Most Assists", "Normal"))),
         Player = iconv(Player, "LATIN1", "ASCII//TRANSLIT"),
         Squad = iconv(Squad, "LATIN1", "ASCII//TRANSLIT"))


big_5_combined$Player <- gsub("'", "", big_5_combined$Player)

big_5_combined %<>%
  mutate(age_group = ifelse(Age < 23, "Under-23",
                            ifelse(Age >= 23 & Age < 26, "23-25 years",
                                   ifelse(Age >= 26 & Age < 30, "26-29 years",
                                          ">= 30 years"))))

leagueNames <- as.character(unique(big_5_combined$league))



max_age_group_team <- big_5_combined %>%
    count(Squad, age_group) %>%
    slice(which.max(n)) %>%
    rename("max_age_group" = "age_group") %>%
    mutate(min_years = ifelse(max_age_group == "Under-23", NA,
                            ifelse(max_age_group == "23-25 years", 23 , 
                                   ifelse(max_age_group == "26-29 years", 26,
                                          30))),
           max_years = ifelse(max_age_group == "Under-23", 23,
                            ifelse(max_age_group == "23-25 years", 25 , 
                                   ifelse(max_age_group == "26-29 years", 29,
                                          NA))))


min_max_age_team <- big_5_combined %>%
    filter(Min > 0) %>%
    group_by(Squad) %>%
    summarise(min_age = min(Age), max_age = max(Age))


age_area_data <- max_age_group_team %>%
    left_join(min_max_age_team) %>%
    mutate(min_years = ifelse(is.na(min_years), min_age, min_years),
           max_years = ifelse(is.na(max_years), max_age, max_years))




big_5_combined %<>%
  left_join(age_area_data, by = "Squad")
