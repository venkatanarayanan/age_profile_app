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





