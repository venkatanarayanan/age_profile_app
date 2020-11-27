library(shiny)
library(shinydashboard)
library(extrafont)
library(ggrepel)
library(tidyverse)
library(magrittr)
library(scales)
library(ggradar)
library(DT)
library(ggtext)
library(gghighlight)

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
                                  ifelse(Ast == max(Ast), "Most Assists", "Normal"))))

leagueNames <- as.character(unique(big_5_combined$league))





