library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(jsonlite)

# Data from Our World in Data: https://ourworldindata.org/grapher/global-fossil-fuel-consumption

df <- read_csv("https://ourworldindata.org/grapher/fossil-fuel-primary-energy.csv?v=1&csvType=full&useColumnShortNames=true")

usa <- df %>% 
  filter(Entity == "United States")

non_usa <- df %>% 
  filter(Entity != "United States") %>% 
  group_by(Year) %>% 
  summarise(fossil_fuels__twh = sum(fossil_fuels__twh))

total <- df %>% 
  group_by(Year) %>% 
  summarise(fossil_fuels__twh = sum(fossil_fuels__twh))

  
comparison <- usa %>% 
  left_join(total, 
            by="Year",
            suffix = c("_USA", "_total"))

comparison <- comparison %>% 
  mutate(percentage_USA = fossil_fuels__twh_USA / fossil_fuels__twh_total)
