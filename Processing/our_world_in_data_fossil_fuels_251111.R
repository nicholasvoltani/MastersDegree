library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

# Analysis of data in https://ourworldindata.org/fossil-fuels (Our World in Data)


df <- read_csv("C:/Users/nicho/Downloads/global-fossil-fuel-consumption.csv")

df <- df %>% 
  mutate(total_twh_direct_energy = gas__twh_direct_energy + oil__twh_direct_energy + coal__twh_direct_energy,
         change = c(0, diff(total_twh_direct_energy)),
         change_yoy = change / lag(total_twh_direct_energy))

# Filter the years where change is negative
neg_years <- df %>% filter(change < 0)

ggplot(df, aes(x = Year, y = total_twh_direct_energy)) +
  geom_line() +
  geom_vline(data = neg_years,
             aes(xintercept = Year, color = factor(Year)),
             linetype = "dotted", size = 1) +
  scale_color_manual(
    name = "Years with negative change (YoY)",      # legend title
    values = rep("red", nrow(neg_years))      # all vertical lines red
  ) +
  theme_minimal() +
  labs(x = "Year", y = "Total energy (TWh)")

plot(df$Year, df$change_yoy)


# Filter the years where change_yoy is negative
neg_years <- df %>% filter(change_yoy < 0)

ggplot(df, aes(x = Year, y = change_yoy)) +
  geom_line() +
  geom_vline(data = neg_years,
             aes(xintercept = Year, color = factor(Year)),
             linetype = "dotted", size = 1) +
  scale_color_manual(
    name = "Years with negative change (YoY)",      # legend title
    values = rep("red", nrow(neg_years))      # all vertical lines red
  ) +
  theme_minimal() +
  labs(x = "Year", y = "Change in Total Energy (YoY)")

