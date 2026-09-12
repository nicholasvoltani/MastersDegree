library(eia)
library(ggplot2)
library(tidyverse)

eia_set_key("AbquS9drO6wdfeNDIjjLhe8Q3TCL97eNfDKBy0ly")

# eia_dir()

eia_metadata("electricity/retail-sales")
eia_facets("electricity/retail-sales", facet = "sectorid")


(df <- eia_data(
  dir = "electricity/retail-sales",
  data = "sales",
  # facets = list(stateid = "OH", sectorid = "RES"),
  freq = "annual",
  start = "2000",
  sort = list(cols = "period", order = "asc"),
))

df <- df |>
  mutate(sales = as.numeric(sales))

df_merged <- df |>
  group_by(period, sectorName) |>
  summarise(total_sales = sum(sales, na.rm = TRUE))


ggplot(
  df_merged |> filter(sectorName != "all sectors"),
  aes(x = period, y = total_sales / 1e3)
) +
  geom_bar(
    aes(fill = sectorName),
    stat = "identity",
    position = position_stack(reverse = TRUE)
  ) +
  theme(legend.position = "top") +
  # theme_bw() +
  labs(
    title = "Annual Retail Sales of Electricity (GWh)",
    # subtitle = "State: Ohio; Sector: Residential",
    x = "Year",
    y = "Sales (GWh)"
  )
