library(sidrar)
library(dplyr)

# Queries de produtos agrícolas

# Geo: "Brazil" | "Region" (Sul etc.) | "State" | "City" (município) | "MesoRegion" | "MicroRegion"
geo="City"

# Variables
## 109: Área Plantada (ha)
## 216: Área Colhida (ha)
## 214: Qtd Produzida (ton)
## 215: Valor da Produção (Mil x, ..., Mil Reais)
variable = 109

# Classific (culturas)
## 2692: Arroz (em casca)
## 2702: Feijão (grão)
## 2696: Cana de açúcar
## 2711: Milho
## 2713: Soja
## 2715: Tomate
## 2716: Trigo
category=list("2711")

# Category
## c81: "Produto das lavouras temporárias" (única categoria dessa tabla)
classific=list("c81")

area_plantada <- get_sidra(
  1612,
  variable=variable,
  period=c("2019-2024"),
  geo=geo,
  classific=classific,
  category=category
)

