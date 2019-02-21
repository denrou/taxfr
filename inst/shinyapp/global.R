library(cartofr)
library(dplyr)
library(glue)
library(leaflet)
library(sf)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(stringr)
library(taxfr)

data(tax)
data(cartofr)

gini_insee <- tax %>%
  filter(!is.na(municipality), !is.na(postal_code), reference_tax_revenue_per_tranche_in_euros != "Total") %>%
  group_by(year, municipality, postal_code) %>%
  summarise(gini = gini(number_of_households, revenue_of_households - tax_income)) %>%
  right_join(mutate_at(cartofr[["insee"]], "code_insee", as.character), by = c("postal_code" = "code_insee")) %>%
  as_tibble() %>%
  st_as_sf()

gini_dept <- tax %>%
  filter(!is.na(municipality), is.na(postal_code), reference_tax_revenue_per_tranche_in_euros != "Total") %>%
  mutate_at("department_code", function(x) str_sub(as.character(x), end = 2L)) %>%
  group_by(year, municipality, department_code) %>%
  summarise(gini = gini(number_of_households, revenue_of_households - tax_income)) %>%
  right_join(mutate_at(cartofr[["dept"]], "code_dept", as.character), by = c("department_code" = "code_dept")) %>%
  as_tibble() %>%
  st_as_sf()

data_map <- list(dept = gini_dept, insee = gini_insee)
