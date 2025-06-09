# Load required libraries
library(peruopen)
library(tidyverse)

# Load configuration
config <- config::get()

# Get catalog
catalogo <- po_catalog()

# Search for epidemiological surveillance data using diseases from config
search <- po_search(
  query = "vigilancia epidemio",
  tags = config$diseases
)

# Download selected resources
data <- search$resources |>
  as_tibble() |>
  slice(2:3) |>
  po_get()

# Combine datasets
data <- bind_rows(data)

# Process and aggregate data
data_agg <- data |>
  filter(
    ano %in% config$years,
    tipo_dx == "C",
    departamento == config$geography$region
    ) |>
  group_by(
    departamento, provincia, distrito, enfermedad, diagnostic,
    diresa, ubigeo, ano
  ) |>
  summarise(casos = n()) |>
  arrange(.by_group = TRUE)
