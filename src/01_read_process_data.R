# Load required libraries
library(peruopen)
library(tidyverse)
library(readxl)
library(innovar)
library(viridis)
library(scales)
library(ggthemes)

# Load configuration
config <- config::get()

# Get catalog
catalogo <- po_catalog(extend_existing = TRUE)

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

# Aggregate
data_grouped <- data_agg |>
  ungroup() |>
  select(year = ano, ubigeo, enfermedad, casos) |>
  mutate(
    enfermedad = case_when(
      str_starts(enfermedad, "DENGUE")  ~ "dengue",
      str_starts(enfermedad, "MALARIA") ~ "malaria"
      )
    ) |>
  group_by(year, ubigeo, enfermedad) |>
  summarise(n = sum(casos, na.rm = TRUE))


# Population
pop <- read_excel("data/projections.xlsx",skip = 4) |>
  na.omit() |>
    rename(ubigeo = 1, name = 2)

# Filter loreto
pop_loreto <- pop |>
  filter(str_starts(ubigeo, "16") & !str_ends(ubigeo, "0")) |>
  mutate(across(`2018`:`2025`, as.numeric))

pop_long <- pop_loreto |>
  pivot_longer(`2018`:`2025`,names_to = "year", values_to = "pop") |>
  mutate(year = as.numeric(year))

earlier_years <- 2010:2017

pop_completed <- pop_long %>%
  group_by(ubigeo, name) %>%
  nest() %>%                               # one tibble per district
  mutate(
    model = map(data, ~ lm(pop ~ year, data = .x)),
    preds = map2(model, data, ~ {
      new_years <- setdiff(earlier_years, .y$year)     # skip any year already present
      if(length(new_years) == 0) return(tibble())      # nothing to add
      tibble(year = new_years,
             pop  = predict(.x, newdata = tibble(year = new_years)),
             source = "predicted")
    }),
    observed = map(data, ~ mutate(.x, source = "observed"))
  ) %>%
  transmute(all = map2(observed, preds, bind_rows)) %>%
  unnest(all) %>%                          # back to a flat long table
  arrange(ubigeo, year) %>%
  ungroup()


# plot all districts

plots_tbl <- pop_completed |>
  group_by(name) |>
  nest() |>
  mutate(
    plot = map(data, ~ {
      ggplot(.x, aes(year, pop)) +
        geom_line() +
        geom_point() +
        labs(title = unique(.x$name))
    })
  )

walk(plots_tbl$plot, print)


# Calculate incidence
grid <- expand_grid(
  ubigeo = unique(pop_completed$ubigeo),
  year = 2010:2023,
  enfermedad =c("dengue", "malaria")
  )


data_vbd <- grid |>
  left_join(pop_completed |> select(ubigeo, year, pop, name)) |>
  mutate(pop = round(pop)) |>
  left_join(data_grouped) |>
  arrange(year, ubigeo) |>
  select(year, ubigeo, name, enfermedad, n, pop) |>
  mutate(n = ifelse(is.na(n), 0, n)) |>
  mutate(incidence = n / pop * 1e3)


data_vbd_nest <- data_vbd |>
  group_by(name) |>
  nest() |>
  mutate(
    plot = map(data, ~ {
      .x |>
        ggplot(aes(x = year, y = incidence, color = enfermedad)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        labs(
          x = "Year", y = "Incidence (per 1,000)", title = name
        )
    }

    )
  )

data_vbd_nest$plot

# plot map

data("Peru")

distritos <- Peru %>% filter(dep == "LORETO") %>% mutate(ubigeo = as.integer(ubigeo))
data_vbd$ubigeo <- as.numeric(data_vbd$ubigeo)

distritos_vbd <- distritos |>
  left_join(data_vbd)

distritos_vbd |>
  filter(enfermedad == "malaria") |>
  ggplot() +
  geom_sf(
    aes(fill = incidence),
    color = "white",
    size = 0.1
    ) +
  facet_wrap(.~year, ncol = 5 ) +
  scale_fill_viridis(
    direction = -1,
    option = "D",
    trans = "log1p",
    breaks = c(1, 10, 100, 900)
  ) +
  theme_bw()

distritos_vbd |>
  filter(enfermedad == "dengue") |>
  ggplot() +
  geom_sf(
    aes(fill = incidence),
    color = "white",
    size = 0.1
  ) +
  facet_wrap(.~year, ncol = 5 ) +
  scale_fill_viridis(
    direction = -1,
    option = "D",
    trans = "log1p",
    breaks = c(1, 5, 20, 50)
  ) +
  theme_bw()

