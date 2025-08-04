#' Filter population data for Loreto region
#'
#' @param pop_data Full population dataset
#' @param config Configuration object
#' @return Filtered population data for Loreto
#' @export
filter_loreto_population <- function(pop_data, config) {
  region_code <- config$geography$region_code
  
  pop_loreto <- pop_data |>
    dplyr::filter(
      stringr::str_starts(ubigeo, region_code) & 
      !stringr::str_ends(ubigeo, "0")
    ) |>
    dplyr::mutate(dplyr::across(as.character(config$population_data_years), as.numeric))
  
  return(pop_loreto)
}

#' Convert population data to long format
#'
#' @param pop_data Wide format population data
#' @param config Configuration object
#' @return Long format population data
#' @export
pivot_population_data <- function(pop_data, config) {
  year_cols <- as.character(config$population_data_years)
  
  pop_long <- pop_data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(year_cols),
      names_to = "year", 
      values_to = "pop"
    ) |>
    dplyr::mutate(year = as.numeric(year))
  
  return(pop_long)
}

#' Project population for earlier years using linear regression
#'
#' @param pop_long Long format population data
#' @param config Configuration object
#' @return Population data with projections for earlier years
#' @export
project_population <- function(pop_long, config) {
  projection_years <- config$population_projection_years
  
  pop_completed <- pop_long |>
    dplyr::group_by(ubigeo, name) |>
    tidyr::nest() |>
    dplyr::mutate(
      model = purrr::map(data, ~ {
        if (nrow(.x) < 2) return(NULL)
        lm(pop ~ year, data = .x)
      }),
      preds = purrr::map2(model, data, ~ {
        if (is.null(.x)) return(tibble::tibble())
        
        new_years <- setdiff(projection_years, .y$year)
        if (length(new_years) == 0) return(tibble::tibble())
        
        tibble::tibble(
          year = new_years,
          pop = predict(.x, newdata = tibble::tibble(year = new_years)),
          source = "projected"
        )
      }),
      observed = purrr::map(data, ~ dplyr::mutate(.x, source = "observed"))
    ) |>
    dplyr::transmute(all = purrr::map2(observed, preds, dplyr::bind_rows)) |>
    tidyr::unnest(all) |>
    dplyr::arrange(ubigeo, year) |>
    dplyr::ungroup()
  
  pop_completed <- pop_completed |>
    dplyr::mutate(
      pop = pmax(pop, 0),
      pop = round(pop)
    )
  
  return(pop_completed)
}

#' Process complete population pipeline
#'
#' @param config Configuration object
#' @param verbose Logical, whether to print progress messages
#' @return Processed population data with projections
#' @export
process_population_data <- function(config, verbose = TRUE) {
  if (verbose) message("Loading population data...")
  pop_data <- load_population_data(config)
  
  if (verbose) message("Filtering Loreto population...")
  pop_loreto <- filter_loreto_population(pop_data, config)
  
  if (verbose) message("Converting to long format...")
  pop_long <- pivot_population_data(pop_loreto, config)
  
  if (verbose) message("Projecting population for earlier years...")
  pop_completed <- project_population(pop_long, config)
  
  if (verbose) {
    n_districts <- length(unique(pop_completed$ubigeo))
    year_range <- range(pop_completed$year)
    message(sprintf("Processed population data for %d districts, years %d-%d", 
                    n_districts, year_range[1], year_range[2]))
  }
  
  return(pop_completed)
}