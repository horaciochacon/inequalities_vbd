#' Aggregate surveillance data by district and disease
#'
#' @param data Filtered surveillance data
#' @return Aggregated data by location and disease
#' @export
aggregate_surveillance_data <- function(data) {
  data_agg <- data |>
    dplyr::group_by(
      departamento, provincia, distrito, enfermedad, diagnostic,
      diresa, ubigeo, ano
    ) |>
    dplyr::summarise(casos = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(departamento, provincia, distrito, ano)
  
  return(data_agg)
}

#' Standardize disease names
#'
#' @param data Aggregated surveillance data
#' @param config Configuration object with disease patterns
#' @return Data with standardized disease names
#' @export
standardize_disease_names <- function(data, config) {
  dengue_pattern <- config$processing$disease_mapping$dengue_pattern
  malaria_pattern <- config$processing$disease_mapping$malaria_pattern
  
  data_std <- data |>
    dplyr::ungroup() |>
    dplyr::select(year = ano, ubigeo, enfermedad, casos) |>
    dplyr::mutate(
      enfermedad = dplyr::case_when(
        stringr::str_detect(enfermedad, dengue_pattern) ~ "dengue",
        stringr::str_detect(enfermedad, malaria_pattern) ~ "malaria",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(enfermedad)) |>
    dplyr::group_by(year, ubigeo, enfermedad) |>
    dplyr::summarise(n = sum(casos, na.rm = TRUE), .groups = "drop")
  
  return(data_std)
}

#' Create complete grid of district-year-disease combinations
#'
#' @param pop_data Population data with ubigeo codes
#' @param config Configuration object
#' @return Complete grid of all combinations
#' @export
create_analysis_grid <- function(pop_data, config) {
  grid <- tidyr::expand_grid(
    ubigeo = unique(pop_data$ubigeo),
    year = config$years,
    enfermedad = config$diseases
  )
  
  return(grid)
}

#' Calculate incidence rates
#'
#' @param grid Analysis grid
#' @param pop_data Population data
#' @param case_data Aggregated case data
#' @param config Configuration object
#' @return Data frame with incidence calculations
#' @export
calculate_incidence <- function(grid, pop_data, case_data, config) {
  data_vbd <- grid |>
    dplyr::left_join(
      pop_data |> dplyr::select(ubigeo, year, pop, name),
      by = c("ubigeo", "year")
    ) |>
    dplyr::mutate(pop = round(pop)) |>
    dplyr::left_join(
      case_data,
      by = c("year", "ubigeo", "enfermedad")
    ) |>
    dplyr::arrange(year, ubigeo) |>
    dplyr::select(year, ubigeo, name, enfermedad, n, pop) |>
    dplyr::mutate(
      n = dplyr::if_else(is.na(n), 0L, as.integer(n)),
      incidence = n / pop * config$processing$incidence_multiplier
    )
  
  return(data_vbd)
}

#' Complete data processing pipeline
#'
#' @param surveillance_data Raw surveillance data
#' @param pop_data Population data
#' @param config Configuration object
#' @param verbose Logical, whether to print progress messages
#' @return Processed data with incidence calculations
#' @export
process_vbd_data <- function(surveillance_data, pop_data, config, verbose = TRUE) {
  if (verbose) message("Filtering surveillance data...")
  filtered_data <- filter_surveillance_data(surveillance_data, config)
  
  if (verbose) message("Aggregating by district and disease...")
  aggregated_data <- aggregate_surveillance_data(filtered_data)
  
  if (verbose) message("Standardizing disease names...")
  standardized_data <- standardize_disease_names(aggregated_data, config)
  
  if (verbose) message("Creating analysis grid...")
  grid <- create_analysis_grid(pop_data, config)
  
  if (verbose) message("Calculating incidence rates...")
  incidence_data <- calculate_incidence(grid, pop_data, standardized_data, config)
  
  if (verbose) {
    n_records <- nrow(incidence_data)
    total_cases <- sum(incidence_data$n)
    message(sprintf("Processed %d district-year-disease records with %d total cases", 
                    n_records, total_cases))
  }
  
  return(incidence_data)
}

#' Validate processed data
#'
#' @param data Processed VBD data
#' @return Logical indicating if data passes validation
#' @export
validate_vbd_data <- function(data) {
  required_cols <- c("year", "ubigeo", "name", "enfermedad", "n", "pop", "incidence")
  
  if (!all(required_cols %in% names(data))) {
    missing <- setdiff(required_cols, names(data))
    warning(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
    return(FALSE)
  }
  
  if (any(data$n < 0)) {
    warning("Negative case counts detected")
    return(FALSE)
  }
  
  if (any(data$pop <= 0, na.rm = TRUE)) {
    warning("Invalid population values detected")
    return(FALSE)
  }
  
  if (any(is.infinite(data$incidence))) {
    warning("Infinite incidence values detected")
    return(FALSE)
  }
  
  return(TRUE)
}