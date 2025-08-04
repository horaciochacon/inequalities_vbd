#' Import epidemiological surveillance data from Peru Open Data portal
#'
#' @param config Configuration object containing query parameters
#' @param verbose Logical, whether to print progress messages
#' @return Combined data frame with epidemiological surveillance data
#' @export
import_surveillance_data <- function(config, verbose = TRUE) {
  if (verbose) message("Loading Peru Open Data catalog...")

  catalogo <- peruopen::po_catalog()

  if (verbose) message("Searching for epidemiological surveillance data...")

  search <- peruopen::po_search(
    query = config$data_source$query,
    tags = config$diseases
  )

  if (verbose) {
    message(sprintf("Found %d resources", nrow(search$resources)))
  }

  resource_indices <- config$data_source$resource_indices

  if (verbose) {
    message(sprintf("Downloading resources at indices: %s",
                    paste(resource_indices, collapse = ", ")))
  }

  data_list <- search$resources |>
    tibble::as_tibble() |>
    dplyr::slice(resource_indices) |>
    peruopen::po_get()

  combined_data <- dplyr::bind_rows(data_list)

  if (verbose) {
    message(sprintf("Combined data contains %d records", nrow(combined_data)))
  }

  return(combined_data)
}

#' Filter surveillance data based on configuration parameters
#'
#' @param data Raw surveillance data
#' @param config Configuration object
#' @return Filtered data frame
#' @export
filter_surveillance_data <- function(data, config) {
  filtered_data <- data |>
    dplyr::filter(
      ano %in% config$years,
      tipo_dx == config$data_source$diagnosis_type,
      departamento == config$geography$region
    )

  return(filtered_data)
}

#' Load and validate population projections data
#'
#' @param config Configuration object containing file paths
#' @return Data frame with population projections
#' @export
load_population_data <- function(config) {
  pop_file <- config$population$file_path

  if (!file.exists(pop_file)) {
    stop(sprintf("Population file not found: %s", pop_file))
  }

  pop_data <- readxl::read_excel(
    pop_file,
    skip = config$population$skip_rows
  ) |>
    tidyr::drop_na() |>
    dplyr::rename(ubigeo = 1, name = 2)

  return(pop_data)
}
