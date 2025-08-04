#' Initialize logging based on configuration
#'
#' @param config Configuration object
#' @return Invisible NULL
#' @export
init_logging <- function(config) {
  if (!config$logging$enable) {
    return(invisible(NULL))
  }
  
  log_dir <- dirname(config$logging$log_file)
  ensure_directory(log_dir)
  
  log_file <- gsub("\\{date\\}", format(Sys.Date(), "%Y%m%d"), config$logging$log_file)
  
  if (config$logging$console_output) {
    sink(type = "message")
  }
  
  options(
    vbd_peru.log_file = log_file,
    vbd_peru.log_level = config$logging$level
  )
  
  log_message("Logging initialized", "INFO")
}

#' Log a message with timestamp and level
#'
#' @param message Message to log
#' @param level Log level (INFO, WARNING, ERROR)
#' @param config Optional configuration object
#' @export
log_message <- function(message, level = "INFO", config = NULL) {
  log_file <- getOption("vbd_peru.log_file")
  log_level <- getOption("vbd_peru.log_level", "INFO")
  
  if (is.null(log_file) && !is.null(config)) {
    init_logging(config)
    log_file <- getOption("vbd_peru.log_file")
  }
  
  levels <- c("ERROR" = 3, "WARNING" = 2, "INFO" = 1)
  
  if (levels[level] >= levels[log_level]) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- sprintf("[%s] %s: %s", timestamp, level, message)
    
    if (!is.null(log_file)) {
      cat(log_entry, "\n", file = log_file, append = TRUE)
    }
    
    if (getOption("vbd_peru.console_output", TRUE)) {
      message(log_entry)
    }
  }
}

#' Save data in multiple formats
#'
#' @param data Data frame to save
#' @param base_name Base filename without extension
#' @param config Configuration object
#' @export
save_data <- function(data, base_name, config) {
  if (!config$output$save_outputs) {
    return(invisible(NULL))
  }
  
  output_dir <- config$output$directories$data
  ensure_directory(output_dir)
  
  for (format in config$output$output_formats) {
    filename <- create_output_filename(base_name, output_dir, config, format)
    
    switch(format,
      "rds" = {
        saveRDS(data, filename)
        log_message(sprintf("Saved RDS file: %s", filename), "INFO")
      },
      "csv" = {
        utils::write.csv(data, filename, row.names = FALSE)
        log_message(sprintf("Saved CSV file: %s", filename), "INFO")
      },
      {
        warning(sprintf("Unknown output format: %s", format))
      }
    )
  }
}

#' Create all output directories from configuration
#'
#' @param config Configuration object
#' @export
create_output_directories <- function(config) {
  dirs <- config$output$directories
  
  for (dir_name in names(dirs)) {
    dir_path <- dirs[[dir_name]]
    ensure_directory(dir_path)
    log_message(sprintf("Created directory: %s", dir_path), "INFO")
  }
}

#' Timer function for performance tracking
#'
#' @param expr Expression to time
#' @param task_name Name of the task being timed
#' @return Result of the expression
#' @export
time_task <- function(expr, task_name = "Task") {
  start_time <- Sys.time()
  result <- force(expr)
  end_time <- Sys.time()
  
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  log_message(sprintf("%s completed in %.2f seconds", task_name, duration), "INFO")
  
  return(result)
}

#' Validate configuration object
#'
#' @param config Configuration object
#' @return Logical indicating if configuration is valid
#' @export
validate_config <- function(config) {
  required_sections <- c("project", "geography", "years", "diseases", 
                        "output", "visualization", "processing")
  
  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0) {
    stop(sprintf("Missing required configuration sections: %s", 
                paste(missing_sections, collapse = ", ")))
  }
  
  if (!is.numeric(config$years)) {
    stop("Configuration 'years' must be numeric")
  }
  
  if (!is.character(config$diseases) || length(config$diseases) == 0) {
    stop("Configuration 'diseases' must be a non-empty character vector")
  }
  
  return(TRUE)
}

#' Print summary statistics for VBD data
#'
#' @param data VBD data frame
#' @param config Configuration object
#' @export
print_vbd_summary <- function(data, config) {
  summary_stats <- data |>
    dplyr::group_by(enfermedad) |>
    dplyr::summarise(
      total_cases = sum(n),
      districts_affected = dplyr::n_distinct(ubigeo[n > 0]),
      years_covered = dplyr::n_distinct(year),
      mean_incidence = mean(incidence, na.rm = TRUE),
      max_incidence = max(incidence, na.rm = TRUE),
      .groups = "drop"
    )
  
  log_message("VBD Data Summary:", "INFO")
  log_message(sprintf("Total records: %d", nrow(data)), "INFO")
  log_message(sprintf("Years covered: %d-%d", min(data$year), max(data$year)), "INFO")
  log_message(sprintf("Districts: %d", dplyr::n_distinct(data$ubigeo)), "INFO")
  
  for (i in seq_len(nrow(summary_stats))) {
    disease <- summary_stats$enfermedad[i]
    log_message(sprintf(
      "%s: %d cases, %d districts affected, mean incidence: %.2f per 1,000",
      stringr::str_to_title(disease),
      summary_stats$total_cases[i],
      summary_stats$districts_affected[i],
      summary_stats$mean_incidence[i]
    ), "INFO")
  }
}