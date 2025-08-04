#!/usr/bin/env Rscript
# VBD Peru Data Processing Pipeline
# Script 01: Read and Process Epidemiological Surveillance Data

# Load required libraries
library(tidyverse)
library(config)

# Source custom functions
source("R/data_import.R")
source("R/population_processing.R")
source("R/data_processing.R")
source("R/visualization.R")
source("R/utils.R")

# Load configuration
config <- config::get()

# Validate configuration
validate_config(config)

# Initialize logging
init_logging(config)

# Create output directories
create_output_directories(config)

# Log start of processing
log_message("Starting VBD Peru data processing pipeline", "INFO", config)
log_message(sprintf("Processing years: %d-%d", min(config$years), max(config$years)), "INFO", config)
log_message(sprintf("Region: %s", config$geography$region), "INFO", config)

# ---- 1. Import Surveillance Data ----
log_message("Step 1: Importing surveillance data", "INFO", config)

surveillance_data <- time_task(
  import_surveillance_data(config, verbose = TRUE),
  "Surveillance data import"
)

# ---- 2. Process Population Data ----
log_message("Step 2: Processing population data", "INFO", config)

population_data <- time_task(
  process_population_data(config, verbose = TRUE),
  "Population data processing"
)

# Save population data
save_data(population_data, "population_projections", config)

# Generate population trend plots
if (config$output$save_outputs) {
  log_message("Generating population trend plots", "INFO", config)
  pop_plots <- plot_population_trends(population_data, config, save_plots = TRUE)
}

# ---- 3. Process VBD Data ----
log_message("Step 3: Processing VBD data", "INFO", config)

vbd_data <- time_task(
  process_vbd_data(surveillance_data, population_data, config, verbose = TRUE),
  "VBD data processing"
)

# Validate processed data
if (!validate_vbd_data(vbd_data)) {
  log_message("Data validation failed - please check warnings", "ERROR", config)
  stop("Data validation failed")
}

# Save processed VBD data
save_data(vbd_data, "vbd_district_year_disease", config)

# Print summary statistics
print_vbd_summary(vbd_data, config)

# ---- 4. Generate Visualizations ----
log_message("Step 4: Generating visualizations", "INFO", config)

# Generate incidence trend plots
if (config$output$save_outputs) {
  log_message("Generating incidence trend plots", "INFO", config)
  incidence_plots <- plot_incidence_trends(vbd_data, config, save_plots = TRUE)
}

# Generate disease maps
for (disease in config$diseases) {
  log_message(sprintf("Generating %s incidence map", disease), "INFO", config)
  
  map_plot <- time_task(
    plot_disease_map(vbd_data, disease, config, save_plot = TRUE),
    sprintf("%s map generation", stringr::str_to_title(disease))
  )
}

# ---- 5. Final Summary ----
log_message("Processing completed successfully", "INFO", config)

# Print final summary
summary_msg <- sprintf(
  "Pipeline completed: %d districts, %d years, %d diseases processed",
  n_distinct(vbd_data$ubigeo),
  n_distinct(vbd_data$year),
  n_distinct(vbd_data$enfermedad)
)

log_message(summary_msg, "INFO", config)

# Return processed data for further use
invisible(list(
  surveillance_data = surveillance_data,
  population_data = population_data,
  vbd_data = vbd_data
))

