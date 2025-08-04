#' Create population trend plots for all districts
#'
#' @param pop_data Population data with projections
#' @param config Configuration object
#' @param save_plots Logical, whether to save plots
#' @return List of ggplot objects
#' @export
plot_population_trends <- function(pop_data, config, save_plots = TRUE) {
  plots_data <- pop_data |>
    dplyr::group_by(name, ubigeo) |>
    tidyr::nest() |>
    dplyr::mutate(
      plot = purrr::map2(data, name, ~ {
        ggplot2::ggplot(.x, ggplot2::aes(year, pop)) +
          ggplot2::geom_line(
            ggplot2::aes(linetype = source),
            size = config$visualization$time_series$line_size
          ) +
          ggplot2::geom_point(
            ggplot2::aes(shape = source),
            size = config$visualization$time_series$point_size
          ) +
          ggplot2::scale_linetype_manual(
            values = c("observed" = "solid", "projected" = "dashed")
          ) +
          ggplot2::scale_shape_manual(
            values = c("observed" = 16, "projected" = 1)
          ) +
          ggplot2::labs(
            title = .y,
            x = config$visualization$time_series$axis_labels$x,
            y = "Population",
            linetype = "Source",
            shape = "Source"
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "bottom")
      })
    )
  
  if (save_plots && config$output$save_outputs) {
    output_dir <- file.path(config$output$directories$figures, "population")
    save_plot_list(plots_data$plot, plots_data$name, output_dir, "population", config)
  }
  
  return(plots_data$plot)
}

#' Create incidence trend plots by district
#'
#' @param vbd_data VBD data with incidence
#' @param config Configuration object
#' @param save_plots Logical, whether to save plots
#' @return List of ggplot objects
#' @export
plot_incidence_trends <- function(vbd_data, config, save_plots = TRUE) {
  plots_data <- vbd_data |>
    dplyr::group_by(name, ubigeo) |>
    tidyr::nest() |>
    dplyr::mutate(
      plot = purrr::map2(data, name, ~ {
        .x |>
          ggplot2::ggplot(ggplot2::aes(x = year, y = incidence, color = enfermedad)) +
          ggplot2::geom_line(size = config$visualization$time_series$line_size) +
          ggplot2::geom_point(size = config$visualization$time_series$point_size) +
          ggplot2::scale_color_manual(
            values = c("dengue" = "#E69F00", "malaria" = "#56B4E9"),
            labels = c("dengue" = "Dengue", "malaria" = "Malaria")
          ) +
          ggplot2::labs(
            x = config$visualization$time_series$axis_labels$x,
            y = config$visualization$time_series$axis_labels$y,
            title = .y,
            color = "Disease"
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "bottom")
      })
    )
  
  if (save_plots && config$output$save_outputs) {
    output_dir <- file.path(config$output$directories$figures, "incidence")
    save_plot_list(plots_data$plot, plots_data$name, output_dir, "incidence", config)
  }
  
  return(plots_data$plot)
}

#' Create spatial map of disease incidence
#'
#' @param vbd_data VBD data with incidence
#' @param disease Disease name ("malaria" or "dengue")
#' @param config Configuration object
#' @param save_plot Logical, whether to save plot
#' @return ggplot object
#' @export
plot_disease_map <- function(vbd_data, disease, config, save_plot = TRUE) {
  if (!disease %in% config$diseases) {
    stop(sprintf("Disease '%s' not found in configuration", disease))
  }
  
  map_config <- config$visualization$maps[[disease]]
  
  suppressPackageStartupMessages(library(sf))
  data("Peru", package = "innovar")
  
  distritos <- Peru |> 
    dplyr::filter(dep == config$geography$region) |> 
    dplyr::mutate(ubigeo = as.integer(ubigeo))
  
  vbd_data$ubigeo <- as.numeric(vbd_data$ubigeo)
  
  distritos_vbd <- distritos |>
    dplyr::left_join(vbd_data, by = "ubigeo")
  
  plot <- distritos_vbd |>
    dplyr::filter(enfermedad == disease) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(fill = incidence),
      color = map_config$border_color,
      size = map_config$border_size
    ) +
    ggplot2::facet_wrap(~year, ncol = map_config$facet_cols) +
    ggplot2::scale_fill_viridis_c(
      direction = map_config$palette_direction,
      option = map_config$palette_option,
      trans = map_config$transformation,
      breaks = map_config$breaks,
      name = "Incidence\n(per 1,000)"
    ) +
    ggplot2::labs(
      title = sprintf("%s Incidence in %s by Year", 
                      stringr::str_to_title(disease), 
                      config$geography$region)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      strip.background = ggplot2::element_rect(fill = "white"),
      panel.spacing = ggplot2::unit(0.1, "lines")
    )
  
  if (save_plot && config$output$save_outputs) {
    output_file <- create_output_filename(
      sprintf("%s_map", disease), 
      config$output$directories$maps, 
      config
    )
    save_plot(plot, output_file, config)
  }
  
  return(plot)
}

#' Save a single plot
#'
#' @param plot ggplot object
#' @param filename Output filename
#' @param config Configuration object
#' @export
save_plot <- function(plot, filename, config) {
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = config$visualization$figure_width,
    height = config$visualization$figure_height,
    dpi = config$visualization$dpi
  )
  message(sprintf("Saved plot: %s", filename))
}

#' Save a list of plots
#'
#' @param plots List of ggplot objects
#' @param names Names for each plot
#' @param output_dir Output directory
#' @param prefix File prefix
#' @param config Configuration object
#' @export
save_plot_list <- function(plots, names, output_dir, prefix, config) {
  ensure_directory(output_dir)
  
  safe_names <- gsub("[^[:alnum:]_-]", "_", names)
  
  purrr::walk2(plots, safe_names, ~ {
    filename <- create_output_filename(
      sprintf("%s_%s", prefix, .y), 
      output_dir, 
      config
    )
    save_plot(.x, filename, config)
  })
}

#' Create output filename with optional timestamp
#'
#' @param base_name Base filename without extension
#' @param output_dir Output directory
#' @param config Configuration object
#' @param extension File extension
#' @return Full path to output file
#' @export
create_output_filename <- function(base_name, output_dir, config, extension = "png") {
  ensure_directory(output_dir)
  
  if (config$output$timestamp_outputs) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- sprintf("%s_%s_%s.%s", 
                        config$output$prefix, 
                        base_name, 
                        timestamp, 
                        extension)
  } else {
    filename <- sprintf("%s_%s.%s", 
                        config$output$prefix, 
                        base_name, 
                        extension)
  }
  
  return(file.path(output_dir, filename))
}

#' Ensure directory exists
#'
#' @param path Directory path
#' @export
ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}