library(tidyverse)
library(LaplacesDemon)


wealth <- read_csv("data/resultados_SAE_loreto_wi.csv") |> filter(modelo == "modelo_fh2_spatial_2")
edu <- read_csv("data/resultados_SAE_loreto_edu.csv") |> filter(modelo == "modelo_fh2_spatial_2")

wealth_vbd <- wealth |>
  left_join(vbd_data |> mutate(ubigeo = as.numeric(ubigeo)) |> filter(enfermedad == "malaria")) |>
  na.omit()

edu_vbd <- edu |>
  left_join(vbd_data |> mutate(ubigeo = as.numeric(ubigeo)) |> filter(enfermedad == "malaria")) |>
  na.omit()


plot(wealth_vbd$wi_estimate_prop, logit(wealth_vbd$incidence/1000))
plot(edu_vbd$edu_estimate_prop, logit(edu_vbd$incidence/1000))


wealth |>
  ggplot(aes(x = year, y = wi_estimate_prop)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ubigeo, scales = "free_y")

# 1. Between-District Gini Coefficient (population-weighted)
library(ineq)
calculate_gini_by_year <- function(data) {
  data %>%
    group_by(year) %>%
    summarise(
      # Unweighted Gini (treats all districts equally)
      gini_unweighted = ineq(incidence, type = "Gini"),

      # Population-weighted Gini (accounts for district size)
      gini_weighted = ineq(incidence, type = "Gini"),

      n_districts = n(),
      .groups = 'drop'
    )
}

# Apply to your data
gini_results <- calculate_gini_by_year(wealth_vbd)
print(gini_results)

# 2. Coefficient of Variation (between-district inequality)
calculate_cv_by_year <- function(data) {
  data %>%
    group_by(year) %>%
    summarise(
      # Simple CV
      cv_simple = sd(incidence) / mean(incidence),

      # Population-weighted CV
      mean_weighted = weighted.mean(incidence, pop),
      var_weighted = sum(pop * (incidence - mean_weighted)^2) / sum(pop),
      cv_weighted = sqrt(var_weighted) / mean_weighted,

      # Also calculate for wealth to see if inequality is changing
      cv_wealth = sd(wi_estimate_prop) / mean(wi_estimate_prop),

      .groups = 'drop'
    )
}

cv_results <- calculate_cv_by_year(wealth_vbd)
print(cv_results)

# 3. Slope Index of Inequality (SII) - Ecological version
calculate_sii_rii <- function(data, year_val) {
  df <- data %>%
    filter(year == year_val) %>%
    arrange(wi_estimate_prop) %>%
    mutate(
      # Calculate cumulative population proportion (district ranking)
      cum_pop = cumsum(pop) / sum(pop),
      # Midpoint of each district's population range
      rank = (lag(cum_pop, default = 0) + cum_pop) / 2
    )

  # Weighted regression of incidence on wealth rank
  model <- lm(incidence ~ rank, data = df, weights = pop)

  # SII is the slope coefficient
  sii <- coef(model)[2]

  # RII is SII divided by mean incidence
  mean_incidence <- weighted.mean(df$incidence, df$pop)
  rii <- sii / mean_incidence

  return(list(
    year = year_val,
    SII = sii,
    RII = rii,
    mean_incidence = mean_incidence,
    model_summary = summary(model)
  ))
}

# Calculate for all years
sii_results <- map_df(unique(wealth_vbd$year), function(y) {
  result <- calculate_sii_rii(wealth_vbd, y)
  data.frame(
    year = result$year,
    SII = result$SII,
    RII = result$RII,
    mean_incidence = result$mean_incidence
  )
})

print(sii_results)

# 4. Wealth Quintile Analysis
calculate_quintile_disparities <- function(data) {
  # Create wealth quintiles by year
  data_with_quintiles <- data %>%
    group_by(year) %>%
    mutate(
      # Create quintiles weighted by population
      wealth_quintile = cut(wi_estimate_prop,
                            breaks = wtd.quantile(wi_estimate_prop,
                                                  weights = pop,
                                                  probs = seq(0, 1, 0.2)),
                            labels = c("Q1 (Poorest)", "Q2", "Q3", "Q4", "Q5 (Richest)"),
                            include.lowest = TRUE)
    )

  # Calculate quintile-specific rates
  quintile_rates <- data_with_quintiles %>%
    group_by(year, wealth_quintile) %>%
    summarise(
      total_cases = sum(n),
      total_pop = sum(pop),
      mean_incidence = total_cases / total_pop * 1000,  # per 1000
      n_districts = n(),
      mean_wealth = weighted.mean(wi_estimate_prop, pop),
      .groups = 'drop'
    )

  # Calculate disparity measures
  disparity_measures <- quintile_rates %>%
    group_by(year) %>%
    summarise(
      # Rate ratio: poorest vs richest
      rate_ratio = mean_incidence[wealth_quintile == "Q1 (Poorest)"] /
        mean_incidence[wealth_quintile == "Q5 (Richest)"],

      # Rate difference: poorest - richest
      rate_difference = mean_incidence[wealth_quintile == "Q1 (Poorest)"] -
        mean_incidence[wealth_quintile == "Q5 (Richest)"],

      # Population attributable fraction
      overall_rate = sum(total_cases) / sum(total_pop) * 1000,
      paf = (overall_rate - mean_incidence[wealth_quintile == "Q5 (Richest)"]) / overall_rate,

      .groups = 'drop'
    )

  return(list(
    quintile_rates = quintile_rates,
    disparity_measures = disparity_measures
  ))
}

quintile_analysis <- calculate_quintile_disparities(wealth_vbd)
print(quintile_analysis$disparity_measures)


# 5. Theil Index (decomposable inequality measure)
calculate_theil_index <- function(data, year_val) {
  df <- data %>%
    filter(year == year_val)

  # Calculate population shares and incidence shares
  df <- df %>%
    mutate(
      pop_share = pop / sum(pop),
      incidence_share = (incidence * pop) / sum(incidence * pop)
    )

  # Theil index
  # Note: need to handle zeros in log
  df_nonzero <- df %>% filter(incidence_share > 0)

  theil <- sum(df_nonzero$incidence_share * log(df_nonzero$incidence_share / df_nonzero$pop_share))

  return(theil)
}

theil_results <- wealth_vbd %>%
  group_by(year) %>%
  summarise(
    theil_index = calculate_theil_index(data, first(year)),
    .groups = 'drop'
  )

print(theil_results)

# 6. Visualization of Inequalities
library(ggplot2)

# Trend in inequality measures over time
inequality_trends <- bind_rows(
  gini_results %>% select(year, value = gini_weighted) %>% mutate(measure = "Gini"),
  cv_results %>% select(year, value = cv_weighted) %>% mutate(measure = "CV"),
  sii_results %>% select(year, value = RII) %>% mutate(measure = "RII")
)

p1 <- ggplot(inequality_trends, aes(x = year, y = value, color = measure)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Trends in District-Level Health Inequality",
       x = "Year",
       y = "Inequality Measure") +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)

# Quintile-specific incidence rates
p2 <- ggplot(quintile_analysis$quintile_rates,
             aes(x = year, y = mean_incidence, color = wealth_quintile)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"),
                     name = "Wealth Quintile") +
  labs(title = "Malaria Incidence by District Wealth Quintile",
       x = "Year",
       y = "Incidence per 1,000 population") +
  theme_minimal()

print(p2)

library(tidyverse)
library(ggplot2)
library(patchwork)

# Function to calculate concentration curve data
calculate_concentration_curve <- function(data, year_val = NULL) {
  # Filter by year if specified
  if (!is.null(year_val)) {
    data <- data %>% filter(year == year_val)
  }

  # Sort districts by wealth (poorest to richest)
  df <- data %>%
    arrange(wi_estimate_prop) %>%
    mutate(
      # Calculate cumulative proportions
      cum_pop = cumsum(pop) / sum(pop),
      cum_cases = cumsum(n) / sum(n),

      # Also calculate cumulative proportion of total disease burden (incidence * pop)
      disease_burden = incidence * pop,
      cum_burden = cumsum(disease_burden) / sum(disease_burden)
    )

  # Add the origin point (0,0) for complete curve
  df <- bind_rows(
    data.frame(cum_pop = 0, cum_cases = 0, cum_burden = 0),
    df
  )

  return(df)
}

# Calculate concentration index from the curve
calculate_concentration_index <- function(data, use_burden = TRUE) {
  # Sort by wealth
  df <- data %>%
    arrange(wi_estimate_prop)

  if (use_burden) {
    # Using disease burden (incidence * population)
    df$health_var <- df$incidence * df$pop
  } else {
    # Using just cases
    df$health_var <- df$n
  }

  # Calculate shares
  df <- df %>%
    mutate(
      pop_share = pop / sum(pop),
      health_share = health_var / sum(health_var),
      cum_pop = cumsum(pop_share),
      rank = (lag(cum_pop, default = 0) + cum_pop) / 2
    )

  # Concentration index using convenient covariance formula
  ci <- 2 * cov(df$health_share, df$rank) / mean(df$health_share)

  return(ci)
}

# Alternative version that works better with summarise
calc_ci <- function(incidence, wealth, pop, n_cases, use_burden = TRUE) {
  # Create a temporary dataframe
  df <- data.frame(
    incidence = incidence,
    wealth = wealth,
    pop = pop,
    n = n_cases
  ) %>%
    arrange(wealth)

  if (use_burden) {
    # Using disease burden (incidence * population)
    df$health_var <- df$incidence * df$pop
  } else {
    # Using just cases
    df$health_var <- df$n
  }

  # Calculate shares
  df <- df %>%
    mutate(
      pop_share = pop / sum(pop),
      health_share = health_var / sum(health_var),
      cum_pop = cumsum(pop_share),
      rank = (lag(cum_pop, default = 0) + cum_pop) / 2
    )

  # Concentration index
  ci <- 2 * cov(df$health_share, df$rank) / mean(df$health_share)

  return(ci)
}

# 1. Single Year Concentration Curve
# Let's use the most recent year as an example
recent_year <- max(wealth_vbd$year)
conc_data <- calculate_concentration_curve(wealth_vbd, recent_year)

# Calculate CI for this year
recent_year_data <- wealth_vbd %>% filter(year == recent_year)
ci_value <- calc_ci(recent_year_data$incidence, recent_year_data$wi_estimate_prop,
                    recent_year_data$pop, recent_year_data$n, use_burden = TRUE)

# Basic concentration curve plot
p1 <- ggplot(conc_data, aes(x = cum_pop, y = cum_burden)) +
  geom_line(size = 1.2, color = "darkblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  coord_equal() +
  labs(
    title = paste("Concentration Curve for Malaria -", recent_year),
    subtitle = paste("Concentration Index =", round(ci_value, 3)),
    x = "Cumulative % of population\n(ranked by district wealth, poorest to richest)",
    y = "Cumulative % of malaria burden"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11)
  ) +
  # Add shaded area to visualize inequality
  geom_ribbon(aes(ymin = cum_pop, ymax = cum_burden),
              alpha = 0.3, fill = "darkblue")

print(p1)

# 2. Multiple Years Comparison
# Calculate concentration curves for all years
all_years_conc <- wealth_vbd %>%
  group_by(year) %>%
  group_modify(~ calculate_concentration_curve(.x)) %>%
  ungroup()

# Calculate CI for each year
ci_by_year <- wealth_vbd %>%
  group_by(year) %>%
  summarise(
    ci_burden = calc_ci(incidence, wi_estimate_prop, pop, n, use_burden = TRUE),
    ci_cases = calc_ci(incidence, wi_estimate_prop, pop, n, use_burden = FALSE),
    .groups = 'drop'
  )

print("Concentration Index by Year:")
print(ci_by_year)

# Plot multiple years
p2 <- ggplot(all_years_conc, aes(x = cum_pop, y = cum_burden, color = factor(year))) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.8) +
  coord_equal() +
  scale_color_viridis_d(name = "Year") +
  labs(
    title = "Concentration Curves Over Time",
    x = "Cumulative % of population (poorest to richest districts)",
    y = "Cumulative % of malaria burden"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(p2)


# SII - RII -----------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(patchwork)

# 1. Slope Index of Inequality (SII) and Relative Index of Inequality (RII)
calculate_sii_rii <- function(data, year_val) {
  df <- data %>%
    filter(year == year_val) %>%
    arrange(-wi_estimate_prop) %>%
    mutate(
      # Calculate cumulative population proportion (ridit score)
      cum_pop = cumsum(pop) / sum(pop),
      # Midpoint of each district's population range
      rank = (lag(cum_pop, default = 0) + cum_pop) / 2
    )

  # Weighted regression of incidence on wealth rank
  model <- lm(incidence ~ rank, data = df, weights = pop)

  # SII is the slope coefficient
  sii <- coef(model)[2]

  # RII is SII divided by mean incidence
  mean_incidence <- weighted.mean(df$incidence, df$pop)
  rii <- sii / mean_incidence

  # R-squared to see how well linear model fits
  r_squared <- summary(model)$r.squared

  # Also fit a quadratic model to detect non-linearity
  model_quad <- lm(incidence ~ rank + I(rank^2), data = df, weights = pop)
  quad_coef <- coef(model_quad)[3]
  quad_p_value <- summary(model_quad)$coefficients[3, 4]

  return(list(
    year = year_val,
    SII = sii,
    RII = rii,
    mean_incidence = mean_incidence,
    r_squared = r_squared,
    quadratic_coef = quad_coef,
    quad_significant = quad_p_value < 0.05,
    model = model,
    model_quad = model_quad,
    data = df
  ))
}

# Calculate for all years
sii_results <- map(unique(wealth_vbd$year), ~calculate_sii_rii(wealth_vbd, .x))
names(sii_results) <- unique(wealth_vbd$year)

# Summary table
sii_summary <- map_df(sii_results, function(x) {
  data.frame(
    year = x$year,
    SII = x$SII,
    RII = x$RII,
    r_squared = x$r_squared,
    quadratic_sig = x$quad_significant
  )
})

print("SII and RII Results:")
print(sii_summary)

# 2. Visualize SII for 2020 (to see the poor fit)
result_2020 <- sii_results[["2020"]]
df_2020 <- result_2020$data

p_sii_2020 <- ggplot(df_2020, aes(x = rank)) +
  # Actual data points (sized by population)
  geom_point(aes(y = incidence, size = pop), alpha = 0.6, color = "darkblue") +
  # Linear SII regression line
  geom_smooth(aes(y = incidence, weight = pop),
              method = "lm", se = TRUE, color = "red", linetype = "solid") +
  # Quadratic fit
  geom_smooth(aes(y = incidence, weight = pop),
              method = "lm", formula = y ~ x + I(x^2),
              se = TRUE, color = "green", linetype = "dashed") +
  labs(
    title = paste("SII Visualization for 2020 - Poor Linear Fit"),
    subtitle = paste("SII =", round(result_2020$SII, 1),
                     "| R² =", round(result_2020$r_squared, 3),
                     "| Quadratic term significant:", result_2020$quad_significant),
    x = "Cumulative population proportion (wealth rank)",
    y = "Malaria incidence",
    size = "Population"
  ) +
  theme_minimal()

print(p_sii_2020)

# 3. Better Measures for Non-Linear Patterns

# A. Between-Group Variance (BGV) and Kunst-Mackenbach Index
calculate_bgv_km <- function(data, year_val) {
  df <- data %>%
    filter(year == year_val) %>%
    mutate(
      # Create wealth quintiles
      wealth_quintile = ntile(-wi_estimate_prop, 5)
    )

  # Calculate quintile-specific rates
  quintile_rates <- df %>%
    group_by(wealth_quintile) %>%
    summarise(
      total_cases = sum(n),
      total_pop = sum(pop),
      rate = total_cases / total_pop * 1000,
      .groups = 'drop'
    )

  # Overall rate
  overall_rate <- sum(df$n) / sum(df$pop) * 1000

  # Between-group variance
  bgv <- sum(quintile_rates$total_pop * (quintile_rates$rate - overall_rate)^2) /
    sum(quintile_rates$total_pop)

  # Kunst-Mackenbach index (ratio of extremes)
  km_index <- quintile_rates$rate[quintile_rates$wealth_quintile == 1] /
    quintile_rates$rate[quintile_rates$wealth_quintile == 5]

  # Index of Disparity (mean absolute deviation from best rate)
  best_rate <- min(quintile_rates$rate)
  id <- sum(abs(quintile_rates$rate - best_rate) * quintile_rates$total_pop) /
    sum(quintile_rates$total_pop)

  return(list(
    year = year_val,
    bgv = bgv,
    km_index = km_index,
    index_disparity = id,
    quintile_rates = quintile_rates
  ))
}

# B. Visualize Quintile-Specific Rates Over Time
all_quintile_data <- map_df(unique(wealth_vbd$year), function(y) {
  result <- calculate_bgv_km(wealth_vbd, y)
  result$quintile_rates %>%
    mutate(year = y)
})

p_quintiles <- ggplot(all_quintile_data,
                      aes(x = year, y = rate, color = factor(wealth_quintile))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"),
                     labels = c("Q1 (Poorest)", "Q2", "Q3", "Q4", "Q5 (Richest)"),
                     name = "Wealth Quintile") +
  labs(
    title = "Malaria Rates by Wealth Quintile Over Time",
    subtitle = "Non-linear pattern visible in 2020 (middle quintiles have highest rates)",
    x = "Year",
    y = "Incidence per 1,000 population"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(p_quintiles)

# 4. Create a "Heat Map" Style Visualization
# This shows incidence by wealth rank for each year
heatmap_data <- wealth_vbd %>%
  group_by(year) %>%
  arrange(-wi_estimate_prop) %>%
  mutate(
    wealth_percentile = ntile(-wi_estimate_prop, 20),  # 20 groups for more detail
    log_incidence = log(incidence + 1)  # Log transform for better visualization
  ) %>%
  group_by(year, wealth_percentile) %>%
  summarise(
    mean_incidence = weighted.mean(incidence, pop),
    log_mean_incidence = weighted.mean(log_incidence, pop),
    .groups = 'drop'
  )

p_heatmap <- ggplot(heatmap_data, aes(x = wealth_percentile, y = factor(year),
                                      fill = mean_incidence)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred",
                       midpoint = median(heatmap_data$mean_incidence),
                       name = "Incidence") +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20),
                     labels = c("Poorest\n5%", "25%", "50%", "75%", "Richest\n5%")) +
  labs(
    title = "Malaria Incidence by Wealth Percentile and Year",
    subtitle = "2020 shows high incidence in middle wealth groups",
    x = "Population ranked by wealth",
    y = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

print(p_heatmap)

# 5. Polarization Index - Detects U-shaped patterns
calculate_polarization <- function(data, year_val) {
  df <- data %>%
    filter(year == year_val) %>%
    arrange(-wi_estimate_prop) %>%
    mutate(
      tercile = ntile(-wi_estimate_prop, 3)
    )

  # Calculate rates for each tercile
  tercile_rates <- df %>%
    group_by(tercile) %>%
    summarise(
      rate = sum(n) / sum(pop) * 1000,
      .groups = 'drop'
    )

  # Polarization = (rate_bottom + rate_top) / (2 * rate_middle)
  polarization <- (tercile_rates$rate[1] + tercile_rates$rate[3]) /
    (2 * tercile_rates$rate[2])

  return(polarization)
}

# Calculate polarization for all years
polarization_results <- data.frame(
  year = unique(wealth_vbd$year),
  polarization = map_dbl(unique(wealth_vbd$year),
                         ~calculate_polarization(wealth_vbd, .x))
)

print("\nPolarization Index (>1 means extremes have higher rates than middle):")
print(polarization_results)

# 6. Comprehensive comparison for 2020
result_2020_bgv <- calculate_bgv_km(wealth_vbd, 2020)

# Create a summary plot for 2020
p_2020_bars <- ggplot(result_2020_bgv$quintile_rates,
                      aes(x = factor(wealth_quintile), y = rate, fill = factor(wealth_quintile))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"),
                    guide = "none") +
  scale_x_discrete(labels = c("Q1\n(Poorest)", "Q2", "Q3", "Q4", "Q5\n(Richest)")) +
  labs(
    title = "2020 Malaria Rates by Wealth Quintile",
    subtitle = "Middle quintiles (Q2-Q3) have highest rates - not captured by linear SII",
    x = "Wealth Quintile",
    y = "Incidence per 1,000"
  ) +
  theme_minimal() +
  geom_text(aes(label = round(rate, 1)), vjust = -0.5)

print(p_2020_bars)

# 7. Summary of all inequality measures for 2020
summary_2020 <- data.frame(
  Measure = c("Concentration Index", "Slope Index (SII)", "Relative Index (RII)",
              "Kunst-Mackenbach (Q1/Q5)", "Between-Group Variance",
              "Index of Disparity", "Polarization Index"),
  Value = c(
    ci_by_year$ci_burden[ci_by_year$year == 2020],
    result_2020$SII,
    result_2020$RII,
    result_2020_bgv$km_index,
    result_2020_bgv$bgv,
    result_2020_bgv$index_disparity,
    polarization_results$polarization[polarization_results$year == 2020]
  ),
  Interpretation = c(
    "Near zero - no linear wealth gradient",
    "Near zero - misses non-linear pattern",
    "Near zero - misses non-linear pattern",
    "Q1 has lower rate than Q5!",
    "High - detects variation between groups",
    "High - shows dispersion from best rate",
    "Low - middle has higher rates than extremes"
  )
)

print("\nComparison of Inequality Measures for 2020:")
print(summary_2020)

# 8. Statistical test for non-linearity
test_nonlinearity <- function(data, year_val) {
  df <- data %>%
    filter(year == year_val) %>%
    arrange(-wi_estimate_prop) %>%
    mutate(
      cum_pop = cumsum(pop) / sum(pop),
      rank = (lag(cum_pop, default = 0) + cum_pop) / 2
    )

  # Compare linear vs polynomial models
  linear_model <- lm(incidence ~ rank, data = df, weights = pop)
  poly_model <- lm(incidence ~ poly(rank, 3), data = df, weights = pop)

  # ANOVA to test if polynomial terms improve fit
  anova_result <- anova(linear_model, poly_model)

  return(list(
    year = year_val,
    linear_r2 = summary(linear_model)$r.squared,
    poly_r2 = summary(poly_model)$r.squared,
    p_value = anova_result$`Pr(>F)`[2],
    significant_nonlinearity = anova_result$`Pr(>F)`[2] < 0.05
  ))
}

# Test for all years
nonlinearity_tests <- map_df(unique(wealth_vbd$year), function(y) {
  result <- test_nonlinearity(wealth_vbd, y)
  data.frame(
    year = result$year,
    linear_r2 = result$linear_r2,
    poly_r2 = result$poly_r2,
    p_value = result$p_value,
    nonlinear = result$significant_nonlinearity
  )
})

print("\nTests for Non-linearity:")
print(nonlinearity_tests)


# 9. Calculate SII and RII with confidence intervals
calculate_sii_rii_ci <- function(data, year_val, n_bootstrap = 1000) {
  # Original calculation
  original_result <- calculate_sii_rii(data, year_val)

  # Bootstrap for confidence intervals
  bootstrap_results <- replicate(n_bootstrap, {
    # Resample districts with replacement
    df_boot <- data %>%
      filter(year == year_val) %>%
      sample_n(size = n(), replace = TRUE) %>%
      arrange(-wi_estimate_prop) %>%
      mutate(
        cum_pop = cumsum(pop) / sum(pop),
        rank = (lag(cum_pop, default = 0) + cum_pop) / 2
      )

    # Fit model
    model_boot <- lm(incidence ~ rank, data = df_boot, weights = pop)
    sii_boot <- coef(model_boot)[2]
    mean_inc_boot <- weighted.mean(df_boot$incidence, df_boot$pop)
    rii_boot <- sii_boot / mean_inc_boot

    c(sii = sii_boot, rii = rii_boot)
  })

  # Calculate confidence intervals
  sii_ci <- quantile(bootstrap_results["sii.rank",], c(0.025, 0.975))
  rii_ci <- quantile(bootstrap_results["rii.rank",], c(0.025, 0.975))

  return(data.frame(
    year = year_val,
    SII = original_result$SII,
    SII_lower = sii_ci[1],
    SII_upper = sii_ci[2],
    RII = original_result$RII,
    RII_lower = rii_ci[1],
    RII_upper = rii_ci[2]
  ))
}

# Calculate for all years with CI
sii_rii_ci_results <- map_df(unique(wealth_vbd$year),
                             ~calculate_sii_rii_ci(wealth_vbd, .x, n_bootstrap = 500))

# 10. Create the RII trend plot with confidence intervals
p_rii_trend <- ggplot(sii_rii_ci_results, aes(x = year, y = RII)) +
  # Confidence intervals
  geom_errorbar(aes(ymin = RII_lower, ymax = RII_upper),
                width = 0.3, color = "darkorange", size = 0.8) +
  # RII points
  geom_point(size = 3, color = "darkorange") +
  # Linear trend line
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen", size = 1) +
  # Reference line at 1 (no inequality)
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  # Add trend statistics
  labs(
    title = "Relative Index of Inequality (RII) Over Time",
    subtitle = {
      # Calculate trend
      trend_model <- lm(RII ~ year, data = sii_rii_ci_results)
      paste0("Linear regression: β = ", round(coef(trend_model)[2], 3),
             ", p ", ifelse(summary(trend_model)$coefficients[2,4] < 0.001, "< 0.001",
                            paste("=", round(summary(trend_model)$coefficients[2,4], 3))))
    },
    x = "Year",
    y = "Relative index of inequality",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  scale_y_continuous(limits = c(-1, max(sii_rii_ci_results$RII_upper) * 1.1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11)
  )

print(p_rii_trend)

# 11. Create the SII trend plot with confidence intervals
p_sii_trend <- ggplot(sii_rii_ci_results, aes(x = year, y = SII)) +
  # Confidence intervals
  geom_errorbar(aes(ymin = SII_lower, ymax = SII_upper),
                width = 0.3, color = "darkblue", size = 0.8) +
  # SII points
  geom_point(size = 3, color = "darkblue") +
  # Linear trend line
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen", size = 1) +
  # Reference line at 0 (no inequality)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # Add trend statistics
  labs(
    title = "Slope Index of Inequality (SII) Over Time",
    subtitle = {
      # Calculate trend
      trend_model <- lm(SII ~ year, data = sii_rii_ci_results)
      paste0("Linear regression: β = ", round(coef(trend_model)[2], 2),
             ", p ", ifelse(summary(trend_model)$coefficients[2,4] < 0.001, "< 0.001",
                            paste("=", round(summary(trend_model)$coefficients[2,4], 3))))
    },
    x = "Year",
    y = "Slope index of inequality",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11)
  )

print(p_sii_trend)

# 12. Combined plot showing both SII and RII trends
# Normalize for comparison
sii_rii_combined <- sii_rii_ci_results %>%
  pivot_longer(cols = c(SII, RII), names_to = "Index", values_to = "Value") %>%
  pivot_longer(cols = c(SII_lower, RII_lower), names_to = "Index_lower", values_to = "Lower") %>%
  pivot_longer(cols = c(SII_upper, RII_upper), names_to = "Index_upper", values_to = "Upper")

# Print summary of trends
print("\nSII and RII with Confidence Intervals:")
print(sii_rii_ci_results)

# Test significance of trends
sii_trend_model <- lm(SII ~ year, data = sii_rii_ci_results)
rii_trend_model <- lm(RII ~ year, data = sii_rii_ci_results)

print("\nSII Trend Analysis:")
print(summary(sii_trend_model))

print("\nRII Trend Analysis:")
print(summary(rii_trend_model))
