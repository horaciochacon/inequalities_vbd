# VBD Peru Analysis: Malaria and Dengue Spatial Epidemiology

A comprehensive R analysis pipeline for studying malaria and dengue patterns in Loreto, Peru, with focus on small area estimation and health inequality assessment at the district level.

## Overview

This project analyzes vector-borne disease surveillance data to:

- **Characterize disease patterns** at the regional and district level in Loreto
- **Implement small area estimation** techniques for robust district-level estimates
- **Assess health inequalities** through concentration curves and inequality metrics
- **Generate actionable insights** for public health decision-making

## Key Features

- 🗺️ **Spatial Analysis**: District-level mapping and spatial clustering detection
- 📊 **Small Area Estimation**: Fay-Herriot models with auxiliary variables
- 📈 **Inequality Metrics**: Gini coefficients, concentration indices, and slope indices  
- ⚙️ **Configurable Pipeline**: YAML-based parameter management
- 🔄 **Reproducible Workflow**: Sequential scripts with intermediate result caching
- 📋 **Quality Control**: Automated data validation and outlier detection

## Project Structure

```
vbd_peru/
├── config.yml          # Analysis parameters and settings
├── R/                   # Reusable functions
├── src/                 # Sequential analysis scripts (01-08)
├── data/               # Raw and processed datasets
├── output/             # Results, figures, and reports
└── docs/               # Documentation and methodology
```

See [CLAUDE.md](CLAUDE.md) for detailed project guidelines and coding standards.

## Quick Start

### Prerequisites

- R ≥ 4.1.0
- RStudio (recommended)
- Git

### Installation

1. **Clone the repository**:
   ```bash
   git clone <repository-url>
   cd vbd_peru
   ```

2. **Open in RStudio**:
   ```r
   # Open vbd_peru.Rproj in RStudio
   ```

3. **Install dependencies**:
   ```r
   # Install renv if not already installed
   if (!requireNamespace("renv", quietly = TRUE)) {
     install.packages("renv")
   }
   
   # Restore project dependencies
   renv::restore()
   ```

4. **Configure analysis parameters**:
   ```r
   # Edit config.yml to match your data sources and analysis preferences
   config <- config::get()
   print(config)
   ```

### Data Preparation

Place your data files in the appropriate folders:

```
data/
├── raw/
│   ├── malaria_surveillance_2015_2023.csv
│   ├── dengue_surveillance_2015_2023.csv
│   └── population_loreto_districts.csv
├── spatial/
│   ├── loreto_districts.shp
│   └── establecimientos_salud.shp
└── external/
    └── socioeconomic_indicators.csv
```

Expected data format:
- **Disease surveillance**: columns for date, district_code, cases, population
- **Spatial data**: Shapefiles with district boundaries
- **Auxiliary variables**: Poverty rates, education levels, health access metrics

### Running the Analysis

Execute the complete pipeline:

```r
# Load required libraries
library(config)
library(tidyverse)
library(sf)

# Load configuration
config <- config::get()

# Run sequential analysis scripts
source("src/01_data_import.R")      # Import and validate data
source("src/02_data_cleaning.R")    # Clean and preprocess
source("src/03_exploratory.R")      # Exploratory analysis
source("src/04_spatial_prep.R")     # Prepare spatial data
source("src/05_small_area_est.R")   # Small area estimation
source("src/06_inequality.R")       # Inequality analysis
source("src/07_visualization.R")    # Generate plots and maps
source("src/08_reporting.R")        # Create final reports
```

Or run individual components as needed.

## Key Analysis Components

### 1. Disease Surveillance Analysis
- Temporal trend analysis with seasonal decomposition
- Spatial clustering detection using local Moran's I
- Disease burden estimation by district

### 2. Small Area Estimation
- Fay-Herriot models with auxiliary variables (poverty, education, health access)
- Uncertainty quantification through bootstrap methods
- Model diagnostics and validation

### 3. Inequality Assessment
- Concentration curves for disease burden by socioeconomic status
- Gini coefficients and concentration indices
- Slope and relative inequality indices
- Geographic inequality patterns

### 4. Visualization and Reporting
- Interactive maps with disease rates and uncertainty intervals
- Time series plots with forecasting
- Inequality curves and summary statistics
- Automated HTML reports with methodology

## Configuration

The `config.yml` file controls all analysis parameters:

```yaml
# Example key parameters
analysis:
  temporal:
    aggregation: "monthly"
    lag_periods: 12
  
  spatial:
    smoothing_method: "INLA"
    minimum_cases: 5
  
  small_area_estimation:
    model_type: "Fay-Herriot"
    auxiliary_variables:
      - poverty_rate
      - education_level
```

Modify these parameters to customize the analysis for your specific needs.

## Output

The analysis generates:

- **Figures**: High-resolution maps, time series plots, inequality curves
- **Tables**: District-level estimates with uncertainty intervals
- **Reports**: Comprehensive HTML reports with methodology and results
- **Data**: Processed datasets for further analysis

All outputs are saved in the `output/` directory with timestamps.

## Quality Control

Built-in quality assurance includes:

- Data validation checks (completeness, outliers, logical constraints)
- Model diagnostics and convergence monitoring  
- Sensitivity analyses for key parameters
- Automated testing of core functions

## Contributing

1. Follow the coding standards outlined in [CLAUDE.md](CLAUDE.md)
2. Add tests for new functions
3. Update documentation for any changes
4. Use descriptive commit messages

## Methodology

This analysis implements established methods for small area estimation and health inequality assessment:

- **Small Area Estimation**: Fay-Herriot area-level models (Rao & Molina, 2015)
- **Spatial Analysis**: Spatial statistics and disease mapping (Lawson, 2018)
- **Inequality Metrics**: Health inequality measurement (Harper & Lynch, 2005)

See `docs/methodology.md` for detailed statistical methods and references.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contact

For questions or collaboration opportunities, please open an issue or contact the development team.

## Acknowledgments

- **MINSA Peru**: Disease surveillance data
- **INEI**: Population and demographic data
- **DIRESA Loreto**: Regional health data and expertise