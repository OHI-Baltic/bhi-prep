Secchi Depth Pressure Layer Data Preparation
================

<br>

``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE, results = "hide", fig.width = 9.5, fig.height = 6)
source(here::here("R", "setup.R"))
loc <- here::here("prep", "pressures", "pressure_secchi")
data_path <- here::here("data", "pressures", "pressure_secchi", version_year, "pressure_secchi_data.rmd")
```

<br>

## 1\. Background

Water clarity / transparency is a direct pressure on Lasting Special
Places and Tourism as it affects people’s perception of aesthetic
properties of nature. Water clarity is measured using Secchi depth data.

### 1.1 Pressure Model

This pressure layer is the inverse of the Eutrophication subgoal secchi
indicator status, for the most recent year. The most recent year varies
among sub-basins (the scale at which the Secchi indicator status is
calculated).

### 1.2 Reference points

Data sources and data preparation are found in the [Clean Water subgoal
Eutrophication](https://github.com/OHI-Science/bhi-prep/blob/master/data/CW/eutrophication/v2019/eut_data.rmd)
which uses secchi data as one measure of nutrient status. The respective
targets are also saved and organized in the Eutrophication goal prep.

## 2\. Data

This prep document is used to generate and explore the following data
layer:

  - `po_inverse_secchi_bhi2019.csv`

This is saved to the `layers` folder. The layer is derived from the raw
datasets from the ICES Oceanographic
database.

<br>

### 2.1 Datasets with Sources

<br/>

#### 2.1.1 Secchi Depth

<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/SecchiDepth_ICES -->

Data are the same as were used for the secchi clean water indicator,
obtained from the [ICES Oceanographic
database](http://ecosystemdata.ices.dk/inventory/Index.aspx?). More
about the data sources and data preparation can be found in the [Clean
Water subgoal
Eutrophication](https://github.com/OHI-Science/bhi-prep/blob/master/data/CW/eutrophication/v2019/eut_data.rmd)
which uses secchi data as one measure of eutrophication
status.

<br>

<br/>

## 3\. Prep: Wrangling & Derivations, Checks/Evaluation, Gapfilling

``` r
## read in the secchi depth data, prepared in the eutrophication goal prep
secchi_depths <- read.csv(file.path(dir_layers, "cw_eut_secchi_bhi2019.csv"))

## read in target values for eutrophication indicators
cw_eut_targets <- read.csv(file.path(dir_layers, "cw_eut_targets_bhi2019.csv")) %>% 
  filter(indicator == "summer_secchi") %>%
  select(helcom_id, target = value)
```

### 3.1 Calculate Secchi Status Scores

``` r
## join regions info for grouping by subbasins
## also if there is no helcom_id column, add one with subbasin_id values
rgns <- read_csv(file.path(dir_prep, "supplement", "lookup_tabs", "rgns_complete.csv"))
secchi_depths <- left_join(secchi_depths, rgns, by = "region_id")


## Calculate mean monthly value for each month
monthly_means <- secchi_depths %>%
  group_by(subbasin_id, year, month) %>%
  summarise(
    month_mean = mean(secchi_depth, na.rm = TRUE),
    month_sd = sd(secchi_depth, na.rm = TRUE),
    n_pts = n()
  ) %>%
  mutate(month_sd = ifelse(is.na(month_sd), 0, month_sd)) %>%
  ungroup()


## Calculate seasonal mean for each year, with min max and 5-year moving average
seasonal_means <- monthly_means %>%
  group_by(subbasin_id, year) %>%
  summarise(
    season_mean = mean(month_mean, na.rm = TRUE),
    season_sd = sqrt(mean((month_sd^2), na.rm = TRUE)),
    n_pts = sum(n_pts)
  ) %>%
  ungroup() %>%
  arrange(subbasin_id, year) %>%
  group_by(subbasin_id) %>%
  mutate(moving_avg_5yr = zoo::rollapply(season_mean, 5, mean, na.rm = TRUE, align = "right", fill = NA)) %>%
  mutate(indicator_min = season_mean-season_sd, indicator_max = season_mean+season_sd) %>%
  ungroup()


## calculate Secchi indicator status
## join regions lookup expands from subbasins to one row per BHI region
status_all_yrs <- seasonal_means %>%
  mutate(subbasin_id = as.character(as.numeric(subbasin_id))) %>%
  rowwise() %>%
  mutate(helcom_id = sprintf("SEA-0%s", substr(subbasin_id, 2, 3))) %>%
  left_join(cw_eut_targets, by = "helcom_id") %>%
  select(-helcom_id) %>% 
  mutate(ratio = moving_avg_5yr/target) %>%
  mutate(status = pmin(1, ratio)) %>%
  ungroup() %>%
  mutate(subbasin_id = as.numeric(subbasin_id)) %>% 
  left_join(rgns, by = "subbasin_id")
```

### 3.2 Calculate Secchi Pressure Scores from Secchi Status

``` r
inv_secchi <- status_all_yrs %>%
  mutate(pressure_score = 1 - status) %>%
  filter(!is.na(pressure_score))
```

### 3.3 Save the Secchi Pressure Layers

Save Secchi depth pressure data as csv in layers folder.

``` r
write_csv(
  inv_secchi %>% 
    select(region_id, year, pressure_score), 
  file.path(dir_layers, "po_inverse_secchi_bhi2019.csv")
)
```

<br>

## 4\. Visualizing Data Layers

### 4.1 Secchi (Water Clarity/Transparency) Pressure Scores by Subbasin

``` r
ggplot(distinct(inv_secchi, year, subbasin, pressure_score)) +
  geom_col(aes(x = year, y = pressure_score)) +
  facet_wrap(~subbasin, scales = "free_y", nrow = 6) +
  labs(x = NULL, y = "Secchi (Water clarity / transparency), Pressure Score")
```

![](pressure_secchi_prep_files/figure-gfm/secchi%20pressure%20scores-1.png)<!-- -->

### 4.2 Secchi (Water Clarity/Transparency) Pressure Scores by Subbasin

``` r
source(here::here("R", "spatial.R"))

bhi_rgn <- st_read(file.path(dirname(dir_B), "Shapefiles", "BHI_shapefile"))
bhi_rgn_simple <- rmapshaper::ms_simplify(input = bhi_rgn) %>% 
  sf::st_as_sf() %>% 
  select(region_id = BHI_ID, rgn_nam, subbasin = Subbasin) %>% 
  mutate(region_nam = paste(rgn_nam, subbasin, sep  = ", "))

basemap <- ggplot2::ggplot(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf(size = 0.1, color = "burlywood", alpha = 0.4) +
  theme(panel.background = element_rect(fill = "#F8FBFC", color = "#E2EEF3")) +
  scale_x_continuous(limit = c(4, 32)) +
  scale_y_continuous(limit = c(53.5, 66)) 

cols <- c("indianred", "coral", "goldenrod1", "khaki", "lightblue", "steelblue")
```

``` r
basemap +
  geom_sf(
    data = left_join(
      bhi_rgn_simple,
      inv_secchi %>% 
        select(region_id, year, pressure_score) %>% 
        # filter(year %in% seq(2005, 2019, by = 2)),
        filter(year %in% c(2004, 2007, 2010, 2013, 2016:2019)),
      by = "region_id"
    ),
    mapping = aes(fill = pressure_score),
    alpha = 0.8
  ) +
  scale_fill_gradientn(
    colors = rev(cols),
    values = c(0, 10^(-7), 0.3, 0.65, 0.8, 1),
    limits = c(0, 1)
  ) +
  facet_wrap(~year, nrow = 2)
```

![](pressure_secchi_prep_files/figure-gfm/secchi%20pressure%20map-1.png)<!-- -->

<br>
