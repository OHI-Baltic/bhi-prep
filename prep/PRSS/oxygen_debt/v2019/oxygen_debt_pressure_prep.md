Oxygen Debt Pressure Layer Data Preparation
================

<br>

``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE, results = "hide", fig.width = 9.5, fig.height = 6)
source(here::here("R", "setup.R"))
loc <- here::here("prep", "pressures", "open_sea_anoxia")
data_path <- here::here("data", "pressures", "oxygen_debt", version_year, "oxygen_debt_pressure_data.rmd")
```

<br>

## 1\. Background

HELCOM uses [anoxia as an applicable
indicator](http://www.helcom.fi/baltic-sea-trends/indicators/oxygen/) in
the Bornholm Basin, Western Gotland Basin, Eastern Gotland Basin,
Northern Baltic Proper and Gulf of Finland. Other areas are not included
in this study but at the present time have no anoxia problems -
therefore they will recieve a pressure value of NA.

### 1.1 Pressure Model

This pressure layer is the inverse of the Eutrophication subgoal oxygen
debt indicator
status.

### 1.2 Reference points

<!-- Data sources and data preparation are found in the [Clean Water subgoal Eutrophication](https://github.com/OHI-Science/bhi-prep/blob/master/data/CW/eutrophication/v2019/eut_data.rmd) which uses secchi data as one measure of nutrient status. The respective targets are also saved and organized in the Eutrophication goal prep.   -->

## 2\. Data

This prep document is used to generate and explore the following data
layer:

  - `hab_oxygen_debt_bhi2019.csv`

This is saved to the `layers` folder. The layer is derived from the raw
datasets from the ICES Oceanographic database.

<br>

### 2.1 Datasets with Sources

<br/>

#### 2.1.1 Oxygen Data

Data are the same as were used for the oxygen debt clean water
indicator. More about the data sources and data preparation can be found
in the [Clean Water subgoal
Eutrophication](https://github.com/OHI-Science/bhi-prep/blob/master/data/CW/eutrophication/v2019/eut_data.rmd)
and the [BHI repository forked from ICES tools HEAT assessment tool
repository](https://github.com/OHI-Baltic/HEAT).

<br>

**ICES Oxygen Data**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/OxygenDebt_ICES -->

Data downloaded from the [ICES
database](https://ocean.ices.dk/HydChem/HydChem.aspx?plot=yes). Each
dataset has been extracted by country and Denmark split into five (5)
because too big otherwise.

**ICES CTD Data**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/OxygenDebt_ICES -->

Data downloaded from the [ICES
database](https://ocean.ices.dk/HydChem/HydChem.aspx?plot=yes). Each
dataset has been extracted by country, Denmark split into three (3) and
Germany into five (5) because too big otherewise. No CTD found for
Lithuania.

**SMHI CTD Data**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/OxygenDebt_ICES -->

Data downloaded from the [SMHI Database](https://sharkweb.smhi.se/),
Physical and Chemical parameters, CTD Profiles.

<br>

<br/>

## 3\. Prep: Wrangling & Derivations, Checks/Evaluation, Gapfilling

### 3.1 Calculate Oxygen Debt Status Scores

For details on data preparation for oxygen debt layer, see [this github
repository](https://github.com/OHI-Baltic/HEAT), forked from ICE HEAT
assessment tool for fitting oxygen debt profiles to oxygen data
measurements. Oxygen data measurements used were obtained from ICE and
merged as documented in section 2.2.4 of the Eutrophication data prep
document.

``` r
heat_output <- "https://raw.githubusercontent.com/OHI-Baltic/HEAT/master/analysis/output/"
oxydebt <- read_csv(paste0(heat_output,"OxygenDebt/uncorrected_indicator_table_by_year_2000_2019.csv"))

## read in target values for eutrophication indicators
eut_thresholds <- read.csv(file.path(dir_layers, "cw_eut_targets_bhi2019.csv")) %>% 
  filter(indicator == "oxyg_debt") %>%
  select(helcom_id, target = value)
```

``` r
basin_lookup <- read_csv(file.path(dir_prep, "supplement", "lookup_tabs", "rgns_complete.csv")) %>% 
  select(region_id, subbasin, helcom_id)

oxydebt <- oxydebt %>% 
  select(-AssessmentUnitID) %>% 
  mutate(AssessmentUnitName = case_when(
    AssessmentUnitName == "Gulf of Gdansk" ~ "Gdansk Basin",
    str_detect(AssessmentUnitName, "land Sea") ~ "Aland Sea",
    TRUE ~ AssessmentUnitName
  )) %>% 
  rename(subbasin = AssessmentUnitName, year = Year)

## only Bornholm Basin and Baltic Proper have target values, the other subbasins are too shallow
## also, the sub-basins within Bornholm Basin and Baltic Proper have uniform values within each year...
oxydebt_rgns <- oxydebt %>% 
  left_join(basin_lookup, by = "subbasin") %>% 
  left_join(filter(eut_thresholds, str_detect(helcom_id, "^SEA")),  by = "helcom_id")

## Calculate basin status as: basin_mean/basin_target, constraining values to 1 as maximum score
oxydebt_status_allyrs <- oxydebt_rgns %>%
  mutate(status = pmin(1, target/ES))
```

<br>

### 3.2 Calculate Oxygen Debt Pressure Scores and Save Layer

``` r
inv_oxydebt <- oxydebt_status_allyrs %>%
  mutate(pressure_score = 1 - status) %>% 
  select(region_id, year, subbasin, ES, target, status, pressure_score)

d <- expand.grid(list(region_id = 1:42, year = 2000:2019), stringsAsFactors = FALSE)
inv_oxydebt <- dplyr::left_join(d, inv_oxydebt, by = c("region_id", "year"))

write_csv(
  inv_oxydebt %>% 
    select(region_id, year, oxydebt_pressure = pressure_score), 
  file.path(dir_layers, "hab_oxygen_debt_bhi2019.csv")
)
```

<br>

## 4\. Visualizing Data Layers

### 4.1 Oxygen Debt Pressure Scores by Subbasin

``` r
plotdf <- inv_oxydebt %>% 
  filter(!is.na(subbasin), !is.na(target)) %>% 
  distinct(year, subbasin, pressure_score)

## many subbasins were not included in HEAT assessment tool and/or had no target
ggplot(plotdf) +
  geom_col(aes(x = year, y = pressure_score)) +
  facet_wrap(~subbasin, scales = "free_y", nrow = 3) +
  labs(x = NULL, y = "Oxygen Debt Pressure Score")
```

![](oxygen_debt_pressure_prep_files/figure-gfm/oxygen%20debt%20pressure%20scores-1.png)<!-- -->

### 4.2 Oxygen Debt Pressure Scores by Subbasin

``` r
bhi_rgn <- sf::st_read(file.path(dirname(dir_B), "Shapefiles", "BHI_shapefile"))
bhi_rgn_simple <- rmapshaper::ms_simplify(input = bhi_rgn) %>% 
  sf::st_as_sf() %>% 
  select(region_id = BHI_ID, rgn_nam, subbasin = Subbasin) %>% 
  mutate(region_nam = paste(rgn_nam, subbasin, sep  = ", "))

basemap <- ggplot2::ggplot(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf(size = 0.1, color = "burlywood", alpha = 0.4) +
  theme(panel.background = element_rect(fill = "#F8FBFC", color = "#E2EEF3")) +
  scale_x_continuous(limit = c(4, 32)) +
  scale_y_continuous(limit = c(53.5, 66)) 

cols <- c("darkred", "indianred", "coral", "goldenrod1", "khaki", "lightblue", "steelblue")
```

``` r
basemap +
  geom_sf(
    data = left_join(
      bhi_rgn_simple,
      inv_oxydebt  %>% 
        # filter(year %in% seq(2005, 2019, by = 2)),
        # filter(year %in% c(2000, 2004, 2012, 2014, 2016:2019)) %>% 
        select(region_id, year, pressure_score),
      by = "region_id"
    ),
    mapping = aes(fill = pressure_score),
    alpha = 0.8,
    size = 0.15
  ) +
  scale_fill_gradientn(
    colors = rev(cols),
    values = c(0, 0.15, 0.25, 0.5, 0.65, 0.85, 1),
    limits = c(0, 1)
  ) +
  facet_wrap(~year)
```

![](oxygen_debt_pressure_prep_files/figure-gfm/oxygen%20debt%20pressure%20map-1.png)<!-- -->

``` r
  # facet_wrap(~year, nrow = 2)
```
