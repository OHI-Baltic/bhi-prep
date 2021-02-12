Climate Change Pressure Layer Data Preparation
================

<br>

``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE, results = "hide", fig.width = 9.5, fig.height = 6)
source(here::here("R", "setup.R"))
loc <- here::here("prep", "pressures", "climate_change")
data_path <- here::here("data", "pressures", "climate_change", version_year, "climate_change_data.rmd")
```

<br>

## 1\. Background

The direct effects of climate change in the abiotic environment will be
changes to water temperature and salinity. In the BHI framework, we will
include sea surface temperature (SST) and surface salinity (SS) as
climate change pressures. Depth of bottom water varies by basin. In
BHI1.0 we also used bottom water salinity (BWS), but were not able to
obtain this (modeled) data for more recent years to include. The Climate
Change layer can negatively affect scores for different goals, in
particular the Artisanal Fishing Opportunities (AO) goal, the Carbon
Storage (CS) goal, the Fisheries (FIS) sub-goal, the Iconic Species
(ICO) sub-goal, and the Biodiversity (BD). The Tourism (TR) is
particularly affected by the sea surface temperature (SST).

### 1.1 Notes on the Data

In BHI 1.0, data were extracted from the [BALTSEM
model](http://www.balticnest.org/balticnest/thenestsystem/baltsem.4.3186f824143d05551ad20ea.html),
run by Bärbel Muller-Karulis from the Baltic Sea Centre at Stockholm
University. In BHI 2.0, since the BALTSEM is not as easily accessible,
[data from HELCOM stations, obtained from
ICES](https://ocean.ices.dk/Helcom/Helcom.aspx?Mode=1) was used instead.

These pressure layers are both calculated on the sub-basin scale, the
scores which are then applied for all BHI regions within the respective
sub-basin.

### 1.2 Reference points and Rescaling

Projections from two models. One set of projection scenarios use BALTSEM
results run with forcing from the
[ECHAM5](http://www.mpimet.mpg.de/en/science/models/echam/) global
climate model for the scenario A1b. Project goes to year 2100. The
second set of projection scenarios use BALTSEM results run with forcing
from the
[HADCM3](http://www.metoffice.gov.uk/research/modelling-systems/unified-model/climate-models/hadcm3)
global climate model for the scenario A1b. Projection goes to year 2099.

These projections are used to identify reasonable lower reference point
for salinity and upper reference point for temperature. Salinity is
projected to decrease with climate change, therefore we take the maximum
from the historical period and the minimum from the current period.
Temperature is projected to increase with climate change and therefore
we take the minimum from the historical period and maximum from the
projection.

Current conditions = mean of recent 5 years

min salinity value = minimum annual salinity during the future
projection period (2020-2050)

max salinity value = maximum annual salinity during reference period
(1960-1990)

min temperature value = minimum annual temperature during reference
period (1960-1990)

max temperature value = maximum annual temperature during the future
projection period (2020-2050)

Note: Greater pressure occurs with lower salinity, so will need to take
the inverse of the rescaling

<br/>

## 2\. Data

This prep document is used to generate and explore the following data
layer:

  - `cc_sst_bhi2019.csv`
  - `cc_sal_surf_bhi2019.csv`

These are saved to the `layers` folder. These are derived from or
informed by the raw Oceanographic datasets obtained from
ICES.

<br>

### 2.1 Datasets with Sources

<br/>

#### 2.1.1 Sea Surface Temperature & Surface Salinity

<!-- dataset save location BHI_share/BHI 2.0/Pressure/climate_change/Helcom/helcom.csv -->

In BHI 2.0, since the BALTSEM is not as easily accessible, [data from
HELCOM stations, obtained from
ICES](https://ocean.ices.dk/Helcom/Helcom.aspx?Mode=1) were used
instead, only including sea surface temperature (SST) and surface
salinity (SS) (as bottom water salinity -BWS- is not available). Data
was downloaded on 24 August 2020 by Andrea De Cervo.

#### 2.1.2 Surface Salinity & Temperature Projections

From BHI 1.0, we have data extracted from the [BALTSEM
model](http://www.balticnest.org/balticnest/thenestsystem/baltsem.4.3186f824143d05551ad20ea.html),
run by Bärbel Muller-Karulis from the Baltic Sea Centre at Stockholm
University.

<br>

<br/>

## 3\. Prep: Wrangling & Derivations, Checks/Evaluation, Gapfilling

Two data layers, surface water salinity and temperature.

``` r
## read in the climate raw data from ICES
## data.table::fread is faster than read.csv or readr
ices_cc_data <- data.table::fread(
  file.path(dir_B, "Pressure", "climate_change", "Helcom",  "helcom.csv"), 
  stringsAsFactors = FALSE
)
```

``` r
source(here::here("R", "spatial.R"))
regions_shape()
```

<br>

### 3.1 Initial Cleaning

``` r
ices_cc_data <- ices_cc_data %>% 
  dplyr::mutate(
    date = as.Date(`yyyy-mm-ddThh:mm`),
    year = format(date, "%Y"), 
    month = format(date, "%m"),
    month = str_replace(month, "^0+", ""),
    cruise = as.character(Cruise),
    station = as.character(Station),
    depth = as.numeric(`Bot. Depth [m]`)
  ) %>% 
  dplyr::select(
    year,
    month,
    cruise,
    station,
    latitude = `Latitude [degrees_north]`, 
    longitude = `Longitude [degrees_east]`,
    depth,
    temp = `TEMP [deg C]`,
    surfsal = `PSAL [psu]`
  ) %>% 
  filter(year >= 1960)
```

### 3.2 Match Monitoring Stations to Subbasins

``` r
cc_data_w_rgns_sp <- join_rgns_info(
  ices_cc_data, helcomID_col = "helcom_id", country_col = "country", 
  latlon_vars = c("latitude", "longitude"), return_spatial = TRUE, 
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefiles"), 
  buffer_shp = NULL
) 
```

### 3.2 Check Spatial Assignment of Data, Distributions, Timeseries

``` r
bhi_rgn <- st_read(file.path(dirname(dir_B), "Shapefiles", "BHI_shapefile"))
bhi_rgn_simple <- rmapshaper::ms_simplify(input = bhi_rgn) %>% 
  sf::st_as_sf() %>% 
  select(region_id = BHI_ID, rgn_nam, subbasin = Subbasin) %>% 
  mutate(region_nam = paste(rgn_nam, subbasin, sep  = ", "))
```

``` r
library(leaflet)

## check points not assigned to BHI regions...
## first, to speed mapping, identify unique lat/long missing BHI regions
## also reproject to match leaflet epsg:3857 proj??
make_datapts_leafletmap <- function(mapdf){
  mapdf <- sf::st_transform(mapdf, crs = 3857)
  data.table::setDT(mapdf)
  selectedcols <- c("latitude", "longitude")
  mapdf <- mapdf[!duplicated(mapdf, by = selectedcols), selectedcols, with = FALSE]
  
  mapdatapts <- leaflet::leaflet(data = mapdf) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(18, 59, zoom = 5) %>%
    addCircleMarkers(radius = 2) %>% 
    addPolygons(data = bhi_rgn_simple, fillOpacity = 0.1, fill = "grey", weight = 0.5)

  return(mapdatapts)
}
# make_datapts_leafletmap(filter(cc_data_w_rgns_sp, is.na(BHI_ID)))
## seems like there are many points near north sea or in denmark which are and should be exculded
## but others especially along Finnish coast, where there are lots of islands, should be included...


## going back to assignment fo BHI regions (join_rgns_info) and including the coastal buffer, 
## more points are assigned to BHI regions...
bhibuffer <- st_read(file.path(dirname(dir_B), "Shapefiles", "BHI_shapefile_25km_buffer"))
cc_data_w_rgns_sp <- join_rgns_info(
  ices_cc_data, helcomID_col = "helcom_id", country_col = "country", 
  latlon_vars = c("latitude", "longitude"), return_spatial = TRUE, 
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefiles"), 
  ## using buffer to assign more of the points, will need still to exclude eg north sea or estuaries...
  buffer_shp = bhibuffer
) 
## keep if not in the buffer zone, or
## if in the buffer zone, keep if not in BHI regions 1, 2, 8, 10
## also filter beyond certain depth, as we just want the surface layer??
cc_data_w_rgns_sp <- cc_data_w_rgns_sp %>% 
  filter(!in_25km_buffer|(in_25km_buffer & !BHI_ID %in% c(1, 2, 8, 10))) %>% 
  # filter((depth <= 50)|(!is.na(surfsal) & is.na(depth))) %>%
  mutate(month = as.numeric(month), year = as.numeric(year))

## recheck points included, with leaflet map
make_datapts_leafletmap(cc_data_w_rgns_sp)

cc_data_w_rgns <- sf::st_drop_geometry(cc_data_w_rgns_sp)
```

``` r
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
    data = st_as_sf(cc_data_w_rgns, coords = c("longitude", "latitude"), crs = 4326) %>% 
      filter(year %in% c(1998, 2008, 2018), !is.na(surfsal)),
    mapping = aes(color = surfsal),
    size = 2,
    alpha = 0.7
  ) +
  scale_color_gradientn(
    colors = c("khaki", "lightblue", "steelblue"),
    values = c(0, 0.4, 1)
  ) +
   labs(color = "Salinity") +
  facet_grid(cols = vars(year))
```

![](climate_change_prep_files/figure-gfm/surface%20salinity%20data%20points%20map-1.png)<!-- -->

``` r
basemap +
  geom_sf(
    data = st_as_sf(cc_data_w_rgns, coords = c("longitude", "latitude"), crs = 4326) %>% 
      filter(year %in% c(1998, 2008, 2018), !is.na(temp)),
    mapping = aes(color = temp),
    size = 2,
    alpha = 0.2
  ) +
  scale_color_gradientn(
    colors = c("khaki", "lightblue", "steelblue"),
    values = c(0, 0.4, 1)
  ) +
  labs(color = "SST") +
  facet_grid(cols = vars(year))
```

![](climate_change_prep_files/figure-gfm/sea%20surface%20temperatures%20data%20points%20map-1.png)<!-- -->

<br>

### 3.3 Current Conditions

Current conditions if taken from the hindcast data. Each scenario year
takes average from the five most recent years (e.g. for 2019 assessment,
pressure score is derived from 2015-2019 average, rescaled).

Note that the climate projections for temperature are for months
July-August, so we will calculate temperature averages from these months
rather than the entire year.

``` r
surf_sal_current <- cc_data_w_rgns %>%
  group_by(Subbasin, year, month) %>% 
  summarize(monthly_mean_surfsal = mean(surfsal, na.rm = TRUE)) %>% 
  group_by(Subbasin, year) %>% 
  summarize(annual_mean_surfsal = mean(monthly_mean_surfsal, na.rm = TRUE)) %>% 
  arrange(Subbasin, year) %>% 
  group_by(Subbasin) %>%
  mutate(
    ma5_surf_sal = zoo::rollapply(annual_mean_surfsal, 5, mean, na.rm = TRUE, align = "right", fill = NA),
    year = as.numeric(year)
  ) %>% 
  rename(basin_name_holas = Subbasin) %>% 
  ungroup() %>% 
  arrange(desc(year))

surf_temp_current <- cc_data_w_rgns %>%
  filter(month %in% 7:8) %>% 
  group_by(Subbasin, year, month) %>% 
  summarize(monthly_mean_temp = mean(temp, na.rm = TRUE)) %>% 
  group_by(Subbasin, year) %>% 
  summarize(julaug_mean_temp = mean(monthly_mean_temp, na.rm = TRUE)) %>% 
  arrange(Subbasin, year) %>% 
  group_by(Subbasin) %>%
  mutate(
    ma5_julaug_surf_temp = zoo::rollapply(julaug_mean_temp, 5, mean, na.rm = TRUE, align = "right", fill = NA),
    year = as.numeric(year)
  ) %>% 
  rename(basin_name_holas = Subbasin) %>% 
  ungroup() %>% 
  arrange(desc(year))

## plot timeseries of surface salinity and temp, annual means and 5year moving average
## data for BHI1.0 had ECHAM5 model data, projections; we don't have that here, only have hindcast... 
gridExtra::grid.arrange(
  ggplot(tidyr::pivot_longer(surf_sal_current, cols = c("annual_mean_surfsal", "ma5_surf_sal"))) +
    geom_point(aes(year, value, color = name), alpha = 0.6) +
    facet_wrap(~basin_name_holas, scales = "free_y", ncol = 4) +
    theme(legend.position = c(0.9, 0.1)) +
    scale_x_continuous(breaks = c(1960, 1975, 1990, 2005, 2020)) +
    labs(color = "Name", x = NULL, y = "Annual Average Salinity (PSU)"),
   ggplot(tidyr::pivot_longer(surf_temp_current, cols = c("julaug_mean_temp", "ma5_julaug_surf_temp"))) +
    geom_point(aes(year, value, color = name), alpha = 0.6) +
    facet_wrap(~basin_name_holas, scales = "free_y", ncol = 4) +
    theme(legend.position = c(0.9, 0.1)) +
    scale_x_continuous(breaks = c(1960, 1975, 1990, 2005, 2020)) +
    labs(color = "Name", x = NULL, y = "Jul-Aug Average Temp (deg C)"),
  nrow = 1
)
```

![](climate_change_prep_files/figure-gfm/current%20surface%20sal%20and%20temp%20ices-1.png)<!-- -->

### 3.4 Maximums and Minimums for Normalizing Salinity and Temperature Averages

Projections data, to get referece points for rescaling surface
temperature and salinity. Upper reference point for temperature and
lower reference point for salinity are derived based on projected future
max and min
respectively.

``` r
# baltsem_basins_lookup <- read_csv(file.path(dir_B, "Pressure", "climate_change", "Projections", "baltsem_basins_lookup.csv"))
baltsem_basins_lookup <- read_csv(file.path(dir_B, "Pressure", "Projections", "baltsem_basins_lookup.csv"))

load_files <- list.files(
  file.path(dir_B, "Pressure", "climate_change", "Projections"), 
  # file.path(dir_B, "Pressure", "climate_change", "Projections"), 
  recursive = TRUE, full.names = TRUE
)
cc_proj_df <- do.call(rbind, lapply(
  grep("_projection.*.csv", load_files, value = TRUE), 
  function(x){
    df <- read_csv(x) %>% 
      select(Year, matches("TEMP_O_5|SALIN_O_5")) %>% 
      mutate(model = ifelse(stringr::str_detect(x, "echam5"),  "echam5", "hadcm3")) %>% 
      mutate(basin_abb_baltsem = stringr::str_extract(x, "(?<=/Hydro_)[A-Z]{2}")) %>% 
      left_join(baltsem_basins_lookup, by = "basin_abb_baltsem")
    colnames(df) <- c(
      "year", "temp_o_5_julaug", "salin_o_5", "model", 
      "basin_abb_baltsem", "basin_name_baltsem", "basin_name_holas"
    )
    return(df)
  }
)) 
cc_proj_df <- cc_proj_df %>% 
  filter(!is.na(basin_name_holas)) %>% 
  select(year, temp_o_5_julaug, salin_o_5, model, basin_name_holas) %>% 
  filter(year %in% 1960:2050) %>% 
  mutate(timeframes = case_when(
    year %in% 2020:2050 ~ "future",
    year %in% 1990:2019 ~ "current",
    year %in% 1960:1990 ~ "past"
  ))
```

**Minimum Salinity, from Projection Period**

min salinity value = minimum annual salinity during the future
projection period (2020-2050)

From projections data, extract the minimum annual surface salinity for
each basin between 2020-2050. Compare between the two projection data
sets. ECHAM5 projects higher salinity than HADCM3, difference greatest
between the two in Great Belt, Kattegat, Kiel, The Sound (which are the
most variable and most saline).

``` r
min_salinity_plot <- cc_proj_df %>% 
  filter(timeframes == "future") %>% 
  select(year, salin_o_5, model, basin_name_holas)

min_salinity <- min_salinity_plot %>% 
  group_by(model, basin_name_holas) %>% 
  summarize(min_salinity = min(salin_o_5)) %>% 
  arrange(basin_name_holas)

ggplot(min_salinity_plot) + 
  geom_line(aes(year, salin_o_5, color = model)) + 
  facet_wrap(~basin_name_holas, scales = "free_y") +
  theme(legend.position = c(0.9, 0.15)) +
  labs(x = NULL, y = "Surface Salinity Projections (PSU)", color = "Model")
```

![](climate_change_prep_files/figure-gfm/min%20annual%20salinity-1.png)<!-- -->

**Maximum Temperature, from Projection Period**

max temperature value = maximum annual temperature during the future
projection period (2020-2050)

From projections data, extract the maximum annual surface temperature
for each basin between 2020-2050. Projections are fairly similar, with
HADCM3 generally predicting higher temperatures.

``` r
max_temp_plot <- cc_proj_df %>%
  filter(timeframes == "future") %>%
  select(year, temp_o_5_julaug, model, basin_name_holas)

max_temp <- max_temp_plot %>%
  group_by(model, basin_name_holas) %>%
  summarize(max_temperature = max(temp_o_5_julaug)) %>%
  arrange(basin_name_holas)

ggplot(max_temp_plot) +
  geom_line(aes(year, temp_o_5_julaug, color = model)) +
  facet_wrap(~basin_name_holas, scales = "free_y") +
  theme(legend.position = c(0.9, 0.15)) +
  labs(x = NULL, y = "Summer Sea Surface Temperature Projections (Jul-Aug, deg C)", color = "Model")
```

![](climate_change_prep_files/figure-gfm/max%20annual%20temperatures-1.png)<!-- -->

**Maximum Annual Salinity, from Hindcast**

max value = maximum annual salinity during reference period (1960-1990)

From hindcast data, extract the maximum annual surface salinity for each
basin between 1960-1990.

``` r
## take max from hindcast dataset
## use moving average or annual means??
max_surf_sal <- surf_sal_current %>%
  filter(year %in% 1960:1990) %>%
  select(basin_name_holas, ma5_surf_sal) %>%
  group_by(basin_name_holas) %>%
  summarise(max_surf_sal = max(ma5_surf_sal, na.rm = TRUE)) %>%
  ungroup()
```

**Minimum Annual Temperature, from Hindcast**

min temperature value = minimum annual temperature during reference
period (1960-1990)

From hindcast data, extract the minimum annual surface temperature for
each basin between 1960-1990.

``` r
## take max from hindcast dataset
## use moving average or annual means??
min_surf_temp <- surf_temp_current %>%
  filter(year %in% 1960:1990) %>%
  select(basin_name_holas, ma5_julaug_surf_temp) %>%
  group_by(basin_name_holas) %>%
  summarise(min_surf_temp = min(ma5_julaug_surf_temp, na.rm = TRUE)) %>%
  ungroup()
```

``` r
plot_surf_sal_refpts <- surf_sal_current %>% 
  left_join(max_surf_sal, by = "basin_name_holas") %>% 
  left_join(
    min_salinity %>% 
      tidyr::pivot_wider(names_from = model, names_prefix = "min_salinity_", values_from = min_salinity), 
    by = "basin_name_holas"
  )
ggplot(plot_surf_sal_refpts) +
  geom_line(aes(x = year, y = ma5_surf_sal)) +
  geom_hline(aes(yintercept = max_surf_sal), color = "goldenrod") +
  geom_hline(aes(yintercept = min_salinity_echam5), color = "royalblue") +
  geom_hline(aes(yintercept = min_salinity_hadcm3), color = "skyblue") +
  facet_wrap(~ basin_name_holas, scales = "free_y") +
  labs(x = NULL, y = "Surface Salinity (PSU) between Upper and Lower Reference Points\n")
```

![](climate_change_prep_files/figure-gfm/current%20surface%20salinity%20between%20min%20max%20reference%20points-1.png)<!-- -->

``` r
plot_surf_temp_refpts <- surf_temp_current %>% 
  left_join(min_surf_temp, by = "basin_name_holas") %>% 
  left_join(
    max_temp %>% 
      tidyr::pivot_wider(names_from = model, names_prefix = "max_temp_", values_from = max_temperature), 
    by = "basin_name_holas"
  )
ggplot(plot_surf_temp_refpts) +
  geom_line(aes(x = year, y = ma5_julaug_surf_temp)) +
  geom_hline(aes(yintercept = min_surf_temp), color = "goldenrod") +
  geom_hline(aes(yintercept = max_temp_echam5), color = "royalblue") +
  geom_hline(aes(yintercept = max_temp_hadcm3), color = "skyblue") +
  facet_wrap(~ basin_name_holas, scales = "free_y") +
  labs(x = NULL, y = "Jul-Aug Surface Temperature (deg C) between Upper and Lower Reference Points\n")
```

![](climate_change_prep_files/figure-gfm/current%20surface%20salinity%20between%20min%20max%20ref%20points-1.png)<!-- -->

### 3.4 Calculate Surface Salinity Pressure Scores

``` r
surf_sal_prs <- plot_surf_sal_refpts %>% 
  mutate(surf_sal_pressure = (ma5_surf_sal - max_surf_sal)/(min_salinity_hadcm3 - max_surf_sal)) %>% 
  mutate(surf_sal_pressure = pmin(1, pmax(0, round(surf_sal_pressure, 2)))) %>% 
  rename(subbasin = basin_name_holas)
```

### 3.5 Calculate Sea Surface Temperature Pressure Scores

Note: Greater pressure occurs with lower salinity, so will need to take
the inverse of the rescaling

``` r
surf_temps_prs <- plot_surf_temp_refpts %>% 
  mutate(surf_temp_pressure = (ma5_julaug_surf_temp - min_surf_temp)/(max_temp_hadcm3 - min_surf_temp)) %>% 
  mutate(surf_temp_pressure = pmin(1, pmax(0, round(surf_temp_pressure, 2)))) %>% 
  rename(subbasin = basin_name_holas)
```

``` r
rgns_complete <- read_csv(file.path(dir_prep, "supplement", "lookup_tabs", "rgns_complete.csv"))
## missing a few regions in a few years, add these...
d <- expand.grid(list(region_id = 1:42, year = 1990:2019), stringsAsFactors = FALSE)


write.csv(
  surf_sal_prs %>% 
    filter(year %in% 1990:2019) %>%
    left_join(rgns_complete, by = "subbasin") %>% 
    dplyr::left_join(d, surf_sal_prs, by = c("region_id", "year")) %>% 
    select(region_id, year, surf_sal_pressure), 
  
  file.path(dir_layers, "cc_sal_surf_bhi2019.csv"), 
  row.names = FALSE
)
write.csv(
  surf_temps_prs %>% 
    filter(year %in% 1990:2019) %>%
    left_join(rgns_complete, by = "subbasin") %>% 
    dplyr::left_join(d, surf_temps_prs, by = c("region_id", "year")) %>% 
    select(region_id, year, surf_temp_pressure), 
  
  file.path(dir_layers, "cc_sst_bhi2019.csv"), 
  row.names = FALSE
)
```

<br>

## 4\. Visualizing Data Layers

``` r
gridExtra::grid.arrange(
  ggplot(surf_sal_prs) +
    geom_col(aes(year, surf_sal_pressure, fill = surf_sal_pressure)) +
    facet_wrap(~subbasin) +
    scale_fill_viridis_c() +
    labs(x = NULL, y = "Surface Salinity Pressure", fill = "Surface Salinity Pressure"),
  ggplot(surf_temps_prs) +
    geom_col(aes(year, surf_temp_pressure, fill = surf_temp_pressure)) +
    facet_wrap(~subbasin) +
    scale_fill_viridis_c() +
    labs(x = NULL, y = "Surface Temperature Pressure", fill = "Surface Temperature Pressure"),
  nrow = 2
)
```

![](climate_change_prep_files/figure-gfm/plot%20surface%20salinity%20and%20temperature%20pressure%20scores%20timeseries-1.png)<!-- -->

<br>

## 5\. Considerations for `BHI3.0`

Need modeled data for full spatial coverage or check somehow that data
are fully representative for subbasins…?
