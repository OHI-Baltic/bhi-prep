Illegal Oil Pressure Layer Data Preparation
================

<br>

``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE, results = "hide", fig.width = 9.5, fig.height = 6)
source(here::here("R", "setup.R"))
loc <- here::here("prep", "pressures", "illegal_oil")
data_path <- here::here("data", "pressures", "illegal_oil", version_year, "illegal_oil_data.rmd")
```

<br>

## 1\. Background

As the Baltic Sea in classified as a very sensitive marine area,
discharging oil into the Baltic can have a huge impact. For the BHI,
some of the goals have been considered as negatively impacted by this
‘pressure’, in particular the Contaminants (CON) sub-goal, the
Fisheries (FIS) and Mariculture (MAR) sub-goals, the Iconic Species
(ICO) and Lasting Special Places (LSP) sub-goals, and the Natural
Products goal.

## 2\. Data

This prep document is used to generate and explore the following data
layer:

  - `hab_illegal_oil_bhi2019.csv`

These are saved to the `layers` folder. These are derived from or
informed by the raw Oceanographic datasets obtained from HELCOM.

<br>

### 2.1 Datasets with Sources

This dataset contains points of information describing the location and
size of illegal oil discharges observed during aerial surveillance
flights by HELCOM Contracting Parties during 1998-2017. Further
information about illegal discharges of oil in the Baltic Sea area and
HELCOM aerial surveillance activities can be found at
<http://www.helcom.fi/baltic-sea-trends/maritime/illegal-spills/>

<!-- dataset save location BHI_share/BHI 2.0/Pressure/illegal_oil -->

| Option    | Specification          |
| :-------- | :--------------------- |
| Shipping: | Illegal oil discharges |
| <br/>     |                        |

Source: [HELCOM Map and Data
Service](https://maps.helcom.fi/website/mapservice/) <br/> Downloaded 21
August 2020 by Andrea De Cervo

<br/>

## 3\. Data Layer preparation

### 3.1 Pressure model

Assign all spill locations a BHI ID. Sum the total volume of illegal oil
spilled in each BHI region in each year.

Calculate the volume spilled per region surface area

Note- Exclude reported spills with volume

Provide summary of total oil spills reported per year per BHI region,
number reported with volumes per year per BHI region

<br/>

**Current conditions** Mean volume spilled in each BHI region 2009-2017
/ region surface area

**Rescaling 0 to 1**

  - min value = 0
  - max value = max annual volume spilled / per surface area *spatial
    reference*

### 3.2 Prep: Wrangling & Derivations, Checks/Evaluation, Gapfilling

Steps:

1.  Get a BHI region assignment for all oil spill reports - do this by
    overlaying lat, lon locations with BHI shapefile

2.  Get volume spilled by year by BHI region (also visualize number of
    spills per year per BHI region, number of spills with zero volumne
    reported)

3.  Get current conditions, find max value, rescale data to between 0
    and 1

4.  Export and register data layer

**Data prep setup**

``` r
## Libraries

library(tidyverse) # install.packages('tidyverse')
library(RMySQL)
library(tools)
library(rprojroot) # install.packages('rprojroot')
library(rgdal)  # install.packages('rgdal')
library(raster) # install.packages('raster')
library(rgeos)  # install.packages('rgeos')

## root location of the raw data
source(here::here("R", "setup.R"))
source(here::here("R", "data.R"))
source(here::here("R", "spatial.R"))

dir_A <- file.path(dirname(dir_prep), "bhi-data")
dir_B <- file.path(dirname(dir_prep), "bhi-data", "BHI 2.0")
dir_rawdata <- file.path(dir_B, "Pressure", "illegal_oil")
```

<br/>

**Read in data and explore**

``` r
## read in BHI shape file
bhi <- rgdal::readOGR(dsn = path.expand(file.path(dir_A, "Shapefiles", "BHI_shapefile")),
                      layer = 'BHI_shapefile') 
# bhi@proj4string  +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 

## read in Illegal Oil Discharge file
oil <- rgdal::readOGR(dsn = path.expand(file.path(dir_rawdata, 'IllegalOilDischarges')),
                     layer = 'IllegalOilDischarges')  

oil_data <- oil@data

# write_csv(oil_data, file.path(dir_oil, "oil_data_raw.csv"))

# oil@proj4string
#  +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs 

# years covered: 
# unique(oil_data$Year)
# 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017

bhi_transf <- spTransform(bhi, oil@proj4string)

## plot data (takes time)
plot(oil); plot(bhi_transf, border = "grey", main = "Illegal Oil Discharges and BHI regions overlay", add = TRUE); legend('bottom', c("BHI regions", " Illegal Oil Discharges"), lty = c(1,1), lwd = c(2.5, 2.5, 2.5), col = c("grey", "black"), text.font = 1, box.lty = 0 )
```

![](illegal_oil_prep_files/figure-gfm/read%20in%20data-1.png)<!-- -->

``` r
## intersect with BHI regions
oil_bhi_intersect <- raster::intersect(oil, bhi_transf)
```

<br/>

## 4 Calculate pressure scores

``` r
## read in oil data, calculate average vol/area
data <- oil_bhi_intersect@data[,-1] %>% #get rid of duplicated column, otherwise select doesn't work...
  dplyr::select(BHI_ID,
                year = Year, 
                volume = EstimVol_m, # m3
                area_km2 = Area_km2) %>% 
  filter(year %in% 2009:2017) %>% 
  group_by(BHI_ID, year, area_km2) %>% 
  summarize(total_vol = sum(volume*(10^(-9)))) %>% # total annual vol of each year in km3
  ungroup() %>% 
  group_by(BHI_ID, area_km2) %>% 
  summarize(ave_vol = mean(total_vol)) %>% # average vol 2009-2014
  ungroup() %>% 
  mutate(score = ave_vol/area_km2, 
         min_score = min(score), 
         max_score = max(score))   # ave_vol / area

# calculate pressure scores 
oil_pressure <- data %>% 
  mutate(pressure_score = round((score-min_score)/(max_score - min_score), 2)) %>% 
  dplyr::select(rgn_id = BHI_ID, 
                pressure_score) %>% 
  complete(rgn_id = full_seq(rgn_id, 1))

# save in layers folder
#write_csv(oil_pressure, file.path(dir_layers, 'hab_illegal_oil_bhi2019.csv'))
```

<br/>
