Invasive Species Pressure Layer Data Preparation
================

<br>

``` r
## root location of the raw data
source(here::here("R", "setup.R"))
source(here::here("R", "spatial.R"))
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE, results = "hide", fig.width = 9.5, fig.height = 6)
source(here::here("R", "setup.R"))
loc <- here::here("prep", "pressures", "invasive_spp")
data_path <- here::here("data", "pressures", "invasive_spp", version_year, "invasive_spp_data.rmd")

dir_A <- file.path(dirname(dir_prep), "bhi-data")
dir_B <- file.path(dirname(dir_prep), "bhi-data", "BHI 2.0")
dir_rawdata <- file.path(dir_B, "Pressure", "invasive_spp")
```

<br>

## 1\. Background

Due to increasing shipping, more alien species are finding their way
into the Baltic Sea than ever before. These invasive species can induce
considerable changes in the structure and dynamics of marine ecosystems.
Among the goals affected by this pressure, the Iconic Species (ICO)
sub-goal and Biodiversity (BD) goal are particularly affected.

## 2\. Data

This prep document is used to generate and explore the following data
layer:

  - `sp_invasives_bhi2019.csv`

This is saved to the `layers` folder. These are derived from or informed
by the raw data obtained from Aquanis database.

<br>

### 2.1 Datasets with Sources

<br/>

Data: number of invasives species introduction events per country
<!-- dataset save location BHI_share/BHI 2.0/Pressure/invasive_spp/ -->

| Option           | Specification    |
| :--------------- | :--------------- |
| Search criteria: | Recipient region |
| Ocean:           | Atlantic         |
| Ocean region:    | NE Atlantic      |
| LME:             | 23\. Baltic Sea  |
| LME sub region:  | Baltic Sea       |
| Country:         | Select           |

Source:
[Aquanis](http://www.corpi.ku.lt/databases/index.php/aquanis/search/search_advanced)
<br/>

<br/>

## 3\. Prep: Wrangling & Derivations, Checks/Evaluation, Gapfilling

``` r
country_score <- read_delim(file.path(dir_rawdata, "invasive_spp.csv"), delim = ";") %>% 
  mutate(ref_pt = 10, 
         score = min(1, num_invasive/ref_pt))

bhi_basin_lookup <- read_delim(file.path(dir_prep, "supplement", "lookup_tabs", "bhi_basin_country_lookup.csv"), delim = ";") %>% 
  rename(country = rgn_nam) 
```

<br>

## 4 Calculate Invasive Species Pressure Scores

**Score** = \#\_of\_species\_introduction\_event / reference\_point

**Reference point** = 10 introduction events (with no additional
information, this is an arbitrary number set for now, as for BHI
1.0)

``` r
bhi_score <- full_join(country_score, bhi_basin_lookup, by = 'country') %>% 
  dplyr::select(rgn_id = BHI_ID, 
         pressure_score = score)

# Save pressure layer
#write_csv(bhi_score, file.path(dir_layers, "sp_invasives_bhi2019.csv"))
```

<br>
