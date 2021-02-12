Nutrients Loads Pressure Layer Data Preparation
================

<br>

``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE, results = "hide", fig.width = 9.5, fig.height = 6)
source(here::here("R", "setup.R"))
loc <- here::here("prep", "pressures", "nutrient_load")
data_path <- here::here("data", "pressures", "nutrient_load", version_year, "nutrient_load_data.rmd")
```

<br>

## 1\. Background

External loads of nitrogen and phosphorus into the Baltic Sea. Loads
include land-based and atmospheric sources and are normalized for river
flow. HELCOM Core Indicator - Inputs of Nitrogen and Phosphorus to the
Baltic Sea. [6th Pollution Load Compilation, Executive
Summary](https://helcom.fi/media/publications/PLC-6-Executive-Summary.pdf)

### 1.1 Data Scale

Assessment of N & P loads and the setting of MAI were done for 7 Baltic
Sea basins: Baltic Proper, Kattegat, Danish Straits, Gulf of Riga, Gulf
of Finland, Bothnian Sea, Bothnian Bay. These basins are larger than the
HOLAS basins used define the BHI regions. The pressure score will be
calculated at the assessment scale and then all BHI regions within an
assessment basin will receive the same pressure value.

### 1.2 Reference points

In discussions with Bo Gustafsson, we have made the following data
rescaling decisions:

**Current pressure** = the mean load in each basin during the most
recent three year assessment period for HELCOM

**Min value** = the minimum value is when there is no pressure on the
system, this is scaled to the MAI for each basin

**Max value** = the maximum value is the highest load to each basin over
the reference period used for determining MAI (1997-2003)

<br/>

## 2\. Data

This prep document is used to generate and explore the following data
layers:

  - `po_nload_bhi2019.csv`
  - `po_pload_bhi2019.csv`

These are saved to the `layers` folder. All these are derived from or
informed by the raw datasets from HELCOM Pollution Load Compilation
(PLC) reports.

<br>

### 2.1 Datasets with Sources

<br/>

Data used are official HELCOM reported data.

#### 2.1.1 Nitrogen & Phosphorus (N & P) Loads

<!-- dataset save location BHI_share/BHI 2.0/Pressure/nutrient_load -->

Data tables on total annual input of nitrogren and phosphorous (tonnes)
as well as water flow into the Baltic Sea can be found
[here](https://helcom.fi/wp-content/uploads/2020/06/BSEFS-Waterborne-nitrogen-and-phosphorus-inputs-and-water-flow-to-the-Baltic-Sea.pdf).
Data on annual deposition of total nitrogen can be found
[here](https://emep.int/publ/helcom/2019/B_BSEFS_N_dep_v3.pdf).

Actual total (e.g. air and waterborne, direct and point source
discharges) annual input of nitrogren and phosphorous (tonnes) are
accessed as part of the HELCOM Core Indicators, in the Pollution Load
Compilation (PLC); see online resource about the most recent compilation
[PLC-6 here](https://helcom.fi/helcom-at-work/projects/plc-6/).

#### 2.1.2 Maximum Allowable Inputs (MAI)

<!-- dataset save location BHI_share/BHI 2.0/Pressure/nutrient_load -->

MAI values for each basin are found in [Summary report on the
development of revised Maximum Allowable Inputs and updated Country
Allocated Reduction Targets of the Baltic Sea Action
Plan](http://www.helcom.fi/Documents/Ministerial2013/Associated%20documents/Supporting/Summary%20report%20on%20MAI-CART.pdf)
in Table 3 ‘Maximum Allowable nutrient Inputs to main Baltic Sea
sub-basins’, on page 8.

These are the same values as presented in Table 2.2 page 11 in the
[Review of the Fifth Baltic Sea PLC for the 2013 HELCOM Ministerial
Meeting](https://helcom.fi/media/publications/BSEP141.pdf)

<br/>

### 2.2 Centralization & Normalization

``` r
## root location of the pdfs containing the data
dir_rawdata <- file.path(dir_B, "Pressure", "nutrient_load")

library(pdftools)
library(stringr)
```

#### 2.2.1 Extract Data from PDFs

``` r
pdf_file_mai <- file.path(dir_B, "Pressure", "nutrient_load", "Summary-report-on-MAI-CART.pdf")
txt <- pdf_text(pdf_file_mai)

mai_txt <- str_extract(txt[9], regex("(?<=Maximum Allowable Inputs\n  ).*(?=4 HOW ARE THE UPDATED CART WAS DETERMINED)", dotall = TRUE))

mai_tab <- as.data.frame(str_split(mai_txt, "\n")[[1]][2:9])
colnames(mai_tab) <- "alldata"

newcolnames <- str_split(str_split(mai_txt, "\n")[[1]][1], "(?<=[a-z])  +")[[1]] %>% 
  str_replace_all(" ", "_") %>% 
  str_remove_all(",|-") %>% 
  str_to_lower()

mai_tab <- tidyr::separate(mai_tab, col = "alldata", into = newcolnames, sep = "(?<=[a-z0-9])  +") %>% 
  rename(baltic_sea_basin = baltic_sea_subbasin) %>% 
  mutate(
    baltic_sea_basin = str_extract(baltic_sea_basin, "[A-Z].*"),
    total_nitrogen_tonnes = str_remove(total_nitrogen_tonnes, ","),
    total_phosphorus_tonnes = str_remove(total_phosphorus_tonnes, ",")
  ) %>% 
   mutate(
    total_nitrogen_tonnes = as.numeric(total_nitrogen_tonnes),
    total_phosphorus_tonnes = as.numeric(total_phosphorus_tonnes)
  ) %>% 
  filter(baltic_sea_basin != "Baltic Sea") 
```

``` r
pdf_file_inflows <- file.path(dir_B, "Pressure", "nutrient_load", "Waterborne-N-and-P-inputs-and-water-flow.pdf")
txt <- pdf_text(pdf_file_inflows)


## extract total annual waterborne flow 
## sum of riverine flow and direct flow (flow for point sources discharging direct into the Baltic Sea)

flows_txt <- str_extract(txt[15], regex("(?<=Flow \\(m3/s-1\\)\n  ).*(?=\n)", dotall = TRUE))
flows_tab <- as.data.frame(str_split(flows_txt, "\n")[[1]][2:24])
colnames(flows_tab) <- "alldata"

newcolnames <- str_split(str_split(flows_txt, "\n")[[1]][1], "(?<=[a-zA-Z])  +")[[1]] %>% 
  str_remove_all(" ")

flows_tab <- tidyr::separate(flows_tab, col = "alldata", into = newcolnames, sep = "(?<=[a-z0-9])  +") %>% 
  mutate_all(as.numeric) %>% 
  rename(year = Sum)


## extract total waterborne (riverine + direct) nitrogen inputs

n_txt <- str_extract(txt[18], regex("(?<= TN \\(tonnes\\)\n  ).*(?=\n)", dotall = TRUE))
n_tab <- as.data.frame(str_split(n_txt, "\n")[[1]][2:24])
colnames(n_tab) <- "alldata"

n_tab <- tidyr::separate(n_tab, col = "alldata", into = newcolnames, sep = "(?<=[a-z0-9])  +") %>% 
  mutate_all(as.numeric) %>% 
  rename(year = Sum)


## extract total waterborne (riverine + direct) phosphorus inputs

p_txt <- str_extract(txt[21], regex("(?<= TP \\(tonnes\\)\n  ).*(?=\n)", dotall = TRUE))
p_tab <- as.data.frame(str_split(p_txt, "\n")[[1]][2:24])
colnames(p_tab) <- "alldata"

p_tab <- tidyr::separate(p_tab, col = "alldata", into = newcolnames, sep = "(?<=[a-z0-9])  +") %>% 
  mutate_all(as.numeric) %>% 
  rename(year = Sum)
```

``` r
# pdf_atmos_n <- file.path(dir_B, "Pressure", "nutrient_load", "Atmospheric-deposition-N.pdf")
pdf_atmos_n <- file.path(dir_B, "Pressure", "nutrient_load", "Atmospheric-deposition-N.pdf")
txt <- pdf_text(pdf_atmos_n)


## extract total annual deposition of total nitrogen to the subbasins 
## units are ktonnes(N)/year

atmos_n_txt <- str_extract(txt[12], regex("(?<=Baltic Sea \\(sum of all sub-basins\\)\\.\n  ).*(?=\n)", dotall = TRUE))
atmos_n_tab <- as.data.frame(str_split(atmos_n_txt, "\n")[[1]][4:26])
colnames(atmos_n_tab) <- "alldata"

newcolnames <- str_split(str_split(atmos_n_txt, "\n")[[1]][3], "(?<=[a-zA-Z])  +")[[1]] %>% 
  str_remove_all(" ")
newcolnames <- c("year", newcolnames, "BAS")

atmos_n_tab <- tidyr::separate(atmos_n_tab, col = "alldata", into = newcolnames, sep = "(?<=[a-z0-9])  +") %>% 
  mutate_all(as.numeric)
```

#### 2.2.2 Standardize Units

``` r
## not sure how to normalize...

n_normalized <- left_join(
  n_tab %>% 
    tidyr::pivot_longer(
      cols = matches("^[A-Z]{2,3}$"),
      names_to = "basin_code", 
      values_to = "input_tonnes"
    ),
  flows_tab %>% 
    tidyr::pivot_longer(
      cols = matches("^[A-Z]{2,3}$"),
      names_to = "basin_code", 
      values_to = "flow_cubic_meters_per_second"
    ),
  by = c("year", "basin_code")
)
n_normalized <- n_normalized %>% 
  mutate(input_per_flow = input_tonnes/flow_cubic_meters_per_second) %>% 
  mutate(basin_code = as.character(basin_code))
  
p_normalized <- left_join(
  p_tab %>% 
    tidyr::pivot_longer(
      cols = matches("^[A-Z]{2,3}$"),
      names_to = "basin_code", 
      values_to = "input_tonnes"
    ),
  flows_tab %>% 
    tidyr::pivot_longer(
      cols = matches("^[A-Z]{2,3}$"),
      names_to = "basin_code", 
      values_to = "flow_cubic_meters_per_second"
    ),
  by = c("year", "basin_code")
)
p_normalized <- p_normalized %>% 
  mutate(input_per_flow = input_tonnes/flow_cubic_meters_per_second) %>% 
  mutate(basin_code = as.character(basin_code))
```

``` r
## codes from figure 1 page 5 of pdf containing the input and flow data                                               
basinsdf <- tibble(
  baltic_sea_basin = c(
    "Kattegat", "Danish Straits", "Baltic Proper", "Bothnian Sea", 
    "Bothnian Bay", "Gulf of Riga", "Gulf of Finland"
  ),
  basin_code = c(
    "KAT", "DS", "BAP", "BOS",
    "BOB", "GUR", "GUF"
  )
)
n_normalized <- left_join(
  filter(n_normalized, basin_code != "BAS"), 
  basinsdf, 
  by = "basin_code"
)
p_normalized <- left_join(
  filter(p_normalized, basin_code != "BAS"), 
  basinsdf, 
  by = "basin_code"
)


## wrangle atmospheric N deposition to match basin codes with subbasins
## codes from figure 4 page 8 of pdf containing the atmospheric nitrogen data   
basinsdf <- tibble(
  baltic_sea_subbasin = c(
    "Archipelago Sea", "Baltic Proper", "Bothnian Bay", "Bothnian Sea", 
    "Gulf of Finland", "Gulf of Riga", "Kattegat", "The Sound", "Western Baltic"
  ),
  basin_code = c(
    "ARC", "BAP", "BOB", "BOS",
    "GUF", "GUR", "KAT", "SOU", "WEB"
  ),
  baltic_sea_basin = c(
    "Bothnian Sea", "Baltic Proper", "Bothnian Bay", "Bothnian Sea", 
    "Gulf of Finland", "Gulf of Riga", "Kattegat", "Danish Straits", "Danish Straits"
  )
)

## but need to aggregate to match the MAI and waterborne inputs spatial units...
atmos_n_tab <- atmos_n_tab %>% 
  tidyr::pivot_longer(
    cols = matches("^[A-Z]{3}$"),
    names_to = "basin_code", 
    values_to = "input"
  ) %>% 
  mutate(input_tonnes = input*1000) %>% 
  filter(basin_code != "BAS") %>% 
  left_join(basinsdf, by = "basin_code") %>% 
  group_by(year, baltic_sea_basin) %>% 
  summarise(input_tonnes = sum(input_tonnes))
```

#### 2.2.3 Save to BHI Database

``` r
dir_intermediate <- file.path(dir_prep, "data", "pressures", "nutrient_load", version_year, "intermediate")
write_csv(
  n_normalized,
  file.path(dir_intermediate, "basin_n_load.csv")
)
write_csv(
  atmos_n_tab,
  file.path(dir_intermediate, "basin_n_atmos_load.csv")
)
write_csv(
  p_normalized,
  file.path(dir_intermediate, "basin_p_load.csv")
)
write_csv(
  mai_tab,
  file.path(dir_intermediate, "n_p_load_targets.csv")
)
```

<br/>

<br/>

## 3\. Prep: Wrangling & Derivations, Checks/Evaluation, Gapfilling

``` r
dir_intermediate <- file.path(dir_prep, "data", "pressures", "nutrient_load", version_year, "intermediate")

## read in N and P data
n_load <- read.csv(file.path(dir_intermediate, "basin_n_load.csv"))
n_atmos_load <- read.csv(file.path(dir_intermediate, "basin_n_atmos_load.csv"))
p_load <- read.csv(file.path(dir_intermediate, "basin_p_load.csv"))

## read in maximum allowable inputs
mai <- read.csv(file.path(dir_intermediate, "n_p_load_targets.csv"))
```

``` r
n_load <- left_join(
  rename(n_load, n_waterborne = input_tonnes), 
  select(mai, baltic_sea_basin, maximum_allowable_input = total_nitrogen_tonnes),
  by = "baltic_sea_basin"
)
p_load <- left_join(
  p_load,
  select(mai, baltic_sea_basin, maximum_allowable_input = total_phosphorus_tonnes),
  by = "baltic_sea_basin"
)
```

``` r
n_load <- n_atmos_load %>% 
  rename(n_atmos = input_tonnes) %>% 
  left_join(n_load, by = c("year", "baltic_sea_basin")) %>% 
  mutate(input_tonnes = n_atmos + n_waterborne)
```

<br>

### 3.1 Calculate Load Pressure Scores

**Calculate average loads of Subbasins**

``` r
n_current <- n_load %>% 
  
  ## three year assessment period
  ## take rolling means with three year window
  arrange(basin_code, year) %>% 
  group_by(basin_code) %>% 
  mutate(n_current = zoo::rollapply(input_tonnes, 3, mean, na.rm = TRUE, align = "right", fill = NA)) %>% 
  ungroup() %>% 
  filter(!is.na(n_current)) %>% 
  mutate(n_current = round(n_current))
```

``` r
p_current <- p_load %>% 
  
  ## three year assessment period
  ## take rolling means with three year window
  arrange(basin_code, year) %>% 
  group_by(basin_code) %>% 
  mutate(p_current = zoo::rollapply(input_tonnes, 3, mean, na.rm = TRUE, align = "right", fill = NA)) %>% 
  ungroup() %>% 
  filter(!is.na(p_current)) %>% 
  mutate(p_current = round(p_current))
```

**Rescale Load pressures**

Data layer needs to be rescaled between 0 (min pressure) and 1 (max
pressure).

``` r
n_min <- n_load %>% 
  distinct(baltic_sea_basin, maximum_allowable_input) %>%
  dplyr::rename(n_min = maximum_allowable_input)

n_max <- n_load %>% 
  filter(year %in% 1997:2003) %>%
  select(year, baltic_sea_basin, input_tonnes) %>%
  group_by(baltic_sea_basin) %>%
  summarise(n_max = max(input_tonnes)) %>%
  ungroup()
        
## to normalize data: normalized = (x-min(x))/(max(x)-min(x))
## high pressure score corresponds to high pressure, i.e. is a bad thing
## pressure of zero indicates the target is achieved
n_current_score <- n_current %>%
  full_join(n_min, by = "baltic_sea_basin") %>%
  full_join(n_max, by = "baltic_sea_basin") %>%
  mutate(n_rescaled = (n_current - n_min)/abs(n_max - n_min)) %>%
  mutate(n_score = pmin(1, pmax(0, n_rescaled))) %>%
  mutate(n_score = round(n_score, 2)) %>% 
  select(year, baltic_sea_basin, n_score, n_current, n_min, n_max)
```

``` r
p_min <- p_load %>% 
  distinct(baltic_sea_basin, maximum_allowable_input) %>%
  dplyr::rename(p_min = maximum_allowable_input)

p_max <- p_load %>% 
  filter(year %in% 1997:2003) %>%
  select(year, baltic_sea_basin, input_tonnes) %>%
  group_by(baltic_sea_basin) %>%
  summarise(p_max = max(input_tonnes)) %>%
  ungroup()
        
## to normalize data: normalized = (x-min(x))/(max(x)-min(x))
p_current_score <- p_current %>%
  full_join(p_min, by = "baltic_sea_basin") %>%
  full_join(p_max, by = "baltic_sea_basin") %>%
  mutate(p_rescaled = (p_current - p_min)/abs(p_max - p_min)) %>%
  mutate(p_score = pmin(1, pmax(0, p_rescaled))) %>%
  mutate(p_score = round(p_score, 2)) %>% 
  select(year, baltic_sea_basin, p_score, p_current, p_min, p_max)
```

**Match nutrient loading values by Subbasin to BHI
regions**

``` r
bhi_basin_match <- read_csv(file.path(dir_prep, "supplement", "lookup_tabs", "rgns_complete.csv")) %>% 
  select(region_id, subbasin) %>% 
  left_join(
    tibble(
      subbasin = c(
        "Great Belt", "Kiel Bay", "Bay of Mecklenburg", 
        "The Sound", "Kattegat", 
        "Arkona Basin", "Bornholm Basin", "Gdansk Basin", "Northern Baltic Proper", 
        "Eastern Gotland Basin", "Western Gotland Basin", 
        "Gulf of Riga", 
        "Gulf of Finland", 
        "Aland Sea", "Bothnian Sea", 
        "The Quark", "Bothnian Bay"
      ),
      baltic_sea_basin = c(
        "Danish Straits", "Danish Straits", "Danish Straits", 
        "Kattegat", "Kattegat",
        "Baltic Proper", "Baltic Proper", "Baltic Proper", "Baltic Proper", 
        "Baltic Proper", "Baltic Proper", 
        "Gulf of Riga", 
        "Gulf of Finland", 
        "Bothnian Sea", "Bothnian Sea", 
        "Bothnian Bay", "Bothnian Bay"
      )
    )
  )

po_nload <- full_join(bhi_basin_match, n_current_score, by = "baltic_sea_basin")
po_pload <- full_join(bhi_basin_match, p_current_score, by = "baltic_sea_basin")
```

<br/>

### 3.2 Save the Nutrient Load Pressure Layers

Save nutrient loading pressure data as csv in layers folder.

``` r
write_csv(
  po_nload %>% 
    select(region_id, year, pressure_score = n_score), 
  file.path(dir_layers, "po_nload_bhi2019.csv")
)
write_csv(
  po_pload %>% 
    select(region_id, year, pressure_score = p_score), 
  file.path(dir_layers, "po_pload_bhi2019.csv")
)
```

<br>

## 4\. Visualizing Data Layers

### 4.1 Nutrient Load Pressure Scores by Subbasin

``` r
ggplot(distinct(po_nload, year, subbasin, n_score)) +
  geom_col(aes(x = year, y = n_score)) +
  facet_wrap(~subbasin, scales = "free_y", nrow = 6) +
  labs(x = NULL, y = "Nitrogen Load, Pressure Score")
```

![](nutrient_load_prep_files/figure-gfm/nutrient%20load%20pressure%20scores-1.png)<!-- -->

``` r
ggplot(distinct(po_pload, year, subbasin, p_score)) +
  geom_col(aes(x = year, y = p_score)) +
  facet_wrap(~subbasin, scales = "free_y", nrow = 6) +
  labs(x = NULL, y = "Phosphorus Load, Pressure Score")
```

![](nutrient_load_prep_files/figure-gfm/nutrient%20load%20pressure%20scores-2.png)<!-- -->

### 4.2 Timeseries

``` r
gridExtra::grid.arrange(
  ggplot(n_current) +
    geom_vline(xintercept = 1997, color = "snow", size = 2) +
    geom_vline(xintercept = 2003, color = "snow", size = 2) +
    geom_hline(aes(yintercept = maximum_allowable_input), color = "maroon") +
    geom_line(aes(year, n_current), size = 0.5) +
    geom_point(aes(year, input_tonnes)) +
    labs(
      x = NULL, y = "Input (tonnes)\n", 
      title = "Nitrogen Inputs (atmospheric and waterborne), line shows 3 year rolling mean"
    ) +
    facet_wrap(~baltic_sea_basin, scales = "free_y", nrow = 1),
  ggplot(p_current) +
    geom_vline(xintercept = 1997, color = "snow", size = 2) +
    geom_vline(xintercept = 2003, color = "snow", size = 2) +
    geom_hline(aes(yintercept = maximum_allowable_input), color = "maroon") +
    geom_line(aes(year, p_current), size = 0.5) +
    geom_point(aes(year, input_tonnes)) +
    labs(
      x = NULL, y = "Input (tonnes)\n", 
      title = "Phosphorus Inputs (only waterborne), line shows 3 year rolling mean"
    ) +
    facet_wrap(~baltic_sea_basin, scales = "free_y", nrow = 1),
  nrow = 2
)
```

![](nutrient_load_prep_files/figure-gfm/timeseries%20plots%20of%20n%20and%20p%20loads-1.png)<!-- -->

### 4.3 Map of N & P pressure scores by BHI region

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
      po_nload %>%
        select(region_id, year, n_score) %>% 
        filter(year %in% seq(1997, 2017, by = 2)),
      # %>% 
      #   tidyr::pivot_wider(names_from = "year", names_prefix = "yr", values_from = "n_score"),
      by = "region_id"
    ),
    mapping = aes(fill = n_score),
    alpha = 0.8
  ) +
  scale_fill_gradientn(
    colors = rev(cols), 
    values = c(0, 10^(-7), 0.3, 0.65, 0.8, 1), 
    limits = c(0, 1)
  ) +
  facet_wrap(~year)
```

![](nutrient_load_prep_files/figure-gfm/nitrogen%20nutrient%20load%20pressures%20map-1.png)<!-- -->

``` r
basemap +
  geom_sf(
    data = left_join(
      bhi_rgn_simple,
      po_pload %>%
        select(region_id, year, p_score) %>% 
        filter(year %in% seq(1997, 2017, by = 2)),
      # %>% 
      #   tidyr::pivot_wider(names_from = "year", names_prefix = "yr", values_from = "n_score"),
      by = "region_id"
    ),
    mapping = aes(fill = p_score),
    alpha = 0.8
  ) +
  scale_fill_gradientn(
    colors = rev(cols), 
    values = c(0, 10^(-7), 0.3, 0.65, 0.8, 1), 
    limits = c(0, 1)
  ) +
  facet_wrap(~year)
```

![](nutrient_load_prep_files/figure-gfm/phosphorus%20nutrient%20load%20pressures%20map-1.png)<!-- -->

<br>

## 5\. Considerations for `BHI3.0`
