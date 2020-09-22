Food Provision (FP) - Mariculture (MAR) Subgoal Data Preparation
================

``` r
loc <- here::here("prep", "MAR")

source(here::here("R", "setup.R"))
source(here::here("R", "spatial.R"))
regions_shape(sp_dir = file.path(dirname(dir_B), "Shapefiles"))
knitr::opts_chunk$set(message = FALSE, warning = FALSE, results = "hide", fig.width = 9.5)

bkgd_path <- here::here("supplement", "goal_summaries", "MAR.Rmd")
data_path <- here::here("data", "MAR", version_year, "mar_data.rmd")
refs_path <- file.path(loc, "mar_references.Rmd")
```

<br>

## 1\. Background

### 1.1 Goal Description

The Mariculture sub-goal describes a country’s ability to maximize the
sustainable yield of farmed fish in the ocean for human consumption.

### 1.2 Model & Data

Sustainable mariculture represents a good supplementary opportunity that
can support food provisioning needs, especially when considering not
compromising the water quality in the farmed area and not relying on
wild populations to feed or replenish the cultivated species. However,
assessing the sustainable production of farmed fish can be difficult, as
information is limited (location of the fish farms, species produced,
nutrient and antibiotic release from these farms). Overall, in terms of
total fish production, mariculture is at the moment not a large industry
in the Baltic Sea, and dominated by rainbow trout production, which was
included using available national data.

**Despite the production of rainbow trouts in some Baltic countries
(Denmark, Germany, Sweden and Finland), there is currently very limited
data on nutrient discharge, and therefore the Mariculture sub-goal was
not assessed and will not contribute to the overall Food Provision goal
score.**

### 1.3 Reference points

The maximum nutrient discharge for both phosphorus (P) and nitrogen (N)
could be used as target for this sub-goal, if information is available.
The [HELCOM
Recommendation](http://www.helcom.fi/Recommendations/Rec%2025-4.pdf),
provides a management target that existing and new marine fish farms
should not exceed the annual average of 7g of P (total-P) and 50g of N
(total-N) per 1kg fish (living weight) produced.

### 1.4 Other information

Data for `BHI1.0` were compiled primarily by [Ginnette Flores
Carmenate](https://bioenv.gu.se/digitalAssets/1575/1575339_flores-carmenate.pdf)
as part of a Masters thesis. Data sources and aggregation approaches
were slightly revised for `BHI2.0`. See data prep methods for more
details.

<br/>

## 2\. Data

**Rainbow trout production (Denmark, Sweden and Finland) and Finfish
(Denmark, Germany, Sweden and
Finland)**

<br>

### 2.1 Datasets with Sources

<br/>

#### 2.1.1 Rainbow trout mariculture Sweden

<!-- dataset save location BHI_share/2.0/Goals/FP/Mariculture/rbt_se/rbt_se.csv -->

<table class="table" style="">

<caption>

Source: [Jorbruksverket
database](https://www.scb.se/hitta-statistik/statistik-efter-amne/jord-och-skogsbruk-fiske/vattenbruk/vattenbruk/)
<br> from
[document](https://www.scb.se/contentassets/cef2fb103630496bb532e76c98f747e6/jo1201_2018a01_sm_jo60sm1901.pdf)
Downloaded 2019-10-09 by Andrea De Cervo

</caption>

<thead>

<tr>

<th style="text-align:left;">

Option

</th>

<th style="text-align:left;">

Specification

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

PDF:

</td>

<td style="text-align:left;">

Vattenbruk 2018

</td>

</tr>

<tr>

<td style="text-align:left;">

Location data:

</td>

<td style="text-align:left;">

Page 7, table
A

</td>

</tr>

</tbody>

</table>

<br/>

#### 2.1.2 Rainbow trout mariculture Denmark

<!-- dataset save location BHI_share/2.0/Goals/FP/Mariculture/rbt_dk/rbt_dk.csv -->

<!-- dataset save location BHI_share/2.0/Goals/FP/Mariculture/rbt_dk/rbt_dk_2005.csv -->

<table class="table" style="">

<caption>

Source: [Ministry of Environment and Food of Denmark
database](https://fiskeristyrelsen.dk/fiskeristatistik/akvakulturstatistik/)
<br> Downloaded 2019-10-11 by Andrea De Cervo

</caption>

<thead>

<tr>

<th style="text-align:left;">

Option

</th>

<th style="text-align:left;">

Specification

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Faste tabeller med akvakulturststistik:

</td>

<td style="text-align:left;">

Produktion af hovedarter fordelt på region

</td>

</tr>

<tr>

<td style="text-align:left;">

Rows:

</td>

<td style="text-align:left;">

Regnbueørred, Saltvand

</td>

</tr>

</tbody>

</table>

<br/>

#### 2.1.3 Mariculture in Finland

**Mariculture by Species (Rainbow Trout)**
<!-- dataset save location BHI_share/2.0/Goals/FP/Mariculture/rbt_fi/rbt_fi.csv -->

<table class="table" style="">

<caption>

Source: [Natural Resources Institute
database](http://statdb.luke.fi/PXWeb/pxweb/en/LUKE/LUKE__06%20Kala%20ja%20riista__02%20Rakenne%20ja%20tuotanto__10%20Vesiviljely/?tablelist=true&rxid=ef8a2896-d6cb-4660-a8aa-fee93bd4212f)
<br> Downloaded 2019-10-10 by Andrea De Cervo

</caption>

<thead>

<tr>

<th style="text-align:left;">

Option

</th>

<th style="text-align:left;">

Specification

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Luke statistics database:

</td>

<td style="text-align:left;">

Food fish production by species (1 000 kg, million e)

</td>

</tr>

<tr>

<td style="text-align:left;">

Production:

</td>

<td style="text-align:left;">

Quantity (1 000 kg)

</td>

</tr>

<tr>

<td style="text-align:left;">

Area:

</td>

<td style="text-align:left;">

Sea

</td>

</tr>

<tr>

<td style="text-align:left;">

Fish:

</td>

<td style="text-align:left;">

Rainbow trout

</td>

</tr>

<tr>

<td style="text-align:left;">

Year:

</td>

<td style="text-align:left;">

All

</td>

</tr>

</tbody>

</table>

<br>

**Total mariculture by Area**
<!-- dataset save location BHI_share/2.0/Goals/FP/Mariculture/mar_fi/mar_by_area.csv -->

<table class="table" style="">

<caption>

Source: [Natural Resources Institute
database](http://statdb.luke.fi/PXWeb/pxweb/en/LUKE/LUKE__06%20Kala%20ja%20riista__02%20Rakenne%20ja%20tuotanto__10%20Vesiviljely/?tablelist=true&rxid=ef8a2896-d6cb-4660-a8aa-fee93bd4212f)
<br> Downloaded 2020-1-20 by Andrea De Cervo

</caption>

<thead>

<tr>

<th style="text-align:left;">

Option

</th>

<th style="text-align:left;">

Specification

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Luke statistics database:

</td>

<td style="text-align:left;">

Food fish production (1 000 kg ungutted fish) by
area

</td>

</tr>

<tr>

<td style="text-align:left;">

ELY-centre:

</td>

<td style="text-align:left;">

All

</td>

</tr>

<tr>

<td style="text-align:left;">

Year:

</td>

<td style="text-align:left;">

All

</td>

</tr>

<tr>

<td style="text-align:left;">

Area:

</td>

<td style="text-align:left;">

Sea

</td>

</tr>

</tbody>

</table>

<br/>

#### 2.1.4 Rainbow trout mariculture Germany

<!-- dataset save location BHI_share/2.0/Goals/FP/Mariculture/rbt_de/rbt_de.csv -->

<table class="table" style="">

<caption>

Source: [Email from plant manager](tassilo@jaeger-kleinicke.de) <br>
Received 2019-10-30

</caption>

<thead>

<tr>

<th style="text-align:left;">

Option

</th>

<th style="text-align:left;">

Specification

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Location:

</td>

<td style="text-align:left;">

Kiel
Bay

</td>

</tr>

</tbody>

</table>

<br/>

#### 2.1.5 Finfish Mariculture

<!-- dataset save location BHI_share/2.0/Goals/FP/Mariculture/Finfish_mariculture/Finfish_mariculture.shp -->

<table class="table" style="">

<caption>

Source: [HELCOM
database](http://maps.helcom.fi/website/mapservice/?datasetID=3cfa469a-6a78-4913-b82f-57fd0e7f4dc0)
<br>
[metadata](http://metadata.helcom.fi/geonetwork/srv/eng/catalog.search#/metadata/3cfa469a-6a78-4913-b82f-57fd0e7f4dc0)
<br> Downloaded 2019-10-09 by Andrea De Cervo

</caption>

<thead>

<tr>

<th style="text-align:left;">

Option

</th>

<th style="text-align:left;">

Specification

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

HELCOM Map and Data service:

</td>

<td style="text-align:left;">

Pressures and human activities

</td>

</tr>

<tr>

<td style="text-align:left;">

Human activities:

</td>

<td style="text-align:left;">

Finfish
mariculture

</td>

</tr>

</tbody>

</table>

<br/>

#### 2.1.6 All Baltic countries, Aquaculture Production (FAO)

<!-- dataset save location BHI_share/2.0/Goals/FP/Mariculture/FAO_fishstat/FAO_fishstat.csv -->

<table class="table" style="">

<caption>

Source: [FAO
FishStatJ](http://www.fao.org/fishery/statistics/software/fishstatj/en#downlApp)
<br> Downloaded 2019-10-16 by Andrea De Cervo

</caption>

<thead>

<tr>

<th style="text-align:left;">

Option

</th>

<th style="text-align:left;">

Specification

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

FAO FishStatJ:

</td>

<td style="text-align:left;">

Global aquaculture production

</td>

</tr>

<tr>

<td style="text-align:left;">

Quantity:

</td>

<td style="text-align:left;">

1950-2017

</td>

</tr>

</tbody>

</table>

<br/>

#### 2.1.7 Reference Point: maximum nutrient discharge (N and P)

In 2016 HELCOM Contracting Parties adopted Recommendation 37/3 on
[“Sustainable aquaculture in the Baltic Sea
Region”](http://www.helcom.fi/Recommendations/Rec%2037-3.pdf).

However, the only parameter that can be used as reference point for
Mariculture subgoal in the Baltic Sea (that can be also found on Finfish
Mariculture data provided by HELCOM) is the [maximum nutrient discharge
for P and N](http://www.helcom.fi/Recommendations/Rec%2025-4.pdf), where
existing and new marine fish farms should not exceed the annual average
of: - 7g of P (tot-P) - 50g of N (tot-N) per 1kg fish (living weight)
produced. (The nutrient limit values (P and N) are calculated on the
basis that living fish contains 0,4% of phosphorus and 2,75% of
nitrogen.)

This recommendation was adopted in 2004, but since there are not updated
nutrient limit values, this will be the parameter used as reference
point.

Only data provided by HELCOM on finfish mariculture include nutrient
discharge. However, nutrient inputs from fish farms in Denmark were
calculated on the basis of production data, and some numbers that were
missing on nutrient input in Sweden were calculated by SCB, Statistics
Sweden based on average input/ amount produced fish. Therefore,
estimates for all other datasets will be made.

<br>

### 2.2 Centralization & Normalization

``` r
## root location of the raw data
dir_rawdata <- file.path(dir_B, "Goals", "FP", "MAR") # list.files(dir_rawdata)

## rainbow trout data
rbt_de_raw <- read_delim(file.path(dir_rawdata, "rbt_de", "rbt_de.csv"), delim = ";")
rbt_dk_raw <- read_delim(file.path(dir_rawdata, "rbt_dk", "rbt_dk.csv"), delim = ";")
rbt_dk2005raw <- read_delim(file.path(dir_rawdata, "rbt_dk", "rbt_dk_2005.csv"), delim = ";")
rbt_fi_raw <- read_delim(file.path(dir_rawdata, "rbt_fi", "rbt_fi.csv"), delim = ";")
rbt_se_raw <- read_delim(file.path(dir_rawdata, "rbt_se", "rbt_se.csv"), delim = ";")

## finland mariculture data by area and year
mar_fi_raw <- read_csv(file.path(dir_rawdata, "mar_fi", "mar_by_area.csv"))

## FAO rainbow trout data
fao_rbt_raw <- read_delim(file.path(dir_rawdata, "FAO_fishstat", "FAO_fishstat.csv"), delim = ";")

# countrycodes for FAO dataset
countrycode <- data.frame(
  area = c("Germany", "Denmark", "Estonia", "Finland","Sweden"),
  country = c("DE", "DK", "ES", "FI", "SE")
)

## HELCOM Finfish mariculture data
## this is spatial data so we need to remove the geometry column
## also need to convert many columns from factor to numeric
finfish_mariculture_raw <- sf::st_read(file.path(dir_rawdata, "Finfish_mariculture", "Finfish_mariculture.shp"))
st_geometry(finfish_mariculture_raw) <- NULL
finfish_mariculture_raw[, 1:28] <- sapply(finfish_mariculture_raw[, 1:28], as.character)
finfish_mariculture_raw[, 8:28] <- sapply(finfish_mariculture_raw[, 8:28], as.numeric)
```

-----

<br>

#### 2.2.1 Spatial regions of the Data

**Create lookup tab for matching areas with bhi regions.**  
May be multiple designations for the same areas, or
intersections/overlap of different areas. To avoid double-counting, need
to distinguish national/subnational data.

``` r
bhiregions <- bind_rows(
  ## germany
  c(country = "DE", lvl = "subnatl", area = "Kiel Bay", rgn = 8),
  ## denmark
  c(country = "DK", lvl = "subnatl", area = "Arhus_cty", rgn = 3),
  c(country = "DK", lvl = "subnatl", area = "Vejle_cty", rgn = 3),
  c(country = "DK", lvl = "subnatl", area = "Fyns_cty", rgn = 3),
  c(country = "DK", lvl = "subnatl", area = "Sønderjylland_cty", rgn = 3),
  c(country = "DK", lvl = "subnatl", area = "Nordjyllands_cty", rgn = 2),
  c(country = "DK", lvl = "subnatl", area = "Frederiksborg_cty", rgn = list(2, 6)),
  c(country = "DK", lvl = "subnatl", area = "Roskilde_cty", rgn = 12),
  c(country = "DK", lvl = "subnatl", area = "Storstrøms_cty", rgn = list(3, 7, 9, 12)),
  c(country = "DK", lvl = "subnatl", area = "Vestsjællands_cty", rgn = list(2, 3)),
  c(country = "DK", lvl = "subnatl", area = "Hovedstaden", rgn = list(2, 6, 12, 15)),
  c(country = "DK", lvl = "subnatl", area = "Sjælland", rgn = list(3, 7, 9, 12)),
  c(country = "DK", lvl = "subnatl", area = "Syddanmark", rgn = 3),
  c(country = "DK", lvl = "subnatl", area = "Midtjylland", rgn = list(2, 3)),
  c(country = "DK", lvl = "subnatl", area = "Belts Danish coastal waters", rgn = 3),
  c(country = "DK", lvl = "subnatl", area = "Bornholm Basin", rgn = 15),
  c(country = "DK", lvl = "subnatl", area = "Arkona Basin", rgn = 12),
  ## sweden
  c(country = "SE", lvl = "subnatl", area = "Kalmar lan", rgn = 26),
  c(country = "SE", lvl = "subnatl", area = "Gavleborgs lan", rgn = 37),
  c(country = "SE", lvl = "subnatl", area = "Vasternorrlands lan", rgn = 37),
  c(country = "SE", lvl = "subnatl", area = "Stockholm lan", rgn = list(35, 29)),
  c(country = "SE", lvl = "subnatl", area = "Vasterbottens lan", rgn = list(41, 39)),
  c(country = "SE", lvl = "subnatl", area = "Norrbottens lan", rgn = 41),
  c(country = "SE", lvl = "subnatl", area = "Aland Sea", rgn = 35),
  c(country = "SE", lvl = "subnatl", area = "Western Gotland Basin", rgn = 26),
  c(country = "SE", lvl = "subnatl", area = "Bornholm Basin", rgn = 14),
  c(country = "SE", lvl = "subnatl", area = "The Sound", rgn = 5),
  c(country = "SE", lvl = "subnatl", area = "Bothnian Sea", rgn = 37),
  c(country = "SE", lvl = "subnatl", area = "Bothnian Bay", rgn = 41),
  c(country = "SE", lvl = "subnatl", area = "Ne_coast", rgn = list(37, 39, 41)),
  c(country = "SE", lvl = "subnatl", area = "Other_coasts", rgn = list(29, 35, 26, 14, 11, 5, 1)), 
  ## finland
  c(country = "FI", lvl = "subnatl", area = "Archipelago Sea", rgn = 36),
  c(country = "FI", lvl = "subnatl", area = "Bothnian Sea", rgn = 38),
  c(country = "FI", lvl = "subnatl", area = "Gulf of Finland", rgn = 32),
  c(country = "FI", lvl = "subnatl", area = "Bothnian Bay", rgn = 42),
  c(country = "FI", lvl = "subnatl", area = "The Quark", rgn = 40),
  c(country = "FI", lvl = "subnatl", area = "Aland", rgn = 36),
  c(country = "FI", lvl = "subnatl", area = "Kainuu", rgn = 42),
  c(country = "FI", lvl = "subnatl", area = "Ostrobothnia", rgn = list(38, 40, 42)),
  c(country = "FI", lvl = "subnatl", area = "Kaakkois_Suomi", rgn = 32),
  c(country = "FI", lvl = "subnatl", area = "Uusimaa", rgn = 32),
  c(country = "FI", lvl = "subnatl", area = "Varsinais_Suomi", rgn = list(36, 38)),
  
  ## countries
  c(country = "DE", lvl = "natl", area = "Germany", rgn = list(16, 13, 10, 8, 4)),
  c(country = "DK", lvl = "natl", area = "Denmark", rgn = list(15, 12, 9, 7, 6, 3, 2)),
  c(country = "SE", lvl = "natl", area = "Sweden", rgn = list(41, 39, 37, 35, 29, 26, 20, 14, 11, 5, 1)),
  c(country = "FI", lvl = "natl", area = "Finland", rgn = list(42, 40, 38, 36, 32, 30)),
  c(country = "ES", lvl = "natl", area = "Estonia", rgn = list(34, 31, 28, 25)),
  c(country = "LA", lvl = "natl", area = "Latvia", rgn = list(27, 24)),
  c(country = "LI", lvl = "natl", area = "Lithuania", rgn = 23),
  c(country = "PO", lvl = "natl", area = "Poland", rgn = list(21, 18, 17)),
  c(country = "RU", lvl = "natl", area = "Russia", rgn = list(33, 22, 19))
)

bhiregions[, 4:15] <- sapply(bhiregions[, 4:15], as.numeric)
bhiregions <- bhiregions %>% 
  tidyr::pivot_longer(cols = 4:15, names_to = "num", values_to = "region_id") %>% 
  filter(!is.na(region_id)) %>% 
  dplyr::select(country, area, region_id, level = lvl)
```

<br>

``` r
source(here::here("R", "spatial.R"))
regions_shape() # loads spatial features objects

bhi_rgns_simple <- rmapshaper::ms_simplify(input = BHI_rgns_shp) %>% 
  sf::st_as_sf() %>% 
  dplyr::right_join(
    dplyr::select(bhiregions, area, BHI_ID = region_id), 
    by = c("BHI_ID")
  ) %>% 
  mutate(areacountry = paste(area, rgn_nam, sep = ", "))

cols <- c(
  RColorBrewer::brewer.pal(7, "Set2"), 
  RColorBrewer::brewer.pal(9, "Set1"),
  RColorBrewer::brewer.pal(6, "Accent")
)

areasMap <- ggplot2::ggplot() + 
  geom_sf(
    data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      st_crop(xmin = 0, xmax = 40, ymin = 53, ymax = 67),
    fill = "ivory", 
    size = 0.1
  ) +
  geom_sf(
    data = bhi_rgns_simple, 
    aes(fill = areacountry),
    colour = NA,
    show.legend = FALSE
  ) +
  facet_wrap(~area, ncol = 6) +
  scale_fill_manual(
    values = colorRampPalette(cols)(51)[sample(1:51)]
  ) +
  scale_x_continuous(limit = c(5, 37)) +
  scale_y_continuous(limit = c(53.5, 66)) +
  theme(
    plot.margin = grid::unit(c(2, 2, 2, 2), "mm"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

MARareasMap <- here::here("data", "MAR", version_year, "intermediate")
ggsave("mar_areas_maps.png", areasMap, "png", MARareasMap, width = 14, height = 15.5, units = c("in"), dpi = 250)

knitr::include_graphics(file.path(MARareasMap, "mar_areas_maps.png"))
```

<img src="/Users/andreadecervo/github/bhi-prep/data/MAR/v2019/intermediate/mar_areas_maps.png" width="100%" />

<br>

#### 2.2.2 Finish datasets: use total-mariculture-by-region to downscale Rainbow Trout data

Data were available both as total marine production by region within
Finlnd, and also as total production nation-wide per fish species.
Rainbow trout dominated the total fish production (over 90%). Following
Ginnette’s approach from the first assessment, we convert the total
production by region to rainbow trout production by region by using the
country wide percent rainbow trout of the marine production in each
year. Other minor contributions to total production were European
whitefish, Trout, other species, Roe of rainbow trout, roe of european
whitefish.

``` r
## two datasets:
## one on rainbow trout production at national level,
## the other with total mariculture production by region

mar_finland <- bind_rows(
  ## rainbow trout production in finland
  rbt_fi_raw %>% 
    dplyr::mutate(
      farm_species = str_to_sentence(production),
      produced_tonnes = tot_wt_tonnes,
      area = "Finland"
    ) %>% 
    dplyr::select(-production, -tot_wt_tonnes) %>% 
    filter(year >= 1997),
  ## total mariculture production by area in finland
  mar_fi_raw %>% 
    mutate(
      area = str_replace(area, "\x81land", "Aland"),
      area = str_replace(area, "Etel\x8a-Savo", "Etel-Savo"),
      area = str_replace(area, "H\x8ame", "Hame"),
      area = str_replace(area, "TOTAL", "Finland Total")
    ) %>% 
    filter(
      # !area == "TOTAL",
      ## no/very little coastline associated with these areas
      !area %in% c("Hame", "North Karelia", "Central Finland", "Etel-Savo", "Pohjois-Savo", "Lapland")
    ) %>% 
    tidyr::gather(key = "year", value = "produced_tonnes", -area) %>% 
    mutate(year = as.numeric(year)) %>% 
    dplyr::mutate(farm_species = "All mariculture")
)
## plot production to compare across years and areas
## substitute data = mar_finland_totals to see just national totals trout vs. all species
ggplot(mapping = aes(year, produced_tonnes, fill = area), data = mar_finland) +
  geom_col(position = position_dodge(), width = 0.8, color = "grey60", alpha = 0.8) +
  ggtitle("Finnish mariculture production (tonnes) 1997-2018") +
  labs(x = "Year", y = "Production (tonnes)", fill = "Area")
```

![](mar_prep_files/figure-gfm/merge%20two%20finish%20datasets-1.png)<!-- -->

``` r
## comparison of 'all mariculture' data totals and national level species-specific data
## sub-dataset with only total mariculture for Finland and/or national level species-specific prduction data
mar_finland_totals <- filter(mar_finland, str_detect(area, "^Finland"))

hist_finland <- ggplot(mar_finland_totals, aes(produced_tonnes)) +
  geom_histogram(binwidth = 400, fill = "blue", alpha = 0.5, color = "grey") +
  facet_wrap( ~ area, nrow = 2)

boxplot_finland <- ggplot(mar_finland_totals, aes(area, produced_tonnes)) + 
  stat_boxplot(geom = "errorbar", width = 0.2) + 
  geom_boxplot()

# t.test(produced_tonnes ~ area, mar_finland_totals)
# wilcox.test(produced_tonnes ~ area, mar_finland_totals)  
## p ≈ 0.17 so don't reject null that true location shift is equal to 0

## finland area-specific rainbow trout data
## calculate percentage from the total to the species-specific prduction data
rbt_percent <- mar_finland_totals %>%
  dplyr::select(-farm_species) %>% 
  tidyr::pivot_wider(names_from = area, values_from = produced_tonnes) %>% 
  dplyr::rename(
    tot_rainbow_trout = Finland,
    total = `Finland Total`
  ) %>% 
  mutate(rbt_percent = (tot_rainbow_trout/total))


rbt_fi_area <- mar_finland %>% 
  filter(area != "Finland", area != "Finland Total") %>% 
  select(-farm_species) %>%
  tidyr::pivot_wider(names_from = area, values_from = produced_tonnes) %>% 
  ## regional 'produced tonnes' values are total mariculture
  rename(
    Varsinais_Suomi = `Varsinais-Suomi`,
    Kaakkois_Suomi = `Southeastern Finland`
  ) %>% 
  left_join(rbt_percent, by = "year") %>% 
  tidyr::pivot_longer(cols = 2:7) %>% 
  mutate(
    country = "FI",
    area = name,
    farm_species = "Rainbow trout",
    produced_tonnes = value*rbt_percent,
    p_kg = as.numeric(NA),
    n_kg = as.numeric(NA)
  ) %>% 
  select(-tot_rainbow_trout, -total, -rbt_percent, -name, -value)
  
## add back years where don't have area specific data
rbt_fi_area <- bind_rows(
  rbt_fi_area,
  rbt_fi_raw %>% 
    dplyr::mutate(
      farm_species = str_to_sentence(production),
      produced_tonnes = tot_wt_tonnes,
      country = "FI", area = "finland",
      p_kg = as.numeric(NA),
      n_kg = as.numeric(NA),
      area = capitalize(area)
    ) %>% 
    dplyr::select(-production, -tot_wt_tonnes) %>% 
    filter(year < 1997)
)

gridExtra::grid.arrange(hist_finland, boxplot_finland, nrow = 1, widths = c(2, 1.5))
```

![](mar_prep_files/figure-gfm/merge%20two%20finish%20datasets-2.png)<!-- -->

-----

<br>

#### 2.2.3 Merge datasets

**National and FAO datasets**

``` r
rbt_raw <- bind_rows(
  
  ## finland rainbow trout data by area calculated
  rbt_fi_area,
  
  ## rbt_de rainbow trout germany
  rbt_de_raw %>% 
    dplyr::mutate(farm_species = str_to_sentence(production)) %>% 
    tidyr::gather(key = "area", value = "produced_tonnes", -year, -farm_species, -production) %>%
    dplyr::mutate(
      produced_tonnes = as.numeric(produced_tonnes),
      country = "DE",
      p_kg = as.numeric(NA),
      n_kg = as.numeric(NA),
      area = capitalize(area),
      area = str_replace(area, "Kiel_bay", "Kiel Bay")
    ) %>% 
    select(year, country, area, farm_species, produced_tonnes, p_kg, n_kg) %>% 
    arrange(country),
  
  ## rbt_dk2005 rainbow trout 2005 denmarks
  rbt_dk2005raw %>% 
    dplyr::mutate(farm_species = str_to_sentence(production)) %>% 
    dplyr::select(-tot_wt_ton, -production) %>% 
    dplyr::rename(
      arhus_cty = cty1, 
      vejle_cty = cty2,
      fyns_cty = cty3,
      sønderjylland_cty = cty4,
      ribe_cty = cty5,
      ringkøbing_cty = cty6,
      viborg_cty = cty7,
      nordjyllands_cty = cty8,
      frederiksborg_cty = cty9,
      roskilde_cty = cty10,
      storstrøms_cty  = cty11,
      vestsjællands_cty = cty12
    ) %>% 
    tidyr::gather(key = "area", value = "produced_tonnes", -year, -farm_species) %>% 
    # filter out the counties on the Atlantic coasts
    filter(
      !(area == "viborg_cty"),
      !(area == "ringkøbing_cty"),
      !(area == "ribe_cty")
    ) %>% 
    dplyr::mutate(
      country = "DK",
      p_kg = as.numeric(NA),
      n_kg = as.numeric(NA),
      area = capitalize(area)
    ) %>% 
    arrange(country),
  
  ## rbt_dk rainbow trout denmark
  ## https://www.was.org/easOnline/AbstractDetail.aspx?i=4377
  rbt_dk_raw %>% 
    dplyr::mutate(farm_species = str_to_sentence(production)) %>% 
    dplyr::select(-production) %>% 
    dplyr::rename(
      denmark = tot_wt_ton,
      hovedstaden = "reg1", 
      sjælland = "reg2",
      syddanmark = "reg3",
      midtjylland = "reg4"
    ) %>% 
    tidyr::gather(key = "area", value = "produced_tonnes", -year, -farm_species) %>% 
    dplyr::mutate(
      country = "DK",
      p_kg = as.numeric(NA),
      n_kg = as.numeric(NA),
      area = capitalize(area)
    ) %>% 
    arrange(country),
  
  ## rbt_se rainbow trout sweden
  rbt_se_raw %>% 
    dplyr::mutate(
      farm_species = str_to_sentence(production),
      sweden = tot_wt_mtonnes
    ) %>% 
    select(-ne_farm_no, -other_farm_no, -production, -tot_wt_mtonnes) %>%
    tidyr::gather(key = "area", value = "produced_tonnes", -year, -farm_species) %>% 
    mutate(
      country = "SE",
      p_kg = as.numeric(NA),
      n_kg = as.numeric(NA),
      area = capitalize(area)
    ) %>% 
    arrange(country)
  
) %>% mutate(source = "national")

rbt_raw <- bind_rows(
  rbt_raw, 
  ## fao_rbt fao data on rainbow trout
  fao_rbt_raw %>% 
    dplyr::mutate(
      area = country,
      farm_species = str_to_sentence(species)
    ) %>% 
    filter(
      ## aqua area atlantic NE, so maybe keep marine?
      # !(environment == "Marine" & area == "Denmark"), 
      str_detect(farm_species, "Rainbow trout")
    ) %>% 
    select(-aqua_area, -Unit, -environment, -("1950":"1971"), -country, -species) %>%
    tidyr::gather(key = "year", value = "produced_tonnes", -area, -farm_species) %>% 
    
    ## remove characters from production numeric values
    ## "F" stands FAO estimates from available sources of information
    mutate(
      year = as.numeric(year),
      produced_tonnes = str_replace(produced_tonnes, "F", " "), 
      produced_tonnes = as.numeric(produced_tonnes),
      p_kg = as.numeric(NA),
      n_kg = as.numeric(NA),
      area = capitalize(area)
    ) %>% 
    group_by(area, year) %>% 
    mutate(
      produced_tonnes = sum(produced_tonnes),
      p_kg = sum(p_kg),
      n_kg = sum(n_kg)
    ) %>% 
    distinct() %>% 
    filter(!is.na(produced_tonnes)) %>% 
    
    ## join with countrycode data
    left_join(countrycode, by = "area") %>% 
    mutate(source = "fao") %>% 
    select(year, country, area, farm_species, produced_tonnes, p_kg, n_kg, source) %>% 
    
    filter(area != "Finland") %>% # remove finland fao data-- duplicates or superceded by rbt_fi_area calculation 
    filter(!(year %in% 2006:2017 & area == "Denmark")) %>% # use Denmark national data instead here...
    filter(!(year %in% 2009:2018 & area == "Sweden")) # sweden natl data matches fao, but includes 2009-2018
)

## reassign types and join bhi region_ids by area
## use lookup table defined in code chunk above
rbt_raw[, c(5:7)] <- sapply(rbt_raw[, c(5:7)], as.numeric) 
```

-----

<br>

**Finfish data from HELCOM**

``` r
## wrangling finfish data so can merge with other datasets
## source of this dataset is helcom
## spatial file located here:
## http://metadata.helcom.fi/geonetwork/srv/eng/catalog.search#/metadata/3cfa469a-6a78-4913-b82f-57fd0e7f4dc0
finfish_raw <- as_tibble(finfish_mariculture_raw) %>% 
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), COUNTY, SUBBASIN)) %>% 
  dplyr::rename(
    area = SUBBASIN,
    farm_species = PRODUCTION,
    country = COUNTRY
  ) %>% 
  ## remove the facilities on land
  ## these are entries with any of NA, on land, or On land as 'AREA'
  ## are we sure that 'NA' value for AREA means that it is on land? there are so many NAs, leaves only "SE" "DK" "FI" data...
  filter(is.na(AREA) |!str_detect(str_to_lower(AREA), "on land"),
         !(area == "Bay of Mecklenburg")) %>% 
  tibble::rowid_to_column("farm_ID") %>% 
  mutate(farm_ID = as.factor(farm_ID)) %>% 
  ## replace NA value with general mariculture name 'finfish'
  mutate(
    area = str_replace(area,"Sland Sea","Aland Sea"),
    AREA = str_replace(AREA, ",", "."),
    farm_species = ifelse(
      is.na(farm_species), 
      "Finfish", ifelse(
        str_detect(farm_species, "Rainbow trout|rainbow trout"),
        "Rainbow trout", farm_species
      )
    )
  ) %>% select(-AREA, -NAME, -COMMENTS)

## rename names within AREA without <?> symbols
arealookup <- data.frame(
  fix_area = c(
    "Kalmar lan",
    "Gavleborgs lan",
    "Vasternorrlands lan",
    "Vasternorrlands lan",
    "Stockholm lan",
    "Vasterbottens lan",
    "Norrbottens lan"
  ),
  area = unique(finfish_raw$area)[1:7]
)
arealookup[,] <- sapply(arealookup[,], as.character)

## gather production, phosphorus and nitrogen vars by year
spreadcols <- names(finfish_raw)[1:5]
gathercols <- names(finfish_raw)[6:26]

## reshape to have year and 3 variable cols: 
## production (PR), nitrogen (N), and phosphorus (P)
finfish_gather_yr <- as_tibble(finfish_raw) %>% 
  tidyr::pivot_longer(cols = gathercols, names_to = "varyear", values_to = "value") %>% 
  mutate(
    year = str_extract(varyear, pattern = "[0-9]{4}"),
    var = str_extract(varyear, pattern = "[A-Z]+")
  ) %>% 
  filter(!is.na(year)) %>%  # filter out na years as these represent aggregated values, but keep NA values
  tidyr::pivot_wider(id_cols = c(spreadcols, "year", "farm_ID"), names_from = var, values_from = value) %>% 
  ## where production is zero and nutrients aren't close to zero, assume production should be NA...
  rowwise() %>% 
  dplyr::mutate(PR = ifelse(PR < 1 & (!is.na(N)|!is.na(P)), ifelse(any(c(N,P) < 10), PR, NA), PR)) %>% 
  ungroup() %>%
  ## keep only necessary columns and join with renamed areas
  select(year, country, area, farm_species, produced_tonnes = PR, p_kg = P, n_kg = N, farm_ID) %>% 
  left_join(arealookup, by = "area") %>%
  mutate(area = ifelse(!is.na(fix_area), fix_area, area), source = "helcom") %>%
  select(-fix_area, -farm_ID)
```

<br>

``` r
combined_rawdata <- rbind(rbt_raw, finfish_gather_yr)
```

``` r
readr::write_csv(
  combined_rawdata, 
  here::here("data", "MAR", version_year, "intermediate", "mar_merged_rawdata.csv")
)
```

-----

<br>

#### 2.2.4 Standardize Units: Production regions and BHI Region assignments

Note: where multiple BHI regions exist within the areas reported for
mariculture data (e.g. “Ne\_coast” for Sweden contains BHI regions 37,
39, and 41), the data values are split among the regions based on region
sizes, i.e. with the assumption that each unit area is equally likely to
contain the given mariculture farm… For future assessments, if adequate
data are available, mariculture sites may be matched to BHI regions more
precisely using spatial files (shapefiles).

``` r
## join spatial area information to bhiregions lookup table
st_geometry(bhi_rgns_simple) <- NULL
bhiregions <- bhi_rgns_simple %>% 
  dplyr::select(region_id = BHI_ID, Area_km2) %>% 
  distinct() %>%
  right_join(bhiregions, by = "region_id") %>% 
  mutate(level = as.numeric(ifelse(level == "natl", "1", "2")))

## combine the datasets and add the ID column, then deal with overlapping data
## overlap here due to nesting of different spatial resolutions, not duplicate entries

combined_w_rgns <- combined_rawdata %>%
  left_join(bhiregions, by = c("country", "area")) %>% 
  ## in the case where theres no subnational data, use national
  mutate(level = ifelse(level == 2 & is.na(produced_tonnes) & is.na(p_kg)& is.na(n_kg), 0, level)) %>% 
  group_by(year, country, region_id, farm_species) %>% 
  ## top_n function will select uppermost available level within grouping
  top_n(1, level) %>% 
  ungroup()

level_area <- combined_w_rgns %>%
  group_by(year, country, level, area, farm_species) %>%
  summarize(level_area_km2 = sum(Area_km2)) %>%
  ungroup()

combined_w_rgns <- combined_w_rgns %>%
  left_join(level_area, by = c("year", "country", "level", "area", "farm_species")) %>%
  mutate(area_frac = Area_km2/level_area_km2) %>% 
  group_by(year, country, level, area, farm_species, region_id) %>% 
  mutate(area_frac = sum(area_frac)) %>%  # if there are multiple points per group, should keep unique
  ungroup() %>% 
  
  ## split production and nutrient use 
  ## based on sizes of regions overlapping subnational areas
  mutate(
    ## split at subnational level
    split_production = ifelse(level == 2, produced_tonnes*area_frac, NA),
    split_N = ifelse(level == 2, n_kg*area_frac, NA),
    split_P = ifelse(level == 2, p_kg*area_frac, NA)
  ) %>%
  group_by(year, country, farm_species) %>% 
  ## calculate subnational totals to subtract from national
  ## so remainder can be split among bhi regions without subnational categorization
  ## use only national data so don't count production amounts twice
  ## for nutrients, only have from helcom data, so use those
  mutate(
    subnatl_prod = sum(ifelse(source == "national", split_production, 0), na.rm = TRUE),
    subnatl_N = sum(ifelse(source == "helcom", split_N, 0), na.rm = TRUE),
    subnatl_P = sum(ifelse(source == "helcom", split_P, 0), na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    ## split remainder at national level
    split_production = ifelse(level == 1, (produced_tonnes-subnatl_prod)*area_frac, split_production),
    split_N = ifelse(level == 1, (n_kg-subnatl_N)*area_frac, split_N),
    split_P = ifelse(level == 1, (p_kg-subnatl_P)*area_frac, split_P)
  ) %>% 
  select(
    year, country, source, area, level, region_id, farm_species,
    produced_tonnes, n_kg, p_kg, 
    rgn_area_km2 = Area_km2, area_fraction = area_frac,
    split_production, split_N, split_P
  )
```

``` r
readr::write_csv(
  combined_w_rgns, 
  here::here("data", "MAR", version_year, "intermediate", "mar_combined_w_rgns.csv")
)
```

-----

<br/>

### 2.3 Initial Data Exploration

**Data by Source**

``` r
datasources <- ggplot(
  combined_w_rgns %>% 
    mutate(region_id = as.factor(region_id)) %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(source = ifelse(source == "fao", "FAO", str_to_title(source))),
  aes(x = year, y = split_production, color = source)
)
datasources + 
  geom_point(alpha =  0.5) + 
  facet_wrap(~ country, ncol = 1) + 
  labs(x = "Year", y = "Production, split over BHI Regions\n", color = "Data Source") +
  theme(legend.position = "bottom")
```

![](mar_prep_files/figure-gfm/explore%20raw%20data%20by%20source%20and%20country-1.png)<!-- -->

<br>

<br/>

## 3\. Considerations for `BHI3.0`

  - If more adequate data are available, mariculture sites may be
    matched to BHI regions more precisely using spatial files
    (shapefiles). In particular, regarding Sweden, there are reports
    (e.g. [page 8,
    table 9](https://www.scb.se/contentassets/8964475d3a8b4aa88af32e4fcbc248eb/jo1201_2014a01_sm_jo60sm1501.pdf))
    where information about rainbow trout production in “other coasts”
    is split into two areas: sounth-eastern coast and south-western
    coast. However, these data are only available until 2014.

  - Also, collect more consistent information and data on mariculture
    and its sustainable production.

<br>

## 4\. References

[Trujillo, P.,
(2008)](http://www.seaaroundus.org/doc/publications/books-and-reports/2008/Alder-and-Pauly-comparative-assessment-of-biodiversity-fisheries-and-aquaculture-53-countries-EEZ.pdf).
Using a mariculture sustainability index to rank countries’ performance.
Pp. 28-56 In: Alder, J. and D. Pauly (eds.). 2008. A comparative
assessment of biodiversity, fisheries and aquaculture in 53 countries’
Exclusive Economic Zones.’ Fisheries Centre Research Reports. Fisheries
Centre, University of British Columbia

[HELCOM
Recommendation](http://www.helcom.fi/Recommendations/Rec%2025-4.pdf)

[Ginnette Flores Carmenate Master
Thesis](https://bioenv.gu.se/digitalAssets/1575/1575339_flores-carmenate.pdf)

<br>
