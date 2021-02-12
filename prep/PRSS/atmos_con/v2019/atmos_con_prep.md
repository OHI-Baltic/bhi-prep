Atmospheric Contaminants Pressure Layer Data Preparation
================

## 1\. Background

Annual atmospheric deposition fluxes of PCB-153 and PCDD/Fs over the
Baltic Sea can negatively affect scores for different goals, in
particular the Artisanal Fishing Opportunities (AO) goal, the Carbon
Storage (CS) goal, the Contaminants (CON) sub-goal, the Fisheries (FIS)
and Mariculture (MAR) sub-goals, the Iconic Species (ICO) sub-goal, the
Biodiversity (BD) and Natural Products (NP) goals.

## 2\. Data

This prep document is used to generate and explore the following data
layers:

  - `po_atmos_pcb153_bhi2019.csv`
  - `po_atmos_pcdd_bhi2019.csv`

These are saved to the `layers/v2019` folder. All these are derived from
or informed by the raw datasets from HELCOM and EMEP/MSC database.

<br>

``` r
source(file.path(here::here(), "R", "data.R"))
source(file.path(here::here(), "R", "common.R"))
source(file.path(here::here(), "R", "spatial.R"))

## root location of the raw data
dir_B <- file.path(dirname(dir_prep), "bhi-data", "BHI 2.0")
dir_rawdata <- file.path(dir_B, "Pressure", "atmos_con")
```

## 2\. Data

### 2.1 Atmospheric deposition of PCB-153 on the Baltic Sea

Data are provided for nine major Baltic Sea basins: Archipelago Sea,
Baltic Proper, Bothnian Bay, Bothnian Sea, Gulf of Finland, Gulf of
Riga, Kattegat, The Sound, Western Baltic.

Data downloaded from the [HELCOM
website](https://helcom.fi/baltic-sea-trends/environment-fact-sheets/hazardous-substances/)
on 19 August 2020 by Andrea De Cervo. Data are provided on the basin
level and extracted from the pdf file.

**Table 1:** Computed annual atmospheric deposition of PCB-153 over the
six Baltic Sea sub-basins, the whole Baltic Sea (BAS) and normalized
deposition to the Baltic Sea (Norm) for period 1990-2016. Units:
kg/year.

**Table 2:** Computed contributions by country to annual total
deposition of PCB-153 to nine Baltic Sea sub-basins for the year 2016.
Units: kg/year. HELCOM: contribution of anthropogenic sources of HELCOM
countries; EMEP: contribution of anthropogenic sources in other EMEP
countries;. NSR: contributions of sources other than primary
anthropogenic emissions (natural, secondary (re-emission), and remote
sources).

#### 2.1.1 Additional reference information as provided on the HELCOM website

**Data Source: EMEP/MSC**

#### 2.1.2 Description of data

Annual atmospheric deposition fluxes of PCB-153 were obtained using the
latest version of MSCE- POP model developed at EMEP/MSC-E (Gusev et al.,
2005). Assessment of global scale transport and fate of PCBs was made on
the basis of the inventory of global PCB emissions \[Breivik et al.,
2007\] and emissions officially reported by the EMEP countries. The
inventory of Breivik et al. \[2007\] provided consistent set of
historical and future emissions of 22 individual PCB congeners from 1930
up to 2100. Model simulations for the period 1990 and 2013 were carried
out for indicator congener PCB-153. The spatial distribution of PCB-153
emissions within the EMEP region was prepared using gridded PCB
emissions officially submitted by 20 EMEP countries, including all
HELCOM countries except Denmark and Russia, and the emission expert
estimates worked out by TNO \[Denier van der Gon et al., 2005\].
Temporal variation of emissions were derived from the officially
reported PCB emissions. Units: kg/year.

#### 2.1.3 Geographical coverage

Annual atmospheric deposition fluxes of PCB-153 were obtained for the
EMEP region.

#### 2.1.4 Temporal coverage

Timeseries of annual atmospheric deposition are available for the period
1990 – 2016.

#### 2.1.5 Methodology and frequency of data collection

Atmospheric input and source allocation budgets of PCB-153 to the Baltic
Sea and its catchment area were computed using the latest version of
MSCE-POP model. MSCE-POP is the regional-scale model operating within
the EMEP region. This is a three-dimensional Eulerian model which
includes processes of emission, advection, turbulent diffusion, wet and
dry deposition, degradation, gaseous exchange with underlying surface,
and inflow of pollutant into the model domain. Horizontal grid of the
model is defined using stereographic projection with spatial resolution
50 km at 60o latitude. The description of EMEP horizontal grid system
can be found in the internet (<http://www.emep.int/grid/index.html>).
Vertical structure of the model consists of 15 non-uniform layers
defined in the terrain-following s-coordinates and covers almost the
whole troposphere. Detailed description of the model can be found in
EMEP reports (Gusev et al., 2005) and in the Internet on EMEP web page
(<http://www.emep.int/>) under the link to information on Persistent
Organic Pollutants. Meteorological data used in the calculations for
1990-2016 were obtained using MM5 meteorological data preprocessor on
the basis of meteorological analysis of European Centre for Medium-Range
Weather Forecasts (ECMWF).

Results of model simulation of atmospheric transport and annual
deposition of PCB-153 are provided on the regular basis annually two
years in arrears on the basis of emission data officially submitted by
Parties to CLRTAP Convention and available expert estimates of emission.

#### 2.1.6 Quality information

**Strength:** annually updated information on atmospheric input of
PCB-153 to the Baltic Sea and its sub-basins.  
**Weakness:** uncertainties in emissions of PCBs.

#### 2.1.7 Uncertainty

The MSCE-POP model results were compared with measurements of EMEP
monitoring network \[Gusev et al., 2006, Shatalov et al., 2005\]. The
model was evaluated through the comparison with available measurements
during EMEP TFMM meetings held in 2005. It was concluded that the
MSCE-POP model is suitable for the evaluation of the long range
transboundary transport and deposition of POPs in Europe.

### 2.2 Atmospheric deposition of PCDD/Fs on the Baltic Sea

Data downloaded from the [HELCOM
website](https://helcom.fi/baltic-sea-trends/environment-fact-sheets/hazardous-substances/)
on 19 August 2020 by Andrea De Cervo. Data are provided on the basin
level and extracted from the pdf file.

**No tables found in the document (it is [EMEP Centre on Emission
Inventories and Projections (CEIP)](http://www.ceip.at/) who makes the
deposition estimations for HELCOM, but we didn’t get any response from
them, so we will skip the PCDD/Fs layer for now.**

Gridded raw data of 23PeCDF were provided by Irina Strijkina
(EMEP/Meteorological Synthesizing Centre-East (MSC-E)) on 26 August 2020
via email (<http://en.msceast.org/database/EMEP/pcdd_dep_2018.dat>)

**Table:** Annual total deposition flux 23PeCDF (emissions 2018,
meteorology 2018) g TEQ/km2/year
EMEP\_01x01

#### 2.2.1 Additional reference information as provided on the HELCOM website

**Data Source: EMEP/MSC**

<br/>

## 3\. Pressure Model

### 3.1 Current conditions

Value in most recent year.

PCB 153 = 2016 PCDD = 2018

### 3.2 Rescaling

Min value = 0

Max value = maximum value across all basin 1990 - max year

## 4\. Data layer preparation

### 4.1 Read in and organize data

#### 4.1.1 Read in data

Computed annual atmospheric deposition of PCB-153 over the six Baltic
Sea sub-basins, the whole Baltic Sea (BAS) and normalized deposition to
the Baltic Sea (Norm) for period 1990-2016. Units:
kg/year.

``` r
pcb153 <- read.csv(file.path(dir_rawdata, "pcb153.csv"), sep=";", stringsAsFactors = FALSE)

# Annual total deposition flux 23PeCDF (emissions 2018, meteorology 2018) ug TEQ/km2/year EMEP_01x01
pcdd <- read_table2(url("http://en.msceast.org/database/EMEP/pcdd_dep_2018.dat"), skip = 2) %>% 
  slice(-1) %>% 
  rename(id = i,
         station = j,
         lon = long,
         value = "PCDD/Fs") %>% 
  mutate(unit = "ug TEQ/km2/year")
```

    ## Warning: 1 parsing failure.
    ## row col  expected    actual         file
    ##   1  -- 5 columns 2 columns <connection>

``` r
# Save it to the pressure data folder
# write.csv(pcdd, file.path(dir_rawdata,"pcdd2018.csv"), row.names = FALSE)

basin_lookup <- file.path(dir_prep, "supplement", "lookup_tabs", "rgns_complete.csv") %>% 
  read_csv() %>%
  dplyr::select(bhi_id = region_id, subbasin, rgn_nam = region_name, HELCOM_ID = helcom_id) %>%
  mutate(
    subbasin = as.character(subbasin),
    rgn_nam = as.character(rgn_nam)
  )

# Create lookup table to match the basins to the holas (sub)basins
basins <- bind_rows(
  c(basin = "Archipelago Sea", subbasin = "Aland Sea"),
  c(basin = "Baltic Proper", subbasin = "Arkona Basin"),
  c(basin = "Baltic Proper", subbasin = "Bornholm Basin"),
  c(basin = "Baltic Proper", subbasin = "Eastern Gotland Basin"),
  c(basin = "Baltic Proper", subbasin = "Gdansk Basin"),
  c(basin = "Baltic Proper", subbasin = "Northern Baltic Proper"),
  c(basin = "Baltic Proper", subbasin = "Western Gotland Basin"),
  c(basin = "Bothnian Bay", subbasin = "Bothnian Bay"),
  c(basin = "Bothnian Sea", subbasin = "Bothnian Sea"),
  c(basin = "Bothnian Sea", subbasin = "The Quark"),
  c(basin = "Gulf of Finland", subbasin = "Gulf of Finland"),
  c(basin = "Gulf of Riga", subbasin = "Gulf of Riga"),
  c(basin = "Kattegat", subbasin = "Kattegat"),
  c(basin = "The Sound", subbasin = "The Sound"),
  c(basin = "Western Baltic", subbasin = "Bay of Mecklenburg"),
  c(basin = "Western Baltic", subbasin = "Great Belt"),
  c(basin = "Western Baltic", subbasin = "Kiel Bay"))
```

#### 4.1.2 Transform data to long format

``` r
library(tidyr)
pcb153_long <- pcb153 %>%
  gather(subbasin_abb, value, -X) %>%
  rename(year = X) %>% 
  mutate(unit = "kg/year",
         basin = ifelse(subbasin_abb == "ARC", "Archipelago Sea",
                        ifelse(subbasin_abb == "BOB", "Bothnian Bay",
                               ifelse(subbasin_abb == "BOS", "Bothnian Sea",
                                      ifelse(subbasin_abb == "BAP", "Baltic Proper",
                                             ifelse(subbasin_abb == "GUF", "Gulf of Finland",
                                                    ifelse(subbasin_abb == "GUR", "Gulf of Riga",
                                                           ifelse(subbasin_abb == "KAT", "Kattegat",
                                                                  ifelse(subbasin_abb == "SOU", "The Sound",
                                                                         ifelse(subbasin_abb == "WEB", "Western Baltic",
                                                                                       NA))))))))),
         year = as.numeric(year),
         value = str_replace(value, ",", "."),
         value = as.numeric(value)) %>% 
  # filter out the whole Baltic Sea (BAS) and normalized deposition to the Baltic Sea (Norm) values
         filter(subbasin_abb != "BAS", subbasin_abb != "Norm")
```

#### 4.1.3 Join to HOLAS basin names

Data are joined to HOLAS basin names because these are what are
associated with BHI regions. This is a slightly indirect approach, could
have made a look up table to associated the major basins from the
deposition data directly with the BHI regions, but this should have the
same result.

``` r
# Join the basins to pcb
pcb153_basins <- pcb153_long %>%
  full_join(., basins, by = "basin") 

## Join the rgns shps to pcdd
pcdd_rgns <- join_rgns_info(
  pcdd, helcomID_col = "helcom_id", country_col = "country", 
  latlon_vars = c("^lat", "^lon"), return_spatial = FALSE, 
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefiles"), 
  buffer_shp = NULL
) 
```

    ## Warning: Unknown columns: `country`, `helcom_id`

    ## Warning: Unknown or uninitialised column: `zn1`.
    
    ## Warning: Unknown or uninitialised column: `zn1`.

    ## Warning: Unknown or uninitialised column: `yr`.
    
    ## Warning: Unknown or uninitialised column: `yr`.

``` r
# Check data points where no BHI_id can be assigned
pcdd_rgns_bhi <- pcdd_rgns %>% 
  filter(!(is.na(BHI_ID))) %>% 
  mutate(station = as.character(station))
```

#### 4.1.4 Plot data

``` r
# Plot data by major basin provided
ggplot(pcb153_basins) +
  geom_point(aes(year, value)) +
  facet_wrap(~ basin)+
  ylab("PCB 153 kg/year")+
  theme(axis.text.x = element_text(colour="grey20", size=8, angle=90, 
                                  hjust=.5, vjust=.5, face = "plain"),
          strip.text.x = element_text(size = 6))+
  ggtitle("PCB153 Atmospheric Deposition by Major Basin")
```

![](atmos_con_prep_files/figure-gfm/plot%20raw%20data-1.png)<!-- -->

``` r
# Plot data by subbasin
ggplot(pcb153_basins) +
  geom_point(aes(year, value)) +
  facet_wrap(~ subbasin)+
  ylab("PCB 153 kg/year")+
  theme(axis.text.x = element_text(colour="grey20", size=8, angle=90, 
                                  hjust=.5, vjust=.5, face = "plain"),
          strip.text.x = element_text(size = 6))+
  ggtitle("PCB153 Atmospheric Deposition by Subbasin")
```

![](atmos_con_prep_files/figure-gfm/plot%20raw%20data-2.png)<!-- -->

``` r
## make basemap
basemap <- ggplot2::ggplot(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) + 
  geom_sf(size = 0.1, color = "burlywood", alpha = 0.4) +
  scale_x_continuous(limit = c(4, 32)) +
  scale_y_continuous(limit = c(53.5, 66)) +
  theme(panel.background = element_rect(fill = "#F8FBFC", color = "#E2EEF3"))

bhi_rgns_simple <- rmapshaper::ms_simplify(input = BHI_rgns_shp) %>% sf::st_as_sf()

basemap +
  geom_sf(data = bhi_rgns_simple, fill = "white", size = 0.2, color = "burlywood") +
  geom_sf(
    mapping = aes(colour = station),
    data = sf::st_as_sf(
      pcdd_rgns_bhi, 
      coords = c("lon", "lat"), 
      crs = 4326
    ),
    size = 0.2
  ) +
  labs(x = NULL, y = NULL, colour = "Stations") +
  ggtitle("PCDD Sampling Stations")
```

![](atmos_con_prep_files/figure-gfm/plot%20raw%20data-3.png)<!-- -->

### 4.2 Current value

#### 4.2.1 Get most recent year for current value

``` r
max_yr_pcb153 <- pcb153_basins %>%
  select(year)%>%
  max()%>%
  as.numeric() # 2016
```

#### 4.2.2 Get current pressure data by Subbasins

``` r
current_pcb153 <- pcb153_basins %>%
  filter(year == max_yr_pcb153) %>%
  select(subbasin, value, unit, year)

current_pcdd <- pcdd_rgns_bhi %>%
  group_by(BHI_ID) %>% 
  summarize(value = mean(value, na.rm = TRUE),
            Subbasin = unique(Subbasin),
            unit = unique(unit)) %>% 
  ungroup() %>% 
  mutate(year = 2018) %>% 
  select(BHI_ID, Subbasin, value, unit, year)
```

#### 4.2.3 Plot current year data

``` r
ggplot(current_pcb153)+
  geom_point(aes(subbasin, value), size = 2.5)+
  ylab("PCB 153 kg/year")+
  ylim(0,15)+
   theme(axis.text.x = element_text(colour="grey20", size=8, angle=90, 
                                  hjust=.5, vjust=.5, face = "plain"),
          strip.text.x = element_text(size = 6))+
  ggtitle("PCB 153 Atmospheric Deposition in 2016")
```

![](atmos_con_prep_files/figure-gfm/plot%20current%20year%20data-1.png)<!-- -->

``` r
# Plot data by subbasin
ggplot(current_pcdd) +
  geom_col(aes(BHI_ID, value, fill = Subbasin)) +
  ylab("PCDD ug TEQ/km2/year")+
  theme(axis.text.x = element_text(colour="grey20", size=8, angle=90, 
                                  hjust=.5, vjust=.5, face = "plain"),
          strip.text.x = element_text(size = 6))+
  ggtitle("PCDD Atmospheric Deposition by Subbasin 2018")
```

![](atmos_con_prep_files/figure-gfm/plot%20current%20year%20data-2.png)<!-- -->

``` r
#ggplot(current_pcdd)+
#  geom_point(aes(BHI_ID, value), size = 2.5)+
#  ylab("PCDD ug TEQ/km2/year")+
#   theme(axis.text.x = element_text(colour="grey20", size=8, angle=90, 
#                                  hjust=.5, vjust=.5, face = "plain"),
#          strip.text.x = element_text(size = 6))+
#  ggtitle("PCDD Atmospheric Deposition in 2018")
```

### 4.3 Rescale data

#### 4.3.1 Maximum value

Select maximum value across all basins within each time series. PCB 153:
Baltic Proper in 1990, 40.246 kg/year

``` r
max_pcb153 <- pcb153_basins %>%
  select(value)%>%
  max()%>%
  as.numeric()
max_pcb153
```

    ## [1] 40.246

``` r
## which year and basin 
pcb153_basins %>% filter(value == max_pcb153) %>% select(basin, year, unit, value) %>% distinct()
```

    ##           basin year    unit  value
    ## 1 Baltic Proper 1990 kg/year 40.246

``` r
max_pcdd <- pcdd_rgns_bhi %>%
  select(value)%>%
  max()%>%
  as.numeric() 

## which basin 
pcdd_rgns_bhi %>% filter(value == max_pcdd) %>% select(Subbasin, unit, value) %>% distinct()
```

    ## # A tibble: 1 x 3
    ##   Subbasin        unit             value
    ##   <chr>           <chr>            <dbl>
    ## 1 Gulf of Finland ug TEQ/km2/year 34727.

``` r
# Gulf of Finland: 34727.4
```

#### 4.3.2 Minimum value

Minimum value is 0, the value at which there is no pressure

``` r
min_pcb153 = 0

min_pcdd = 0
```

#### 4.3.3 Join current, max, and min values

``` r
pcb153_rescale <- current_pcb153 %>%
  mutate(min = min_pcb153, 
         max= max_pcb153) %>% 
  dplyr:: rename(current = value)


pcdd_rescale <- current_pcdd %>%
  mutate(min = min_pcdd, 
         max= max_pcdd) %>% 
  dplyr:: rename(current = value)
```

#### 4.3.4 Plot current, max and min

``` r
ggplot(gather(pcb153_rescale, type, concentration, -subbasin, -unit, -year)) +
  geom_point(aes(subbasin, concentration, colour = type, shape = type), size= 2.5)+
  ylab("PCB 153 kg/year") +
   theme(axis.text.x = element_text(colour="grey20", size=8, angle=90, 
                                hjust=.5, vjust=.5, face = "plain"))+
  ggtitle("PCB 153 minimum, maximum, and current pressure value")
```

![](atmos_con_prep_files/figure-gfm/plot%20current%20max%20and%20min-1.png)<!-- -->

``` r
ggplot(gather(pcdd_rescale, type, concentration, -BHI_ID, -Subbasin, -unit, -year)) +
  geom_point(aes(Subbasin, concentration, colour = type, shape = type), size= 2.5)+
  ylab("23PeCDF ug TEQ/km2/year") +
   theme(axis.text.x = element_text(colour="grey20", size=8, angle=90, 
                                hjust=.5, vjust=.5, face = "plain"))+
  ggtitle("PCDD minimum, maximum, and current pressure value")
```

![](atmos_con_prep_files/figure-gfm/plot%20current%20max%20and%20min-2.png)<!-- -->

#### 4.3.5 Rescale data

``` r
pcb153_rescale1 <- pcb153_rescale %>%
  mutate(pcb153_normalize = (current - min)/(max - min))

pcdd_rescale1 <- pcdd_rescale %>%
  mutate(pcdd_normalize = (current - min)/(max - min))
```

#### 4.3.6 Plot rescaled pressure layer

``` r
ggplot(pcb153_rescale1) +
  geom_point(aes(subbasin, pcb153_normalize), size= 2.5)+
  ylab("Pressure Value") +
  ylim(0,1)+
   theme(axis.text.x = element_text(colour="grey20", size=8, angle=90, 
                                hjust=.5, vjust=.5, face = "plain"))+
  ggtitle("PCB 153 pressure value")
```

![](atmos_con_prep_files/figure-gfm/plot%20rescales%20pressure%20layer-1.png)<!-- -->

### 4.4 Pressure layer for BHI regions

#### 4.4.1 Apply basin values to BHI regions

``` r
pcb153_rgn <- pcb153_rescale1 %>%
  full_join(., basin_lookup, by = "subbasin") %>%
  select(bhi_id, pcb153_normalize) %>%
  arrange(bhi_id)
```

#### 4.4.2 Plot Pressure layer by BHI regions

``` r
ggplot(pcb153_rgn)+
  geom_point(aes(bhi_id, pcb153_normalize), size= 2.5)+
  ylab("Pressure Value")+
  ylim(0,1)+
   theme(axis.text.x = element_text(colour="grey20", size=8, angle=90, 
                                hjust=.5, vjust=.5, face = "plain"))+
  ggtitle("PCB 153 pressure value by BHI regions")
```

![](atmos_con_prep_files/figure-gfm/Plot%20Pressure%20layer%20by%20BHI%20regions-1.png)<!-- -->

``` r
ggplot(pcdd_rescale1) +
  geom_point(aes(BHI_ID, pcdd_normalize), size= 2.5)+
  ylab("Pressure Value") +
  ylim(0,1)+
   theme(axis.text.x = element_text(colour="grey20", size=8, angle=90, 
                                hjust=.5, vjust=.5, face = "plain"))+
  ggtitle("PCDD pressure value")
```

![](atmos_con_prep_files/figure-gfm/Plot%20Pressure%20layer%20by%20BHI%20regions-2.png)<!-- -->

### 4.5 Prepare and save object for layers

#### 4.5.1 Prepare objects

``` r
po_atmos_pcb153 <- pcb153_rgn %>%
  dplyr::rename(pressure_score = pcb153_normalize)

po_atmos_pcdd <- pcdd_rescale1 %>%
  dplyr::rename(pressure_score = pcdd_normalize) %>% 
  select(bhi_id = BHI_ID, pressure_score)
```

#### 4.5.2 Save objects to layers

``` r
write.csv(po_atmos_pcb153, file.path(dir_layers,"po_atmos_pcb153_bhi2019.csv"), row.names = FALSE)

write.csv(po_atmos_pcdd, file.path(dir_layers,"po_atmos_pcdd_bhi2019.csv"), row.names = FALSE)
```

## 5\. Considerations for `BHI3.0`

## 6\. References

[Atmospheric deposition of PCB-153 on the Baltic
Sea](https://helcom.fi/wp-content/uploads/2020/07/H_BSEFS_PCB153_dep_v2.pdf)

[Atmospheric deposition of PCDD/Fs on the Baltic
Sea](https://helcom.fi/wp-content/uploads/2020/07/AIR-Deposition-PCDDF-1990-2015.pdf)

Bartnicki J., Gusev A., Aas W., Gauss M., and J. E. Jonson (2017)
Atmospheric supply of nitrogen, cadmium, mercury, lead, and PCDD/Fs to
the Baltic Sea in 2015. EMEP Centres Joint Report for HELCOM. EMEP/MSC-
W Technical Report 1/2017. Norwegian Meteorological Institute. Oslo,
Norway. Available in the web:
<http://www.emep.int/publ/helcom/2017/index.html>

Breivik K., Sweetman A., Pacyna J.M., Jones K.C. \[2007\] Towards a
global historical emission inventory for selected PCB congeners - A mass
balance approach-3. An update. Science of the Total Environment,
vol. 377, pp. 296-307.

Denier van der Gon H.A.C., van het Bolscher M., Visschedijk A.J.H. and
P.Y.J.Zandveld \[2005\]. Study to the effectiveness of the UNECE
Persistent Organic Pollutants Protocol and costs of possible additional
measures. Phase I: Estimation of emission reduction resulting from the
implementation of the POP Protocol. TNO-report B\&O-A R 2005/194.

Gusev A., I. Ilyin, L.Mantseva, O.Rozovskaya, V. Shatalov, O. Travnikov
\[2006\] Progress in further development of MSCE-HM and MSCE-POP models
(implementation of the model review recommendations. EMEP/MSC-E
Technical Report 4/2006. (<http://www.msceast.org/reports/4_2006.pdf>)

Gusev A., E. Mantseva, V. Shatalov, B.Strukov \[2005\] Regional
multicompartment model MSCE-POP EMEP/MSC-E Technical Report 5/2005.
(<http://www.msceast.org/reports/5_2005.pdf>)

Shatalov V., Gusev A., Dutchak S., Holoubek I., Mantseva E., Rozovskaya
O., Sweetman A., Strukov B. and N.Vulykh \[2005\] Modelling of POP
Contamination in European Region: Evaluation of the Model Performance.
Technical Report 7/2005. (<http://www.msceast.org/reports/7_2005.pdf>)

Shatalov V., Ilyin I., Gusev A., Rozovskaya O., Sokovykh V., Travnikov
O., Wiberg K. and Cousins I. \[2012\] Heavy Metals and Persistent
Organic Pollutants : New developments. EMEP/MSC-E Technical report
4/2012. (<http://www.msceast.org/reports/4_2012.pdf>))
