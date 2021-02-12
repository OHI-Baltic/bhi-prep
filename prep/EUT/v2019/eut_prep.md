Clean Water (CW) - Eutrophication (EUT) Subgoal Data Preparation
================

<br>

## 1\. Background

### 1.1 Glossary

**Baltic Health Index (BHI)**: The BHI is a regional study applying the
**OHI framework**. The aim is to provide an integrative and transparent
tool to be used by decision-makers to guide management of the Baltic Sea
towards increased sustainability. The Baltic’s Ocean Health Index
assessments score 9 goals, some of which have sub-goals. **Goal scores**
are calculated by modeling data first as Present Status and Likely
Future State. Likely Future State is a combination of status, trend,
pressures and resilience calculations, which contribute to goal scores
according to specified ratios.

**BHI regions**: Scores for are calculated for each region individually
and then combined (with an offshore area-weighted average) to produce
scores for the entire Baltic Sea. There are 42 BHI regions in the Baltic
Sea, created by intersecting exclusive economnic zones (EEZs) with the
Baltic Sea subbasins.

**BHI Sub-basins**: In the BHI calculations, **sub-basins** refer to the
HELCOM sub-basins, including open-sea and coastal areas unless otherwise
indicated ([HELCOM
Mapservice](http://maps.helcom.fi/website/mapservice/): Monitoring -\>
Assessment Units -\> HELCOM subbasins 2018).

**Status**: Present Status is the sub-goal’s current value compared to
the reference point. In many cases, status is initially calculated as a
value between 0 and 1, and later rescaled to the 0 to 100 range via a
simple multiplication by 100. A status of 100 means that the current
status is equal to the target reference point (not pristine, unless that
is the agreed upon target\!), and 0 means that it is as far from the
target reference point as *theoretically* possible, or a specified lower
cut-off point (e.g. secchi depth at zero in the eutrophication subgoal,
or 75% of species extinct for biodiversity goal).

**Trend**: Though these systems have a lot of annual variation,
short-term trends (5 years, or 10 years for slow-change variables) are
calculated to capture tangential directionality of state over recent
years. This dimension is included as a way to reflect how it is not just
the immediate state, but the tendancy in the system towards or away from
‘good status’ that reflects the ‘health’ of the marine system.

### 1.2 Goal Description

The Eutrophication sub-goal of the Clean Water goal captures the degree
to which local waters are affected by eutrophication. For the BHI, five
indicators were included:

  - Winter (December-February) dissolved inorganic nitrogen (DIN)
    concentrations in the surface layer (0 - 10 m depth)
  - Winter (December-February) dissolved inorganic phosphorus (DIP)
    concentrations in the surface layer (0 - 10 m depth)
  - Summer (June-September) chlorophyll a concentration in the surface
    layer (0 - 10 m depth)  
  - Summer (June-September) Secchi depth  
  - Oxygen debt indicator

These five indicators are HELCOM core indicators (Baltic Sea
Environmental Proceedings No 143). Oxygen debt is calculated using the
ICES HEAT assessment tool, [in a separate
repository](https://github.com/OHI-Baltic/HEAT).

<br/>

### 1.3 Model & Data

The geometric means of the status of all five indicators have been
calculated for Bornholm Basin, Western Gotland Basin, Eastern Gotland
Basin, Northern Baltic Proper and Gulf of Finland. For the other basins
only four indicators (without oxygen debt) have been used to calculate
the eutrophication status. Moreover, **only offshore data were used for
all the core indicators**, due to insufficient data and/or a threshold
value associated to coastal areas.

  - Mean Summer Secchi depth (June-September) data from ICES and SMHI
    databases.
  - Winter (DIN), (DIP), and summer chlorophyll a data were downloaded
    from the Baltic Nest Institute database (DAS and NEST); additional
    chlorophyll a data come from ICES and SMHI.
  - Oxygen debt calculations are based on oxygen data from ICES (CTD and
    bottle data), and follow the approach introduced in HELCOM Indicator
    Sheets (2018) where the oxygen debt below halocline is assessed and
    general additive models are applied to address temporal, seasonal
    and spatial variation within the basins (more information available
    at the HELCOM 2013 (BSEP133)).

#### 1.3.1 Method Overview

  - Regarding the status calculation, we calculate a five-year moving
    average for each year in the dataset only for three core indicators
    (Secchi depth, DIN and DIP) and an annual average for the other two
    core indicators (Chlorophyll a and Oxygen debt) due to gaps in the
    data.

  - Status is calculated on the basin level and then applied to all
    regions, for each year between 2000 and 2019. Status for the each
    indicator is calculated as basin mean divided by basin target.

  - Regarding the trend calculation, as all core indicators are slow
    response variables, we use a 10-year period to calculate the trend
    with a minimum of 5 data points. The trend value is the slope of the
    linear regression (mean value vs year) multiplied by the year of
    future interest (5 years from status year) and this value is
    constrained to between -1 and 1.

<br/>

### 1.4 Reference points

The reference points for the eutrophication subgoal are set following
the thresholds set by HELCOM (2018) and agreed on by the HELCOM Heads of
Delegation (HOD): [Approaches and methods for eutrophication target
setting in the Baltic Sea
region](https://helcom.fi/media/documents/Eutorophication-targets_BSEP133.pdf)

<br/>

#### 1.4.1 Secchi depth Indicator

*Secchi depth* reference points are set according to the HELCOM
thresholds (HELCOM 2018). They are based on results obtained in the
TARGREV project (HELCOM 2013a), taking advantage of the work carried out
during the EUTRO PRO project (HELCOM 2009) and national work for WFD.
The final targets were set through an expert evaluation process done by
the intersessional activity on development of core eutrophication
indicators (HELCOM CORE EUTRO) and the targets were adopted by the
HELCOM Heads of Delegations. More information:

  - [Fleming-Lehtinen and
    Laamanen. 2012](https://doi.org/10.1016/j.ecss.2012.02.015).
    Long-term changes in Secchi depth and the role of phytoplankton in
    explaining light attenuation in the Baltic Sea. Estuarine, Coastal,
    and Shelf Science 102-103:1-10
  - [EUTRO-OPER](https://helcom.fi/helcom-at-work/projects/eutro-oper/).
    Included on this page is a link to the Eutrophication Assessment
    Manual.

<br/>

#### 1.4.2 Dissolved Inorganic Nitrogen and Phosphorus, and Chlorophyll a

HELCOM sub-basin specific reference points for the *winter (DIN), (DIP),
and summer chlorophyll* a are set according to the HELCOM thresholds
(HELCOM 2018).

<br/>

#### 1.4.3 Oxygen Debt Indicator

The *oxygen debt*, i.e., “missing” oxygen in relation to fully
oxygenated water column, calculations follow the approach introduced in
HELCOM Indicator Sheets (2018) and the same threshold values (the 95th
percentiles before 1940, detected using change point analysis) are used.
There are only two distinct reference points, set for the Baltic Proper
(including Gdansk Basin, Eastern and Western Gotland Basins, Northern
Baltic Proper, and Gulf of Finland) and for Bornholm Basin. These
targets were also set according to HELCOM, as described in [Baltic Sea
Environmental Proceedings
No 143](https://www.helcom.fi/wp-content/uploads/2019/08/BSEP143.pdf9).

<br/>

### 1.4 Other information

External advisors/goalkeepers: Vivi Fleming-Lehtinen.

<br/>

<br>

## 2\. Data

This prep document is used to generate and explore the following data
layers:

  - `cw_eut_secchi_bhi2019.csv`
  - `cw_eut_chla_bhi2019.csv`
  - `cw_eut_din_bhi2019.csv`
  - `cw_eut_dip_bhi2019.csv`
  - `cw_eut_oxydebt_bhi2019.csv`

These are saved to the `layers` folder. Saved to
`data/CW/eutrophication/v2019/intermediate` are intermediate datasets:
`secchi_merged_rawdata.csv`,`chla_merged_rawdata.csv`,
`ctd_merged_rawdata.csv`, and `ox_merged_rawdata.csv`. All these are
derived from or informed by the raw datasets from ICES, SMHI and Baltic
Nest database.

<br>

### 2.1 Datasets with Sources

<br/>

#### 2.1.1 Secchi Depth

**ICES Secchi Depth Data**

Extraction from ICES database follows HELCOM Assessment Units – HELCOM
sub-basins with coastal WFD water bodies or water
types.

<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/SecchiDepth_ICES -->

| Option           | Specification                                                       |
| :--------------- | :------------------------------------------------------------------ |
| Dataset set:     | Oceanographic (per Parameter)                                       |
| Parameter:       | secchi depth (Download)                                             |
| \*must request a | ll records as web download limited to max 10000 records per layer\* |

Source: [ICES data
portal](http://ecosystemdata.ices.dk/inventory/Index.aspx?) <br/>
Requested 24 Feb 2020 by Andrea De Cervo and received 24 Feb 2020 from
Else Juul Green

<br/>

**SMHI Secchi Depth
Data**  
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/SecchiDepth_SMHI -->

| Option                                       | Specification                      |
| :------------------------------------------- | :--------------------------------- |
| Datatype:                                    | Physical and Chemical              |
| Parameter:                                   | secchi depth                       |
| Months:                                      | All                                |
| Years:                                       | 1893-2020                          |
| Decimal/fält-avgränsare (decimal/delimiter): | punkt/semikolon (period/semicolon) |
| Teckenkodning:                               | UTF-8                              |
| Rubrikrad:                                   | Englelska (English)                |

Source: [SMHI Shark
database](http://www.smhi.se/klimatdata/oceanografi/havsmiljodata/marina-miljoovervakningsdata)
(<https://sharkweb.smhi.se>) <br/> Downloaded 6 March 2020 by Andrea De
Cervo

<br/>

#### 2.1.2 Oxygen Debt

**ICES Oxygen Data**

Each dataset has been extracted by country and Denmark split into five
(5) due to large file
size.

<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/OxygenDebt_ICES -->

| Option     | Specification                          |
| :--------- | :------------------------------------- |
| Period:    | 2000-2020                              |
| Parameter: | Oxygen                                 |
| Country:   | (choose each Baltic country at a time) |
| Ship:      | Any                                    |

Source: [ICES
database](https://ocean.ices.dk/HydChem/HydChem.aspx?plot=yes) <br/>
Downloaded 30 March 2020 by Andrea De Cervo. Note: the maximum number of
stations per download is 20000.

<br/>

**ICES CTD Data**

Each dataset has been extracted by country, Denmark split into three (3)
and Germany into five (5) due to large file size. No CTD records found
for
Lithuania.

<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/OxygenDebt_ICES -->

| Option     | Specification                          |
| :--------- | :------------------------------------- |
| Period:    | 2000-2020                              |
| Parameter: | CTD                                    |
| Country:   | (choose each Baltic country at a time) |
| Ship:      | Any                                    |

Source: [ICES
database](https://ocean.ices.dk/HydChem/HydChem.aspx?plot=yes) <br/>
Downloaded 10 April 2020 by Andrea De Cervo. Note: the maximum number of
stations per download is 20000.

<br/>

**ICES Hydrogen Sulphide**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/OxygenDebt_ICES -->

| Option     | Specification                          |
| :--------- | :------------------------------------- |
| Period:    | 2000-2020                              |
| Parameter: | Hydrogen Sulphide                      |
| Country:   | (choose each Baltic country at a time) |
| Ship:      | Any                                    |

Source: [ICES
database](https://ocean.ices.dk/HydChem/HydChem.aspx?plot=yes) <br/>
Downloaded 10 April 2020 by Andrea De Cervo. Note: the maximum number of
stations per download is 20000.

<br/>

**SMHI CTD Data**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/OxygenDebt_ICES -->

| Option                                       | Specification                      |
| :------------------------------------------- | :--------------------------------- |
| Datatype:                                    | Physical and Chemical              |
| Parameter:                                   | Dissolved oxygen O2 CTD            |
| Months:                                      | All                                |
| Years:                                       | 2000-2020                          |
| Decimal/fält-avgränsare (decimal/delimiter): | punkt/semikolon (period/semicolon) |
| Teckenkodning:                               | UTF-8                              |
| Rubrikrad:                                   | Englelska (English)                |

Source: [SMHI database](https://sharkweb.smhi.se/) <br/> Downloaded 16
April 2020 by Andrea De Cervo

<br/>

#### 2.1.3 Chlorophyll a, and Nutrients (DIN and DIP)

**Baltic Nest Datasets**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/NestData/nestdata_winter_nutrients.csv -->
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/NestData/nestdata_summer_chlorophyl.csv -->

| Option                        | Specification                      |
| :---------------------------- | :--------------------------------- |
| Latitude/Longitude Begin/End: | Baltic Sea                         |
| Dates Begin/End:              | Jan 1, 2005 to Dec 31, 2019        |
| Parameters:                   | PO4P, NO3N, NO2N, NO23N, NH4N, CHL |

Source: [Baltic Nest database](http://nest.su.se/dataPortal/getStations)
<br/> Downloaded 28 February 2020 by Ellie Campbell. See
data.R/get\_nest\_data function for more detailed info. [About
units](http://nest.su.se/bed/chem_struct.shtml).

``` r
## Retrieving Baltic Nest Nutrients datasets:
get_nest_data(
  date_range = c(20050101,20191231), 
  months = c(11,12,1:3), 
  param_codes = c("NO3N","NO2N","NO23N","NH4N","TOTN","PO4P")
) %>% readr::write_csv(file.path(dir_B, "Goals", "CW", "EUT", "NestData", "nestdata_winter_nutrients.csv"))
```

``` r
## Retrieve Baltic Nest Chlorophyll a datasets:
get_nest_data(
  date_range = c(20050101, 20191231), 
  months = 5:10, 
  param_codes = c("CHL")
) %>% readr::write_csv(file.path(dir_B, "Goals", "CW", "EUT", "NestData", "nestdata_summer_chlorophyl.csv"))
```

<br/>

**Chlorophyll a Data from SMHI**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/OxygenDebt_ICES -->

| Option                                       | Specification                      |
| :------------------------------------------- | :--------------------------------- |
| Datatype:                                    | Physical and Chemical              |
| Parameter:                                   | Chlorophyll-a bottle               |
| Months:                                      | All                                |
| Years:                                       | 2000-2020                          |
| Decimal/fält-avgränsare (decimal/delimiter): | punkt/semikolon (period/semicolon) |
| Teckenkodning:                               | UTF-8                              |
| Rubrikrad:                                   | Engelska (English)                 |
| <br/>                                        |                                    |

Source: [SMHI Shark
database](http://www.smhi.se/klimatdata/oceanografi/havsmiljodata/marina-miljoovervakningsdata)
(<https://sharkweb.smhi.se>) <br/> Downloaded 24 April 2020 by Andrea De
Cervo

**Chlorophyll a Data from ICES**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/EUT/Chla_ICES -->

| Option     | Specification                          |
| :--------- | :------------------------------------- |
| Period:    | 2000-2020                              |
| Parameter: | Chlorophyll.a                          |
| Country:   | (choose each Baltic country at a time) |
| Ship:      | Any                                    |

Source: [ICES
database](https://ocean.ices.dk/HydChem/HydChem.aspx?plot=yes) <br/>
Downloaded 30 April 2020 by Andrea De Cervo. Note: the maximum number of
stations per download is 20000

<br/>

#### 2.1.4 HELCOM HOLAS Sub-basins

These sub-basins are the relevant physical units for the analyses.

Secchi data will be first assessed at the level of HELCOM subbasin and
then assigned to specific BHI regions. The dataset `HELCOM Subbasins
with coastal and offshore division 2018` will be used for Secchi as well
as Chlorophyll a, DIN and DIP, to assign ‘coastal’ and ‘offshore’
locations.

| Option                       | Specification                                            |
| :--------------------------- | :------------------------------------------------------- |
| HELCOM Map and Data service: | Monitoring                                               |
| Assessment units:            | HELCOM Subbasins with coastal and offshore division 2018 |

Source:
[HELCOM](http://metadata.helcom.fi/geonetwork/srv/eng/catalog.search#/metadata/e5a59af9-c244-4069-9752-be3acc5dabed)
<br/> Downloaded 23 April 2020 by Andrea De Cervo

<br/>

In addition to this, the dataset `HELCOM subbasins with coastal WFD
water bodies or water types 2018` will be used for Chlorophyll a, to
assign ‘coastal’
locations.

| Option                       | Specification                                                      |
| :--------------------------- | :----------------------------------------------------------------- |
| HELCOM Map and Data service: | Monitoring                                                         |
| Assessment units:            | HELCOM subbasins with coastal WFD water bodies or water types 2018 |

Source:
[HELCOM](http://metadata.helcom.fi/geonetwork/srv/eng/catalog.search#/metadata/67d653b1-aad1-4af4-920e-0683af3c4a48)
<br/> Downloaded 8 June 2020 by Andrea De Cervo

<br/>

#### 2.1.5 Reference Values

##### 2.1.5.1 Offshore reference values

The threshold values have been agreed on by HELCOM and by Heads of
Delegation (HOD) and the applied threshold values for core and pre-core
indicators in the HELCOM open sea assessment units are presented in the
[Baltic Sea Environment Proceedings
no.156](http://stateofthebalticsea.helcom.fi/wp-content/uploads/2019/09/BSEP156-Eutrophication.pdf)
(p.10, Table 2).

``` r
## eutrophication thresholds data
## Note: this table exists in its original form on the bhi internal shared server 
## read.delim(file.path(dir_rawdata, "eut_threshold_values.csv"), sep = ";")
## obtained originally from (?)
## with columns:  
## basin = str_trim(str_replace_all(Assessment.units,"_",""), side = "right"),
## oxyg_debt = as.numeric(str_replace(O2..mg.l.1., "N", " ")),
## winter_DIN = DIN.._mol.l.1., winter_DIP = DIP.._mol.l.1.,
## summer_chla = Chla.._g.l.1., summer_secchi = Water.clarity..m. 

eut_thresholds <- read_csv(file.path(dir_prep, "prep", "CW", "eutrophication", "eut_threshold_values.csv"))
```

<br/>

##### 2.1.5.2 Coastal Chlorophyll a Reference Values

The coastal threshold values for Chlorophyll a are presented in the
[HELCOM Chlorophyll a core
indicator](https://helcom.fi/media/core%20indicators/Chlorophyll-a-HELCOM-core-indicator-2018.pdf)
(p.5-10, Results Table
2).

``` r
chla_coastal_thresholds <- read_csv(file.path(dir_rawdata, "chla_coastal_thresholds.csv"))
```

<br/>

##### 2.1.5.3 Coastal DIN Reference Values

The coastal threshold values for DIN are presented in the [HELCOM DIN
core
indicator](https://helcom.fi/media/core%20indicators/Dissolved-inorganic-nitrogen-DIN-HELCOM-core-indicator-2018.pdf)
(p.7-8, Results Table
2).

``` r
din_coastal_thresholds <- read_csv(file.path(dir_rawdata, "din_coastal_thresholds.csv"))
```

<br/>

##### 2.1.5.4 Coastal DIP Reference Values

The coastal threshold values for DIP are presented in the [HELCOM DIP
core
indicator](https://helcom.fi/media/core%20indicators/Dissolved-inorganic-phosphorus-DIP-HELCOM-core-indicator-2018.pdf)
(p.6-7, Results Table
2).

``` r
dip_coastal_thresholds <- read_csv(file.path(dir_rawdata, "dip_coastal_thresholds.csv"))
```

<br/>

### 2.2 Centralization & Normalization

#### 2.2.1 Secchi Datasets

``` r
## read in secchi data
secchi_raw_ices <- read.delim(
  file.path(dir_rawdata, "SecchiDepth_ICES", "SecchiDepth_ICES.csv"), 
  sep = ";",
  stringsAsFactors = FALSE
)
secchi_raw_smhi <- read.delim(
  file.path(dir_rawdata, "SecchiDepth_SMHI", "SecchiDepth_SMHI.csv"), 
  sep = ";",
  stringsAsFactors = FALSE
)
basin_add_lookup_table <- read.csv(file.path(dir_prep, "supplement", "lookup_tabs", "basin_add_lookup_table.csv"))
```

**Coastal data**

  - Non-coastal (offshore) data are flagged with the code “0” under the
    column *“HELCOM\_COASTAL\_CODE”*  
  - HELCOM basin shape files with coastal and non-coastal areas were
    overlaid with the secchi sampling locations, all locations were
    flagged with a code indicating coastal or offshore.  
  - Coastal data are later removed from the analysis

**Steps in Merging ICES and SMHI Secchi Datasets**

1)  Rename variables and subset datasets to be merged, so column names
    and data types align
2)  Check duplicates and select only distinct observations
3)  Combine SMHI and ICES data, identifying and eliminating overlaps

<!-- end list -->

``` r
## ICES Secchi Data

## overview
# colnames(secchi_raw_ices)
# str(secchi_raw_ices)

## Merge the add basin lookup table to ices dataset
ices_secchi <- secchi_raw_ices %>% 
  rename(
    secchi = Secchi.Depth..m.,
    latitude = Latitude..degrees_north.,
    longitude = Longitude..degrees_east., 
    bottom_m = Bot..Depth..m.
  ) %>%
  mutate(
    date =  as.Date(yyyy.mm.ddThh.mm), 
    year = format(date, "%Y"), 
    month = format(date, "%m"),
    month = str_replace(month, "^0+", ""),
    cruise = as.character(Cruise),
    station = as.character(Station),
    bottom_m = as.numeric(bottom_m)
  ) %>%
  select(-Type, -yyyy.mm.ddThh.mm) 

## Look for Duplicates in the Data:
## check for duplicates in ices
ices_duplicates <- ices_secchi %>%
  group_by(
    cruise, station, date, year, month, 
    latitude, longitude, 
    secchi, bottom_m
  ) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup() 

## Only extract unique observations
secchi_ices_unique <- ices_secchi %>% 
  distinct(
    cruise, station, date, year, month, 
    latitude, longitude, 
    secchi, bottom_m
  ) %>% 
  ## removed 'Assessment_unit'
  mutate(supplier = "ices") # nrow(ices_unique) 102881
```

``` r
## SMHI Secchi Data 

## overview
# colnames(secchi_raw_smhi)
# str(secchi_raw_smhi)

## ADJUST ROWS
## three extra columns starting with 'X' indicate some weird column shifts

## correct where Sample depths columns have been shifted one or two columns over 
depth_measurement <- "^[0-9]{0,3}\\.[0-9]+$|^[0-9]{1,3}$"
fix_data_smhi <- secchi_raw_smhi %>% 
  filter(!str_detect(Sample.minimum.depth, depth_measurement)) %>% 
  mutate(Sample.maximum.depth = as.character(Sample.maximum.depth)) %>%
  mutate(Sampling.laboratory = as.numeric(as.character(Sampling.laboratory))) %>%
  select(-Sample.minimum.depth)
colnames(fix_data_smhi) <- setdiff(colnames(secchi_raw_smhi), "Sample.comment")

## bind corrected rows back to the other rows
secchi_smhi_correctcols <- secchi_raw_smhi %>% 
  filter(str_detect(Sample.minimum.depth, depth_measurement)) %>% 
  bind_rows(mutate(fix_data_smhi, Sample.comment = NA))

fix_data_smhi <- secchi_smhi_correctcols %>% 
  filter(is.na(Sample.minimum.depth), str_detect(Sampling.laboratory, depth_measurement)) %>% 
  mutate(Sample.maximum.depth = as.character(Sample.maximum.depth)) %>%
  mutate(Sampling.laboratory = as.numeric(as.character(Sampling.laboratory))) %>%
  select(-Sample.minimum.depth)
colnames(fix_data_smhi) <- setdiff(colnames(secchi_smhi_correctcols), "Species.flag")
secchi_smhi_correctcols <- secchi_smhi_correctcols %>% 
  filter(!(is.na(Sample.minimum.depth) & str_detect(Sampling.laboratory, depth_measurement))) %>% 
  bind_rows(mutate(fix_data_smhi, Species.flag = NA))

## rows where parameter, values, and units are shifted
## cols to right of parameter column are shifted one or two to the right
fix_data_smhi <- secchi_smhi_correctcols %>%
  filter(str_detect(Value, "Secchi depth")) %>%
  select(-Parameter)
colnames(fix_data_smhi) <- setdiff(colnames(secchi_smhi_correctcols), "Data.set.name")
secchi_smhi_correctcols <- bind_rows(
  mutate(fix_data_smhi, Data.set.name = NA),
  filter(secchi_smhi_correctcols, !str_detect(Value, "Secchi depth"))
)
fix_data_smhi <- secchi_smhi_correctcols %>%
  filter(str_detect(Unit, "Secchi depth")) %>%
  select(-Parameter, -Value)
colnames(fix_data_smhi) <- setdiff(colnames(secchi_smhi_correctcols), c("File.name",  "Data.set.name"))
secchi_smhi_correctcols <- bind_rows(
  mutate(fix_data_smhi, File.name = NA, Data.set.name = NA),
  filter(secchi_smhi_correctcols, !str_detect(Unit, "Secchi depth"))
)

## check result of column shift corrections
# dim(secchi_raw_smhi)
# dim(secchi_smhi_correctcols)
# for(col in c("Sample.minimum.depth", "Sample.maximum.depth", "Parameter", "Unit")){
#   print(unique(secchi_smhi_correctcols[[col]]))
# }
# summary(as.numeric(unique(secchi_smhi_correctcols[["Value"]])))
# dim(filter(secchi_smhi_correctcols, Parameter != "Secchi depth"))


secchi_smhi <- secchi_smhi_correctcols %>% 
  filter(Parameter == "Secchi depth") %>% 
  mutate(
    Value = as.numeric(as.character(Value)), 
    Parameter = as.character(Parameter)
  ) %>% 
  rename(
    secchi = Value, 
    date = Sampling.date, 
    cruise = Visit.event.identifier, 
    station = Station.name, 
    ## use the decimal degrees for lat and lon
    latitude = Sample.latitude..DD., 
    longitude = Sample.longitude..DD.,
    ## 'Station.water.depth' or 'Sample.maximum.depth' cosidered as 'Bottom depth'?
    bottom_m = Station.water.depth # Sample.maximum.depth??
  ) %>% 
  mutate(
    date = as.Date(date), 
    year = as.character(format(date, "%Y")),
    month = format(date, "%m"),
    month = str_replace(month, "^0+" ,""),
    bottom_m = as.numeric(bottom_m),
    station = as.character(station),
    cruise = as.character(cruise)
  ) %>% 
  select(cruise, station, date, year, month, latitude, longitude, secchi, bottom_m) 

## Look for duplicate data points
secchi_smhi_duplicates <- secchi_smhi %>%
  group_by(
    cruise, station, date, year, month, 
    latitude, longitude, 
    secchi, bottom_m
  ) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup()
# sum(secchi_smhi_duplicates$n)-nrow(secchi_smhi_duplicates)

## extract only unique observations
secchi_smhi_unique <- secchi_smhi %>% 
  distinct(
    cruise, station, date, year, month, 
    latitude, longitude, 
    secchi, bottom_m
  ) %>% 
  mutate(supplier = "smhi") 
# nrow(secchi_smhi)-nrow(secchi_smhi_unique)
```

``` r
## Combined Secchi Data

merge_secchi_datasets <- bind_rows(secchi_smhi_unique, secchi_ices_unique) %>% 
  ## prefer first ices, then smhi data where there are overlaps
  mutate(supplier_num = ifelse(supplier == "ices", 2, 1)) %>% 
  group_by(date, year, month, latitude, longitude, secchi, bottom_m) %>% 
  top_n(n = 1)

# summary(as.factor(merge_secchi_datasets$supplier))
# tmp <- merge_secchi_datasets %>%
#   group_by(cruise, station, date, year, month, latitude, longitude, secchi, bottom_m) %>%
#   mutate(count = n())
# unique(tmp$count)

secchi <- rename(merge_secchi_datasets, lat = latitude, lon = longitude, secchi_m = secchi)
# nrow(distinct(secchi))

## Save merged dataset
# readr::write_csv(
#   secchi,
#   here::here("data", "CW", "eutrophication", version_year, "intermediate", "secchi_merged_rawdata.csv")
# )
```

<br/>

#### 2.2.2 Merging Oxygen Datasets

``` r
## this will read in both the oxygen and CTD datasets from the OxygenDebt_ICES directory
## in the same step, this will check for duplicates and then keep only the distinct observations
read_ox_data <- function(datasets = "ox"){
  
  lapply(
    ## lists all subfiles in OxygenDebt_ICES directory, and selects only ones with csv extension
    grep(
      pattern = datasets, 
      list.files(file.path(dir_rawdata, "OxygenDebt_ICES"), pattern = ".csv", recursive = TRUE),
      value = TRUE
    ),
    ## read in the file and assign to a variable in the global environment
    function(x){
      tmp <- read.delim(file.path(dir_rawdata, "OxygenDebt_ICES", x), sep = ",", stringsAsFactors = FALSE)
      
      ## Check for duplicates in all national datasets
      # duplicates <- tmp %>% 
      #   group_by(
      #     Cruise, Station, yyyy.mm.ddThh.mm, Bot..Depth..m., TEMP..deg.C., PSAL..psu.,
      #     PRES..db., Latitude..degrees_north., Longitude..degrees_east., DOXY..ml.l.
      #   ) %>%
      #   summarise(n = n()) %>%
      #   filter(n > 1) %>%
      #   ungroup()
      
      ## Extract only unique observations
      uniquedat <- tmp %>% 
        distinct(
          Cruise, Station, yyyy.mm.ddThh.mm, Bot..Depth..m., TEMP..deg.C., PSAL..psu.,
          PRES..db., Latitude..degrees_north., Longitude..degrees_east., DOXY..ml.l.
        )
      nam <- x %>% 
        stringr::str_extract("[a-zA-Z_0-9\\-]+.csv|[a-z0-9\\-]+.csv|[a-z]+.csv") %>% 
        stringr::str_remove_all(".csv") %>% 
        stringr::str_remove_all("[0-9]{4}\\-[0-9]{2}")
      ## different ctd and h2s naming conventions...
      nam <- ifelse(nam == "0410032c", "finlandctd", nam)
      nam <- ifelse(str_detect(nam, "CTD_"), paste0(str_remove(nam, "CTD_"), "ctd"), nam)
      nam <- ifelse(str_detect(nam, "H2S_"), paste0(str_remove(nam, "H2S_"), "h2s"), nam)
      
      ## assign to names in global environment...
      if(str_detect(nam, "ox$")){
        assign(sprintf("ox_%s", nam), uniquedat, envir = .GlobalEnv)
      }
      if(str_detect(nam, "ctd$")){
        assign(sprintf("ctd_%s", nam), uniquedat, envir = .GlobalEnv)
      }
      if(str_detect(nam, "h2s$")){
        assign(sprintf("h2s_%s", nam), uniquedat, envir = .GlobalEnv)
      }
    }
  ) 
}
```

<br/>

#### 2.2.3 Chlorophyll a Wrangling

``` r
chla_raw_nest <- read.delim(file.path(dir_rawdata, "NestData", "nestdata_summer_chlorophyl.csv"), sep = ",")
chla_raw_smhi <- read.delim(file.path(dir_rawdata, "chla_rawdata_smhi.csv"), sep = ";")
```

**Coastal data**

  - Non-coastal (offshore) data are flagged with the code “0” under the
    column *“HELCOM\_COASTAL\_CODE”*  
  - HELCOM basin shape files with coastal and non-coastal areas were
    overlaid with the Chl *a* sampling locations, all locations were
    flagged with a code indicating coastal or offshore.

**Steps in Merging ICES and SMHI Secchi Datasets**

1)  Rename variables and subset datasets to be merged, so column names
    and data types align
2)  Check duplicates and select only distinct observations
3)  Combine Baltic Nest, SMHI and ICES data, identifying and eliminating
    overlaps

<!-- end list -->

``` r
## Baltic Nest Chlorophyll a Data

chla_nest <- chla_raw_nest %>% 
  rename(
    chla_ug_l = CHL,  
    date = OBSDATE, time = OBSTIME,
    latitude = LATITUDE, longitude = LONGITUDE, 
    cruise = SHIP, id = ID, depth_m = OBSDEP
  ) %>%
  mutate(
    date = as.Date(date), 
    year = format(date, "%Y"), 
    month = format(date, "%m"),
    cruise = as.character(cruise)
  ) %>% 
  select(-id, -time)

## Check for duplicates 
chla_nest_duplicates <- chla_nest %>%
  group_by(
    cruise, date, year, month, 
    latitude, longitude, 
    chla_ug_l, depth_m
  ) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup()
# sum(chla_nest_duplicates$n)-nrow(chla_nest_duplicates)

## Only extract unique observations
chla_nest_unique <- chla_nest %>% 
  distinct(
    cruise, date, year, month, 
    latitude, longitude, 
    chla_ug_l, depth_m
  ) %>% 
  mutate(supplier = "nest") # 44071
# nrow(chla_nest)-nrow(chla_nest_unique)
```

``` r
## SMHI Chlorophyll a Data

## overview
# colnames(chla_raw_smhi)
# str(chla_raw_smhi)

## ADJUST ROWS
## three extra columns starting with 'X' indicate some weird column shifts

## correct where Sample depths columns have been shifted one or two columns over 
depth_measurement <- "^[0-9]{0,3}\\.[0-9]+$|^[0-9]{1,3}$"
fix_data_smhi <- chla_raw_smhi %>% 
  filter(!str_detect(Sample.minimum.depth, depth_measurement)) %>% 
  mutate(Sample.maximum.depth = as.character(Sample.maximum.depth)) %>%
  mutate(Sampling.laboratory = as.numeric(as.character(Sampling.laboratory))) %>%
  select(-Sample.minimum.depth)
colnames(fix_data_smhi) <- setdiff(colnames(chla_raw_smhi), "Sample.comment")

## bind corrected rows back to the other rows
chla_smhi_correctcols <- bind_rows(
  mutate(fix_data_smhi, Sample.comment = NA),
  filter(chla_raw_smhi, str_detect(Sample.minimum.depth, depth_measurement))
)

fix_data_smhi <- chla_smhi_correctcols %>% 
  filter(is.na(Sample.minimum.depth), str_detect(Sampling.laboratory, depth_measurement)) %>% 
  mutate(Sample.maximum.depth = as.character(Sample.maximum.depth)) %>% 
  mutate(Sampling.laboratory = as.numeric(as.character(Sampling.laboratory))) %>% 
  select(-Sample.minimum.depth)
colnames(fix_data_smhi) <- setdiff(colnames(chla_smhi_correctcols), "X.3")
chla_smhi_correctcols <- bind_rows(
  mutate(fix_data_smhi, X.3 = NA),
  filter(chla_smhi_correctcols, !(is.na(Sample.minimum.depth) & str_detect(Sampling.laboratory, depth_measurement)))
)

## rows where parameter and values are shifted
## cols to right of parameter column are shifted one or two to the right
fix_data_smhi <- chla_smhi_correctcols %>% 
  filter(str_detect(Value, "Chlorophyll-a bottle")) %>% 
  select(-Parameter)
colnames(fix_data_smhi) <- setdiff(colnames(chla_smhi_correctcols), "X.3")
chla_smhi_correctcols <- bind_rows(
  mutate(fix_data_smhi, X.3 = NA),
  filter(chla_smhi_correctcols, !str_detect(Value, "Chlorophyll-a bottle"))
)
fix_data_smhi <- chla_smhi_correctcols %>% 
  filter(str_detect(Unit, "Chlorophyll-a bottle")) %>% 
  select(-Parameter, -Value)
colnames(fix_data_smhi) <- setdiff(colnames(chla_smhi_correctcols), c("X.2","X.3"))
chla_smhi_correctcols <- bind_rows(
  mutate(fix_data_smhi, X.2 = NA, X.3 = NA),
  filter(chla_smhi_correctcols, !str_detect(Unit, "Chlorophyll-a bottle"))
)

## check result of column shift corrections
# for(col in c("Sample.minimum.depth", "Sample.maximum.depth", "Parameter", "Unit")){
#   print(unique(chla_smhi_correctcols[[col]]))
# }
# summary(as.numeric(unique(chla_smhi_correctcols[["Value"]])))
# dim(filter(chla_smhi_correctcols, Parameter != "Chlorophyll-a bottle"))


chla_smhi <- chla_smhi_correctcols %>% 
  filter(Parameter == "Chlorophyll-a bottle") %>% 
  mutate(
    Value = as.numeric(as.character(Value)), 
    Parameter = as.character(Parameter)
  ) %>% 
  rename(
    chla_ug_l = Value, 
    date = Sampling.date, 
    cruise = Visit.event.identifier, 
    station = Station.name, 
    ## use the decimal degrees for lat and lon
    latitude = Sample.latitude..DD., 
    longitude = Sample.longitude..DD.,
    ## 'Station.water.depth' or 'Sample.maximum.depth' cosidered as 'Bottom depth'?
    depth_m = Sample.maximum.depth
  ) %>% 
  mutate(
    date = as.Date(date), 
    year = as.character(format(date, "%Y")),
    month = format(date, "%m"),
    # month = str_replace(Month, "^0+" ,""),
    depth_m = as.numeric(depth_m),
    # station = as.character(station),
    cruise = as.character(cruise)
  ) %>% 
  select(cruise, date, year, month, latitude, longitude, chla_ug_l, depth_m) 

## Look for duplicate data points
chla_smhi_duplicates <- chla_smhi %>%
  group_by(
    cruise, date, year, month, 
    latitude, longitude, 
    chla_ug_l, depth_m
  ) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup()
# sum(chla_smhi_duplicates$n)-nrow(chla_smhi_duplicates)

## extract only unique observations
chla_smhi_unique <- chla_smhi %>% 
  distinct(
    cruise, date, year, month, 
    latitude, longitude, 
    chla_ug_l, depth_m
  ) %>% 
  mutate(supplier = "smhi") 
# nrow(chla_smhi)-nrow(chla_smhi_unique)
```

``` r
## ICES Chlorophyll a Data


## this will read in the chl a datasets from our Chla_ICES directory and bind them into one
chla_raw_ices <- do.call(rbind, lapply(
  grep(list.files(file.path(dir_rawdata, "Chla_ICES"), recursive = TRUE), pattern = ".csv", value =  TRUE),
  function(x){
    cbind(
      read.delim(file.path(dir_rawdata, "Chla_ICES", x), sep = ","), 
      country = str_extract(x, pattern = "^[a-z]*")
    )
  }
))
chla_ices <- chla_raw_ices %>% 
  select(-Type) %>% 
  rename(
    date = yyyy.mm.ddThh.mm,
    latitude = Latitude..degrees_north.,
    longitude = Longitude..degrees_east.,
    station = Station, 
    cruise = Cruise, 
    temperature = TEMP..deg.C.,
    salinity = PSAL..psu.,
    pressure = PRES..db., 
    chla_ug_l = CPHL..ug.l.,
    depth_m = Bot..Depth..m. # so many NAs in this variable....
  ) %>% 
  mutate(
    year = format(strptime(date, "%Y-%m-%dT%H:%M"), "%Y"),
    month = format(strptime(date, "%Y-%m-%dT%H:%M"), "%m"),
    date = as.Date(date, "%Y-%m-%d"),
    station = as.character(station),
    pressure = as.numeric(pressure),
    depth_m = as.numeric(depth_m),
    chla_ug_l = str_replace(chla_ug_l, "<", ""),
    ## keep the values or set a range lower than that?
    chla_ug_l = as.numeric(chla_ug_l)
  )

## Check for duplicates in ICES secchi data
ices_duplicates <- chla_ices %>%
  group_by(
    cruise, station, latitude, longitude, 
    date, year, month,
    chla_ug_l, depth_m
  ) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>%
  ungroup() 
# sum(ices_duplicates$n)-nrow(ices_duplicates) # 22595

## Extract only unique observations
chla_ices_unique <- chla_ices %>% 
  distinct(
    cruise, station, latitude, longitude, 
    date, year, month,
    chla_ug_l, depth_m
  ) %>% 
  mutate(supplier = "ices") 
# nrow(chla_ices)-nrow(chla_ices_unique) # 22595
```

``` r
## Combined Chlorophyll a Data

merge_chla_datasets <- bind_rows(chla_nest_unique, chla_smhi_unique, chla_ices_unique) %>% 
  filter(chla_ug_l >= 0) %>% 
  ## prefer first ices, then smhi, then nest data where there are overlaps
  mutate(supplier_num = ifelse(supplier == "ices", 3, ifelse(supplier == "smhi", 2, 1))) %>% 
  group_by(date, year, month, latitude, longitude, chla_ug_l, depth_m) %>% 
  top_n(n = 1)

# summary(as.factor(merge_chla_datasets$supplier))
# tmp <- merge_chla_datasets %>%
#   group_by(cruise, station, date, year, month, latitude, longitude, chla_ug_l, depth_m) %>%
#   mutate(count = n())
# unique(tmp$count)
```

<br/>

### 2.3 Initial Data Exploration

#### 2.3.1 Maps of SMHI datasets

``` r
basemap <- ggplot2::ggplot(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf(size = 0.1, color = "burlywood", alpha = 0.4) +
  theme(panel.background = element_rect(fill = "#F8FBFC", color = "#E2EEF3")) +
  scale_x_continuous(limit = c(4, 32)) +
  scale_y_continuous(limit = c(53.5, 66)) 

## make larger discrete color palette
pal <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(17)
```

``` r
smhi_secchi_map <- basemap +
  geom_sf(
    mapping = aes(color = Sea.basin), 
    data = sf::st_as_sf(
      filter(secchi_smhi_correctcols, HELCOM.OSPAR.area %in% c("HELCOM", "OSPAR/HELCOM"), str_detect(Sea.basin, "[0-9]")), 
      coords = c("Sample.longitude..DD.", "Sample.latitude..DD."), 
      crs = 4326
    ),
    size = 0.5, alpha = 0.3, show.legend = FALSE
  ) +
  labs(
    color = "Sea Basin", 
    title = "Secchi SMHI Data Measurement Locations"
  ) +
  # theme(legend.position = c(0.2, 0.7)) +
  scale_color_manual(values = pal)

smhi_chla_map <- basemap +
  geom_sf(
    mapping = aes(color = Sea.basin), 
    data = sf::st_as_sf(
      filter(chla_smhi_correctcols, HELCOM.OSPAR.area %in% c("HELCOM", "OSPAR/HELCOM"), str_detect(Sea.basin, "[0-9]")), 
      coords = c("Sample.longitude..DD.", "Sample.latitude..DD."), 
      crs = 4326
    ),
    size = 0.5, alpha = 0.3
  ) +
  labs(
    color = "Sea Basin", 
    title = "Chlorophyll a SMHI Data Measurement Locations"
  ) +
  # theme(legend.position = c(0.2, 0.7)) +
  scale_color_manual(values = pal)

gridExtra::grid.arrange(smhi_secchi_map, smhi_chla_map, nrow = 2, heights = c(1.3, 1))
```

![](eut_prep_files/figure-gfm/chla%20raw%20datasets-1.png)<!-- --> <br>

#### 2.3.2 Oxygen data

In the depth vs dissolved oxygen plots below, the lighter greens and
yellow correspond to earlier years, and darker greens and purples
represent more recent
years.

``` r
dir_interm <- here::here("data", "CW", "eutrophication", version_year, "intermediate")
ox_merged_w_rgns <- readr::read_csv(file.path(dir_interm, "ox_merged_rawdata.csv"))
ctd_merged_w_rgns <- readr::read_csv(file.path(dir_interm, "ctd_merged_rawdata.csv"))

## quick maps of measurements by subbasin for selected year
oxplotsf <- ox_merged_w_rgns %>% 
  filter(Year == 2018, !is.na(Assessment_Unit)) %>% 
  sf::st_as_sf(crs = 4326, coords = c("Longitude", "Latitude"))
oxmap <- basemap + 
  geom_sf(
    data = oxplotsf, 
    aes(color = Assessment_Unit), 
    size = 0.4, alpha = 0.02
  ) +
  ggtitle("Dissolved Oxygen Bottle\nMeasurement Locations") +
  scale_color_manual(values = pal)

ctdplotsf <- ctd_merged_w_rgns %>% 
  filter(Year == 2018) %>% 
  sf::st_as_sf(crs = 4326, coords = c("Longitude", "Latitude"))
ctdmap <- basemap + 
  geom_sf(
    data = ctdplotsf, 
    aes(color = Assessment_Unit), 
    size = 0.4, alpha = 0.02, show.legend = FALSE
  ) +
  ggtitle("Dissolved Oxygen CTD \nMeasurement Locations") +
  scale_color_manual(values = pal)

## plot by subbasins
oxplotdf <- ox_merged_w_rgns %>% 
  filter(!is.na(Assessment_Unit)) %>% 
  group_by(Assessment_Unit) %>%
  sample_n(3000) %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))
ctdplotdf <- ctd_merged_w_rgns %>% 
  filter(!is.na(Assessment_Unit)) %>% 
  group_by(Assessment_Unit) %>%
  sample_n(2000) %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))

oxplot <- ggplot(oxplotdf, aes(Oxygen, Depth, color = Year)) + 
  geom_point(alpha = 0.02, size = 0.7, show.legend = FALSE) +
  facet_wrap(~Assessment_Unit, scales = "free_y", ncol = 3)  +
  labs(x = "Dissolved Oxygen (ml/l)", y = "Depth (m)") +
  scale_y_reverse() +
  scale_color_viridis_c(direction = -1)
ctdplot <- ggplot(ctdplotdf, aes(Oxygen, Depth, color = Year)) + 
  geom_point(alpha = 0.02, size = 0.7, show.legend = FALSE) +
  facet_wrap(~Assessment_Unit, scales = "free_y", ncol = 3)  +
  labs(
    x = "Dissolved Oxygen (ml/l)", 
    y = "Depth (m)"
    # caption = paste(
    #   "Dissolved Oxygen (ml/l) vs Sub-basin Depth (m)",
    #   "The lighter greens and yellow correspond to earlier years", 
    #   "and darker greens and purples represent more recent years.",
    #   "Plot on the left shows bottle data, plot on the right shows CTD data."
    # )
  ) + 
  scale_y_reverse() +
  scale_color_viridis_c(direction = -1) +
  theme(plot.caption = element_text(hjust = 0,  size = 6))

## timeseries plot
tsplotdf <- rbind(
  mutate(oxplotdf, measure_type = "Bottle") %>% select(-Bot_Depth, -Station),
  mutate(ctdplotdf, measure_type = "CTD")
)
oxtsplot <- ggplot(tsplotdf, aes(Date, Oxygen, color = measure_type)) + 
  geom_point(size = 0.1, alpha = 0.05, show.legend = FALSE) +
  facet_wrap(~Assessment_Unit, ncol = 3) +
  labs(
    x = "Year", 
    y = "Dissolved Oxygen (ml/l, Bottle data in pink and CTD data in Blue)",
    caption = paste(
      "Timeseries of dissolved oxygen (ml/l) by Sub-basin.",
      "Bottle data are shown in pink and CTD data in blue."
    )
  ) + 
  theme(plot.caption = element_text(hjust = 0))


gridExtra::grid.arrange(oxmap, ctdmap, nrow = 2, widths = c(1.2, 1))
```

![](eut_prep_files/figure-gfm/map%20showing%20oxygen%20datasets-1.png)<!-- -->

``` r
#gridExtra::grid.arrange(
#  oxplot, ctdplot, oxtsplot,
#  layout_matrix = matrix(c(1,2,3,3), ncol = 2, byrow = TRUE)
#)
```

<br>

#### 2.3.3 Comparing chl a data Sources

![](eut_prep_files/figure-gfm/chlorophyl%20a%20data%20source%20histograms-1.png)<!-- -->

<!-- #### 2.3.1 Compare versus Previous Years Data {-} -->

<!-- #### 2.3.2 Timeseries Plots {-} -->

<!-- #### 2.3.3 Map {-} -->

<br>

## 3\. Prep: Wrangling & Derivations, Checks/Evaluation, Gapfilling

``` r
## secchi, oxygen, and chlorophyll a datasets, 
## previously merged in 'data/CW/eutrophication/eut_data.R' script
combined_secchi_rawdata <- read_csv(file.path(dirname(data_path), "intermediate", "secchi_merged_rawdata.csv"))
combined_ox_rawdata <- read_csv(file.path(dirname(data_path), "intermediate", "ox_merged_rawdata.csv"))

chla_rawdata <- read_csv(file.path(dirname(data_path), "intermediate", "chla_rawdata.csv"))
# tmp <- problems(chla_rawdata) # to check parsing errors
# unique(tmp$col) # only problem with parsing cruise column, doesn't matter, not used in the analysis

## nutrients dataset
nutrients_rawdata <- read_csv(file.path(dir_rawdata, "NestData", "nestdata_winter_nutrients.csv"))

## eutrophication thresholds data
## Note: this table exists in its original form on the bhi internal shared server 
## read.delim(file.path(dir_rawdata, "eut_threshold_values.csv"), sep = ";")
## obtained originally from (?)
## with columns:  
## basin = str_trim(str_replace_all(Assessment.units,"_",""), side = "right"),
## oxyg_debt = as.numeric(str_replace(O2..mg.l.1., "N", " ")),
## winter_DIN = DIN.._mol.l.1., winter_DIP = DIP.._mol.l.1.,
## summer_chla = Chla.._g.l.1., summer_secchi = Water.clarity..m. 
eut_thresholds <- read_csv(file.path(loc, "eut_threshold_values.csv"))

chla_coastal_thresholds <- read_csv(file.path(loc, "chla_coastal_thresholds.csv"))
din_coastal_thresholds <- read_csv(file.path(loc, "din_coastal_thresholds.csv"))
dip_coastal_thresholds <- read_csv(file.path(loc, "dip_coastal_thresholds.csv"))
```

<br/>

**The basin-specific offshore thresholds used in calculating the five
Eutrophication indicators**

``` r
tab <- eut_thresholds
newnames <- colnames(tab) %>% 
  str_replace("_", " ") %>% 
  str_to_title() %>% 
  str_replace("D[a-z]{2}$", str_to_upper(str_extract(., "D[a-z]{2}$")))
colnames(tab) <- newnames
knitr::kable(tab)
```

| Basin                  | Winter DIN | Winter DIP | Summer Chla | Summer Secchi | Oxyg Debt |
| :--------------------- | ---------: | ---------: | ----------: | ------------: | --------: |
| Kattegat               |        5.0 |       0.49 |        1.50 |           7.6 |        NA |
| Great Belt             |        5.0 |       0.59 |        1.70 |           8.5 |        NA |
| The Sound              |        3.3 |       0.42 |        1.20 |           8.2 |        NA |
| Kiel Bay               |        5.5 |       0.57 |        2.00 |           7.4 |        NA |
| Bay of Mecklenburg     |        4.3 |       0.49 |        1.80 |           7.1 |        NA |
| Arkona Basin           |        2.9 |       0.36 |        1.80 |           7.2 |        NA |
| Bornholm Basin         |        2.5 |       0.30 |        1.80 |           7.1 |      6.37 |
| Gdansk Basin           |        4.2 |       0.36 |        2.20 |           6.5 |      8.66 |
| Eastern Gotland Basin  |        2.6 |       0.29 |        1.90 |           7.6 |      8.66 |
| Western Gotland Basin  |        2.0 |       0.33 |        1.20 |           8.4 |      8.66 |
| Gulf of Riga           |        5.2 |       0.41 |        2.70 |           5.0 |        NA |
| Northern Baltic Proper |        2.9 |       0.25 |        1.65 |           7.1 |      8.66 |
| Gulf of Finland        |        3.8 |       0.59 |        2.00 |           5.5 |      8.66 |
| Aland Sea              |        2.7 |       0.21 |        1.50 |           6.9 |        NA |
| Bothnian Sea           |        2.8 |       0.19 |        1.50 |           6.8 |        NA |
| The Quark              |        3.7 |       0.10 |        2.00 |           6.0 |        NA |
| Bothnian Bay           |        5.2 |       0.07 |        2.00 |           5.8 |        NA |
| <br>                   |            |            |             |               |           |

**The basin-specific coastal thresholds** *(later excluded from the
analyses)*

Since only [Latvia, Poland, and Sweden have threshold
values](https://helcom.fi/media/core%20indicators/Dissolved-inorganic-nitrogen-DIN-HELCOM-core-indicator-2018.pdf)
for dissoved inorganic nutrients, we only assess offshore components for
these indicators, despite the significant impact of coastal nutrients on
overall Baltic Health. Nutrient inflows, however, are accounted for
elsewhere in the index as pressures e.g. on Biodiversity.

Both offshore and coastal data are included in calculation of the
Chlorophyll a indicator, with different and respective offshore and
coastal threshold values.

``` r
tab <- chla_coastal_thresholds %>% 
  rename(summer_chla = threshold, summer_chla_units = units) %>% 
  full_join(
    din_coastal_thresholds %>% 
      rename(winter_DIN = threshold, winter_DIN_units = units) ,
    by = "code"
  ) %>% 
  full_join(
    dip_coastal_thresholds %>% 
      rename(winter_DIP = threshold, winter_DIP_units = units),
    by = "code"
  )
# newnames <- colnames(tab) %>% 
#   str_replace_all("_", " ") %>% 
#   str_to_title() %>% 
#   str_replace("D[a-z]{2}$", str_to_upper(str_extract(., "D[a-z]{2}$"))) %>% 
#   str_replace("D[a-z]{2} Units$", paste(str_to_upper(str_extract(., "D[a-z]{2}")), "Units"))
# colnames(tab) <- newnames
# knitr::kable(tab)

## only have coastal nutrient thresholds for Latvia, Poland, Sweden..
```

<br>

``` r
thresholds_combined <- bind_rows(
  eut_thresholds %>% 
    rename(subbasin = basin) %>% 
    left_join(distinct(basin_lookup, subbasin, HELCOM_ID), by = "subbasin") %>% 
    rename(helcom_id = HELCOM_ID) %>% 
    select(-subbasin) %>% 
    mutate(
      summer_secchi_units =  "meters",
      summer_chla_units = "ug_per_l",
      winter_DIN_units = "umol_per_l",
      winter_DIP_units = "umol_per_l"
    ),
  tab %>% 
    rename(helcom_id = code) %>% 
    mutate(oxyg_debt = NA, summer_secchi = NA, summer_secchi_units = NA)
)

## convertions for uniform units per indicator...
## https://ocean.ices.dk/tools/UnitConversion.aspx
## not sure what to do when have EQR units!!!
thresholds_combined <- thresholds_combined %>% 
  mutate(
    winter_DIN = ifelse(winter_DIN_units == "mg_per_l", winter_DIN*1000, winter_DIN),
    winter_DIN = ifelse(winter_DIN_units == "EQR", NA, winter_DIN),
    winter_DIN_units = "umol_per_l",
    winter_DIP = ifelse(winter_DIP_units == "mg_per_l", winter_DIP*1000, winter_DIP),
    winter_DIP = ifelse(winter_DIP_units == "EQR", NA, winter_DIP),
    winter_DIP_units = "umol_per_l",
  ) %>% 
  select(helcom_id, summer_secchi, summer_chla, winter_DIN, winter_DIP, oxyg_debt) %>% 
  tidyr::pivot_longer(2:6, names_to = "indicator", values_to = "value")

write_csv(
  thresholds_combined, 
  file.path(dir_layers, sprintf("cw_eut_targets_bhi%s.csv", assess_year))
)
```

### 3.1 Secchi Indicator

#### 3.1.1 Match BHI regions

The raw Secchi data (consisting of merged ICES and SMHI data) must be
assigned to their respective Subbasins (with HELCOM Coastal/Offshore
division codes) and BHI Regions based on their latitude/longitude
coordinates. Then the data are subsetted to assure only summer months
(June-September) and years 2000 onward are
included.

``` r
## before assigning bhi regions to secchi data, run this function first, to load required shapefiles:
# source(here::here("R", "spatial.R"))
# regions_shape(sp_dir = file.path(dirname(dir_B), "Shapefiles"))

combined_secchi_rawdata <- combined_secchi_rawdata %>% 
  rename(lat = Latitude, lon = Longitude)

## assign helcom offshore codes
secchi_rgns_w_offshore <- combined_secchi_rawdata %>% 
  sf::st_as_sf(
    crs = 4326,
    coords = c("lon","lat"),
    remove = FALSE
  ) %>%  
  sf::st_join(sf::st_transform(offshore_codes, 4326)) %>% 
  sf::st_drop_geometry() %>% 
  select(-Subbasin, -HELCOM_ID)

## assign bhi basins to secchi data 
secchi_rgns_shp <- join_rgns_info(
  secchi_rgns_w_offshore, helcomID_col = "helcom_id", country_col = "country", 
  latlon_vars = c("^lat", "^lon"), return_spatial = FALSE, 
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefiles"), 
  buffer_shp = NULL
) 
secchi_rgns <- secchi_rgns_shp %>% 
  ## select summer months (June-September) and Years >= 2000  
  filter(month == "6" | month == "7" | month == "8" | month == "9") %>% 
  filter(year >= 2000) %>% 
  mutate(supplier = stringr::str_to_upper(supplier)) %>% 
  mutate(coastal = ifelse(is.na(helcom_coastal_code), "Coastal", "Offshore"))
```

<br>

**Remove coastal observations**

For the Secchi depth indicator, only offshore data points will be used.
These are identified using [HELCOM Coastal and Offshore divisions (2018
version)](https://maps.helcom.fi/arcgis/rest/services/MADS/Sea_environmental_monitoring/MapServer/4).

``` r
## Select only data with coastal code "0"  
dim(secchi_rgns) # 45479   

## Do any observations have NA for coast_code
secchi_rgns %>% 
  filter(is.na(helcom_coastal_code) & is.na(BHI_ID)) %>% 
  dim() # 4803  

secchi_rgns %>% 
  filter(is.na(helcom_coastal_code) & !is.na(BHI_ID)) %>% 
  dim() # 32661

## 617 observations with no coast_code or BHI_ID
loc_no_coastcode_nobhi <- secchi_rgns %>% 
  filter(is.na(helcom_coastal_code) & is.na(BHI_ID))%>%
  dplyr::select(lat, lon)%>%
  distinct()

## check locations
# library(ggmap)
# map <- get_map(location = c(8.5, 53, 32, 67.5))
# no_coastal_or_bhi_id <- ggmap(map) + 
#   geom_point(aes(x = lon, y = lat), data = loc_no_coastcode_nobhi, size = 2) +
#   labs(x = NULL, y = NULL)
## these locations are very coastal or outside of the Baltic Sea

## observations with NA for the coast_code but have BHI_ID
loc_no_coastcode_bhi <- secchi_rgns %>% 
  filter(is.na(helcom_coastal_code) & !is.na(BHI_ID)) %>% 
  dplyr::select(lat, lon) %>%
  distinct()
# no_coastal_id <- ggmap(map) + 
#   geom_point(aes(x = lon, y = lat), data = loc_no_coastcode_bhi, size = 2, alpha = 0.3) +
#   labs(x = NULL, y = NULL)
## many coastal location

## Filter data that are only offshore, coast_code == 0
secchi_offshore <- filter(secchi_rgns, helcom_coastal_code == 0) 
dim(secchi_offshore) # 8015
# secchi_offshore_spatial <- ggmap(map) + 
#   geom_point(aes(x = lon, y = lat), data = secchi_offshore, colour = "tomato", size = 2, alpha = 0.3) +
#   labs(x = NULL, y = NULL)
## This is a substantial reduction in the number of observations

# gridExtra::grid.arrange(no_coastal_or_bhi_id, no_coastal_id, secchi_offshore_spatial, nrow = 1)
```

<br>

#### 3.1.2 Visualize Secchi Depth Data

**Spatial Visualization of Secchi data**

The four plots generated in this section show temporal trends in
offshore Secchi depth (right) and monthly distributions (left) by BHI
regions (top) or subbasins (bottom), with data source indicated by
color. Pink points correspond to ICES data, and turquoise points to SMHI
data.

``` r
plotdf <- left_join(
  secchi_offshore, 
  data.frame(month = c(6,7,8,9), Month = c("Jun", "Jul", "Aug", "Sep"))
)
plotdf$Month <- factor(plotdf$Month, levels = c("Jun", "Jul", "Aug", "Sep"))

## Plot by BHI regions and year
plot_secchi_bhi_ts <- ggplot(secchi_offshore) + 
  geom_point(
    aes(date, secchi_m, color = supplier), 
    size = 1, alpha = 0.3, show.legend = FALSE
  ) +
  facet_wrap(
      c("Subbasin", "rgn_nam"), 
      labeller = label_wrap_gen(width = 35, multi_line = FALSE),
      ncol = 4
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), 
    strip.text.x = element_text(size = 6)
  ) +
  labs(x = NULL, y = "Secchi depth (m)", color = "Data Source") +
  ggtitle("Yearly Secchi by BHI regions")

## Month boxplots, by BHI region
plot_secchi_bhi_box <- ggplot(plotdf) + 
  geom_boxplot(
    aes(Month, secchi_m, fill = supplier), 
    size = 0.2, outlier.size = 0.3, alpha = 0.6, show.legend = FALSE
  ) +
  facet_wrap(
      c("BHI_ID", "Subbasin", "rgn_nam"), 
      labeller = label_wrap_gen(width = 35, multi_line = FALSE),
      ncol = 4
  ) +
  labs(x = NULL, y = "Secchi depth (m)", fill = "Data Source") +
  theme(strip.text.x = element_text(size = 6)) +
  ggtitle("Monthly Secchi by BHI regions")

## Plot by subbasins and year
plot_secchi_basin_ts <- ggplot(secchi_offshore) + 
  geom_point(
    aes(date, secchi_m, color = supplier),
    size = 1, alpha = 0.3, show.legend = FALSE
  ) +
  # geom_point(aes(year, secchi_m), size = 0.3, alpha = 0.4) +
  facet_wrap(~Subbasin, ncol = 4) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), 
    strip.text.x = element_text(size = 6)
  ) +
  labs(x = NULL, y = "Secchi depth (m)", color = "Data Source") +
  ggtitle("Yearly Secchi by Subbasins")

## Month boxplots, by Subbasin region
plot_secchi_basin_box <- ggplot(plotdf) + 
  geom_boxplot(
    aes(Month, secchi_m, fill = supplier), 
    size = 0.2, outlier.size = 0.3, alpha = 0.6, show.legend = FALSE
  ) +
  facet_wrap(~Subbasin, ncol = 4) +
  labs(x = NULL, y = "Secchi depth (m)", fill = "Data Source") +
  theme(strip.text.x = element_text(size = 6)) +
  ggtitle("Monthly Secchi by Subbasins")

gridExtra::grid.arrange(
  plot_secchi_basin_ts, plot_secchi_basin_box, 
  plot_secchi_bhi_ts, plot_secchi_bhi_box,
  nrow = 2,
  heights = c(1, 2)
)
```

![](eut_prep_files/figure-gfm/secchi%20timeseries%20yearly%20and%20monthly-1.png)<!-- -->

<br/>

#### 3.1.3 Mean Secchi depth Calculation

Mean Secchi depths are calculated per HELCOM sub-basin or BHI region,
using only offshore Secchi depth measurements. Means are first
calculated within each summer month, then those monthly means are
averaged to get an overall summer mean. Variances (standard deviations
squared) are also calculated within each month, then averaged to get an
average variance across summer months for the subbasin or BHI region.
This is presented as a standard deviation (square root of variance
across months) on the plots below.

Along with the mean Secchi calculation, we calculate a five-year moving
average for each year in the dataset. The moving average (or ‘rolling
mean’) is an average of summer means from the current and previous four
years.

``` r
for(r in c("Subbasin", "BHI_ID")){
  
  ## Calculate mean monthly value for each summer month
  ## using only offshore data for secchi indicator
  secchi_monthly_means <- secchi_offshore %>% 
    group_by(year, month, Subbasin, !!!syms(r)) %>%
    summarise(
      mean_secchi = mean(secchi_m, na.rm = TRUE),
      secchi_sd = sd(secchi_m, na.rm = TRUE),
      n_pts = n()
    ) %>%
    mutate(secchi_sd = ifelse(is.na(secchi_sd), 0, secchi_sd))
  
  # filter(secchi_monthly_means, is.na(mean_secchi)|is.nan(mean_secchi))
  ## what prop. of data where mean is based on <4 data points?
  # nrow(filter(secchi_monthly_means, n_pts <= 3))/nrow(secchi_monthly_means)
  
  ## Calculate summer mean secchi (subbasin)
  secchi_summer_months_mean <- secchi_monthly_means %>% 
    group_by(year, Subbasin, !!!syms(r)) %>%
    summarise(
      mean_secchi = mean(mean_secchi, na.rm = TRUE),
      secchi_sd = sqrt(mean((secchi_sd^2), na.rm = TRUE)),
      n_pts = sum(n_pts)
      
      # standard_err = secchi_sd/sqrt(n_pts)
    ) 
  
  ## calculate min and max, and rolling 5 year mean
  secchi_summer_months_mean <- secchi_summer_months_mean %>% 
    arrange(!!!syms(r), year) %>% 
    group_by(!!!syms(r)) %>%
    mutate(ma5yr = zoo::rollapply(mean_secchi, 5, mean, na.rm = TRUE, align = "right", fill = NA)) %>% 
    mutate(secchi_min = mean_secchi-secchi_sd, secchi_max = mean_secchi+secchi_sd) %>% 
    ungroup() # in mean calculation some months have NA, ignore for that year's calculation
  
  ## join with basin-specific eutrophication secchi targets
  secchi_summer_months_mean <- eut_thresholds %>% 
    select(Subbasin = basin, target_secchi = summer_secchi) %>% 
    right_join(secchi_summer_months_mean, by = "Subbasin")
  assign(paste0(str_to_lower(r), "_target_secchi"), secchi_summer_months_mean)
  
  ## Plot summer mean secchi
  secchimeansplot <- ggplot(secchi_summer_months_mean, aes(year, mean_secchi, group = 1)) + 
    geom_errorbar(
      aes(x = year, ymin = secchi_min, ymax = secchi_max), 
      alpha = 0.6, size = 0.3, width = 0.3
    ) +
    geom_point(size = 0.3) +
    # geom_line(aes(year, ma5yr), size = 0.2) +
    geom_line(size = 0.2) +
    ## helcom target value
    geom_line(aes(y = target_secchi), colour = "tomato", size = 0.4) +
    facet_wrap(vars(!!!syms(r)), ncol = 5) +
    scale_y_continuous(limits = c(0, 10)) +
    labs(x = NULL, y = "Secchi depth (m)") +
    ggtitle(sprintf("Mean Summer (June-Sept) Secchi by %s", r)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  assign(paste0("secchimeansplot", str_to_lower(r)), secchimeansplot)
}
```

<br>

**Plot summer Secchi with target values indicated**

The following shows annual mean summer Secchi depths in meters.
Horizontal lines are HELCOM target values, as summarized in the table at
the top of section 3.

``` r
## adding onto/calling plots made in code chunk above
gridExtra::grid.arrange(
  secchimeansplotsubbasin + 
    ggtitle("Mean Summer Secchi by Subbasin (June-Sept, Offshore) against Target Values"),
  secchimeansplotbhi_id + 
    ggtitle("Mean Summer Secchi by BHI Region (June-Sept, Offshore) against Target Values"),
  nrow = 2, 
  heights = c(1,2)
)
```

![](eut_prep_files/figure-gfm/summer%20secchi%20with%20targets-1.png)<!-- -->

<br>

#### 3.1.4 Status Calculation

**Explore years in data available for status calculation**

Only five basins have data for 2019, which will be used as the status
year, nine basins have data for 2018 and three basins for 2017 and for
these basins, their most recent year’s data is used for current status
calculations.

``` r
## get the last year of non-NA data
secchi_last_year <- subbasin_target_secchi %>%
  filter(!is.na(ma5yr)) %>%
  group_by(Subbasin)%>%
  summarise(last_year = last(year))

knitr::kable(
  secchi_last_year %>% 
    group_by(last_year) %>%
    summarize(`Num. Subbasins with Year` = n(), Subbasins = paste(Subbasin, collapse = ", ")) %>% 
    rename(`Last Year ` = last_year)
)
```

| Last Year | Num. Subbasins with Year | Subbasins                                                                                                                              |
| :-------: | -----------------------: | :------------------------------------------------------------------------------------------------------------------------------------- |
|   2017    |                        3 | Aland Sea, Gulf of Riga, The Quark                                                                                                     |
|   2018    |                        9 | Bay of Mecklenburg, Bothnian Bay, Bothnian Sea, Gdansk Basin, Great Belt, Gulf of Finland, Kiel Bay, Northern Baltic Proper, The Sound |
|   2019    |                        5 | Arkona Basin, Bornholm Basin, Eastern Gotland Basin, Kattegat, Western Gotland Basin                                                   |

``` r
## which are not in 2019
# filter(secchi_last_year, last_year < 2019)
```

<br>

**Status calculation with raw (non-modeled) mean summer Secchi by
basin**

Status is calculated on the basin level and then applied to all regions.
It is calculated here for additional visualization and evaluation, but
the final method of status calculation is also completed by the EUT
function in `functions.R` of the relevant bhi repository assessment
folder.

Status for the Secchi indicator is calculated as `basin summer mean`
divided by `basin target`, with the value capped at 1. The plot shows
BHI region scores (uniform across regions within the same basin, as the
data is aggregated by basin for the summer means calculation), and also
indicates which data year the status is based on.

``` r
## Define constants for status and trend calculations
min_year <- 2009 # earliest year to use as a start for regr_length timeseries
regr_length <- 10 # number of years to use for regression
future_year <- 5 # the year at which we want the likely future status
min_regr_length <- 5 # min actual number of years with data to use for regression
```

``` r
## Calculate basin status as: basin_mean/basin_target, constraining values to 1 as maximum score
secchi_status_allyrs <- subbasin_target_secchi %>%
  filter(year >= min_year) %>% 
  rename(subbasin = Subbasin) %>%
  # mutate(status = pmin(1, mean_secchi/target_secchi)) 
  ## or use five year rolling means (moving average) rather than annual mean?
  mutate(status = pmin(1, ma5yr/target_secchi))
secchi_status <- group_by(secchi_status_allyrs, subbasin)

## Alternative status calculation: Status calculated using BHI region means
# secchi_status_allyrs <- bhi_id_target_secchi %>%
#   rename(subbasin = Subbasin, bhi_id = BHI_ID) %>%
#   mutate(status = pmin(1, mean_secchi/target_secchi))
# secchi_status <- group_by(secchi_status_allyrs, bhi_id)

## select last year of data for status in each basin or bhi region
## this means status year differs by basin!
## joining to basin lookup expands from subbasins to one row per BHI region
secchi_status <- secchi_status %>%
  filter(year == max(year)) %>%
  mutate(status = round(status*100, 2)) %>% # keep decimal places, will round when combine eut indicators
  ungroup() %>%
  dplyr::select(score = status, !!!syms(intersect(c("subbasin", "bhi_id"), names(secchi_status)))) %>% 
  mutate(dimension = "status", indicator = "secchi") %>% 
  left_join(basin_lookup) %>%
  mutate(region = as.factor(rgn_nam))

## plot the status
## BHI region plots will look the same, as BHI status are taken from subbasin means
## unless alternative calculation is used, based on BHI region means rather than subbasin means
set.seed(2)
statuspal <- colorRampPalette(fullpal)(80)[sample(1:80, size = 18)]

ggplot(secchi_status %>% left_join(secchi_last_year, by = c("subbasin" = "Subbasin"))) +
  geom_col(
    aes(region, score, fill = subbasin),
    position = position_dodge(), 
    color = "grey", 
    alpha = 0.6,
    size = 0.2,
    show.legend = FALSE
  ) + 
  geom_text(aes(region, score, label = last_year), color = "slategray", size = 3) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 9), plot.caption = element_text(hjust = 0)) +
  scale_fill_manual(values = statuspal) +
  labs(
    title = "Secchi Indicator Status by Region",
    caption = paste(
      "Summer mean Secchi Depth divided by basin target,",
      "scaled between 0-100 via multiplication by 100.\n",
      "The status score reflects the state of the goal in the Sub-basin,",
      "between worst-possible and 'Good Environmental Status'.\n",
      "A status of 100 indicates target (GES) is achieved."
    )
  )
```

![](eut_prep_files/figure-gfm/eut%20secchi%20indicator%20status%20calculation-1.png)<!-- -->
<br>

#### 3.1.5 Trend Calculation

In our approach here, we use a 10 year period to calculate the trend
with a minimum of 5 data points. In most cases, the BHI framework uses a
5 year period for the trend, but as Secchi is a slow response variable,
we use a longer time period. The trend value is the slope of the linear
regression (mean offshore summer Secchi depth per year) multiplied by
the year of future interest (5 years from status year) and this value is
constrained to between -1 and 1.

The trend plots show Secchi depth measurements by basin, with a red line
indicating where the linear model predicts mean Secchi depths will be
five years in the future, and a blue line indicating current assessment
year summer mean. Vertical lines frame the 5-year time frame over which
trend is projected.

``` r
trendyrs <- min_year:(min_year + regr_length)

## linear model from the basin mean secchi values ----

secchi_lm_estim <- secchi_status_allyrs %>%
  distinct(subbasin, year, mean_secchi) %>%
  filter(year %in% trendyrs) %>% 
  group_by(subbasin) %>%
  ## calculate trend only if:
  ## have at least X yrs data (min_regr_length) in the last Y yrs (regr_length) of time series
  do(tail(., n = regr_length)) %>%
  do(data.frame(enough_data = ifelse(sum(!is.na(.$mean_secchi)) >= min_regr_length, TRUE, FALSE))) %>%
  right_join(secchi_status_allyrs %>% filter(year %in% trendyrs), by  = "subbasin") %>%
  group_by(subbasin, enough_data, target_secchi) %>%
  do(trend_mdl = lm(mean_secchi ~ year, data = .))

## trend plots ----

trendplotdf <- secchi_offshore %>%
  rename(subbasin = Subbasin) %>%
  left_join(secchi_status_allyrs) %>% 
  filter(year %in% trendyrs) %>% 
  left_join(
    mutate(
      secchi_lm_estim,
      current = predict(trend_mdl, data.frame(year = max(trendyrs))),
      prj = predict(trend_mdl, data.frame(year = max(trendyrs) + future_year))
    )
  )
ggplot(trendplotdf) +
  geom_point(aes(x = year, y = secchi_m), size = 0.2, alpha = 0.2) +
  geom_point(aes(x = year, y = mean_secchi), size = 0.6) +
  geom_vline(xintercept = max(trendyrs), size = 0.2) +
  geom_vline(xintercept = max(trendyrs) + future_year, size = 0.2) +
  geom_hline(aes(yintercept = prj), color = "tomato") +
  geom_hline(aes(yintercept = current), color = "lightsteelblue") +
  facet_wrap(~subbasin) +
  labs(x = NULL, y = NULL) +
  ggtitle("Offshore Secchi with 5 Years in Future and Current Estimates (Red and Blue lines Respectively)")
```

![](eut_prep_files/figure-gfm/eut%20secchi%20indicator%20trend%20calculation-1.png)<!-- -->

``` r
## trend score dataframe ----

## assign basin trend to BHI regions
secchi_trend <- secchi_lm_estim %>% 
    mutate(
      expectedchange5yrs = coef(trend_mdl)["year"]*future_year,
      ## divide expected 5 year change in secchi by respective basin targets
      ## interpretation of trend is roughly expected (percent) change in status
      trend_score = max(-1, min(1, expectedchange5yrs/target_secchi))
    ) %>%
  right_join(basin_lookup, by = "subbasin") %>% 
  mutate(
    score = round(ifelse(enough_data, trend_score, NA), 3),
    region = rgn_nam,
    dimension = "trend",
    indicator = "secchi"
  ) %>% 
  select(score, subbasin, dimension, indicator, bhi_id, rgn_nam, HELCOM_ID, region)
```

<br>

#### 3.1.5 Status and Trend Timeseries Plots

**Basin status**

As a ratio between summer mean and basin target, the sub-basin status is
initially a value between 0 and 1. This is later rescaled to the 0 to
100 range via multiplication by 100. The status is calculated for each
(data) year between 2000 and 2019.

``` r
plotdf <- secchi_status_allyrs %>% 
  distinct(subbasin, year, status, n_pts) %>% 
  mutate(year = as.factor(year))
ggplot(plotdf) + 
  geom_col(aes(year, status, fill = n_pts), alpha = 0.7) +
  facet_wrap(~subbasin) +
  scale_fill_viridis_c() +
  geom_hline(yintercept = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = NULL, y = "Secchi Indicator Status\n", fill = "Num. Contributing of Points") +
  ggtitle("Yearly Secchi Status by Subbasin")
```

![](eut_prep_files/figure-gfm/timeseries%20plot%20of%20basin%20eut%20secchi%20status-1.png)<!-- -->

<br>

**Plot region Status and Trend values**

Status values can range from 0-100 – this is the status for the *most
recent* set of 5 years. In most cases this is 2015-2019.

``` r
ggplot(rbind(secchi_status, secchi_trend)) + 
  geom_col(aes(region, score, fill = subbasin), alpha = 0.5, show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~dimension, scales = "free_x", nrow = 1) +
  labs(x = "BHI Region\n", y = NULL) +
  ggtitle("Secchi Status and Trend Scores by Region") +
  scale_fill_manual(values = statuspal) +
  theme(axis.text.y = element_text(size = 7))
```

![](eut_prep_files/figure-gfm/secchi%20status%20and%20trend%20for%20regions%20plot-1.png)<!-- -->

<br>

#### 3.1.6 Save Secchi depth eutrophication layer

``` r
write_csv(
  secchi_offshore %>% 
    select(
      region_id = BHI_ID,
      lat, lon, 
      month, year,
      secchi_depth = secchi_m
    ), 
  file.path(dir_layers, sprintf("cw_eut_secchi_bhi%s.csv", assess_year))
)
```

<br>

### 3.2 Chlorophyll a Indicator

#### 3.2.1 Match BHI regions

The raw chlorophyll a data (consisting of merged ICES, SMHI, and Baltic
Nest data) must be assigned to their respective Sub-basins (along with
HELCOM Coastal WFD /Offshore division codes) and BHI Regions based on
their latitude/longitude coordinates. Then the data are subsetted to
assure only summer months (June-September), years 2000 onward, and
measurements from the surface layer (0 - 10 m depth) are included.

``` r
## assign helcom offshore codes
# nrow(filter(chla_rawdata, is.na(lat) | is.na(lon))) # first check if any lat/lon have NAs
chla_rgns_w_offshore_code <- chla_rawdata %>% 
  filter(!is.na(lat) & !is.na(lon)) %>%
  sf::st_as_sf(
    crs = 4326,
    coords = c("lon", "lat"),
    remove = FALSE
  ) %>%  
  sf::st_join(sf::st_transform(offshore_codes, 4326)) %>% 
  select(-HELCOM_ID, -Subbasin)

## spatial join also the WFD water bodies
chla_rgns_w_offshore_code <- chla_rgns_w_offshore_code %>% 
  sf::st_join(
    coastal_codes %>% 
      sf::st_transform(4326) %>% 
      rename(area_km2_wfd = Area_km2, code = HELCOM_ID) %>% 
      select(area_km2_wfd, code)
  ) %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(lat) & !is.na(lon))
  

## assign bhi basins to chla data 
chla_rgns_shp <- join_rgns_info(
  chla_rgns_w_offshore_code, helcomID_col = "helcom_id", country_col = "country", 
  latlon_vars = c("^lat", "^lon"), return_spatial = FALSE, 
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefile"), buffer_shp = NULL
)

## select summer months (June-September), surface layer (0 - 10 m depth), Years >= 2000
chla_rgns <- chla_rgns_shp %>% 
  mutate(month = stringr::str_pad(month, 2, "left", "0")) %>% 
  filter(month == "06" | month == "07" | month == "08" | month == "09") %>% 
  filter(depth_m <= 10) %>% 
  filter(year >= 2000) %>% 
  mutate(supplier = stringr::str_to_upper(supplier)) %>% 
  mutate(coastal = ifelse(is.na(helcom_coastal_code), "Coastal", "Offshore")) %>% 
  ## some cases where code SEA-010 is matched with Northern Baltic Proper, 
  ## by our spatial join should be code SEA-012,
  ## happens because our polygons are different than HELCOM's
  mutate(code = as.character(code)) %>% 
  mutate(area_km2_wfd = ifelse(
    code == "SEA-010" & Subbasin == "Northern Baltic Proper", 
    unique(filter(chla_rgns_shp, code == "SEA-012")$area_km2_wfd), 
    area_km2_wfd
  )) %>% 
  mutate(code = ifelse(code == "SEA-010" & Subbasin == "Northern Baltic Proper", "SEA-012", code))

## Gdansk and The Quark have only coastal, no offshore data points
distinct(chla_rgns, Subbasin, HELCOM_ID, coastal) %>% 
  group_by(HELCOM_ID) %>% 
  mutate(n = n()) %>% 
  arrange(n, Subbasin)

## not all coastal regions have data... 76 of 257 do not have data
length(levels(coastal_codes$HELCOM_ID))
coastalareas_nodata <- setdiff(unique(coastal_codes$HELCOM_ID), as.character(unique(chla_rgns$code)))
length(coastalareas_nodata)

## 17 of 257 are offshore, 240 are coastal
length(levels(coastal_codes$HELCOM_ID)[grep("SEA-[0-9]{3}", levels(coastal_codes$HELCOM_ID))])

## 224 of the 240 coastal areas have corresponding chla coastal thresholds, 16 do not
length(unique(chla_coastal_thresholds$code))
coastal_nothresh <- setdiff(
  levels(coastal_codes$HELCOM_ID)[grep("SEA-[0-9]{3}", levels(coastal_codes$HELCOM_ID), invert = TRUE)], 
  unique(chla_coastal_thresholds$code)
)

## coastal areas with no data, but which do have threshold
coastalareas_nodata[!coastalareas_nodata %in% coastal_nothresh]

## coastal areas with data but no thresholds
levels(coastal_codes$HELCOM_ID)[levels(coastal_codes$HELCOM_ID) %in% coastal_nothresh]
```

<br>

**Checking coastal and offshore observations**

For the chlorophyll a indicator, coastal and offshore data points must
be associated with different thresholds. Coastal versus offshore points
are distinguished using [HELCOM Coastal and Offshore divisions (2018
version)](https://maps.helcom.fi/arcgis/rest/services/MADS/Sea_environmental_monitoring/MapServer/4),
and each category (coastal and offshore) gives important information
regarding the status of Eutrophication in the Baltic Sea. However, not
every coastal area has sufficient data and/or a threshold value, and
therefore coastal data must be excluded at this time from our analysis.

``` r
## make basemap
basemap <- ggplot2::ggplot(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) + 
  geom_sf(size = 0.1, color = "burlywood", alpha = 0.4) +
  scale_x_continuous(limit = c(4, 32)) +
  scale_y_continuous(limit = c(53.5, 66)) +
  theme(panel.background = element_rect(fill = "#F8FBFC", color = "#E2EEF3"))
```

``` r
pal <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(257)[sample(1:257)]
coastal_simple <- rmapshaper::ms_simplify(input = coastal_codes) %>% sf::st_as_sf()
## note: DEN-034, DEN-035, DEN-040 are eliminated by simplification of polygons for plotting
## coastal_simple cannot be used for analysis; is only used for plotting

## coastal regions which are not accounted for i.e. no data matched to these
coastalareas_nodata_map <- basemap +
  geom_sf(
    mapping = aes(fill = Name),
    data = filter(coastal_simple, HELCOM_ID %in% coastalareas_nodata),
    color = "snow", size = 0.05,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = pal)


## checking chla_rgns observations, coastal/offshore locations
nrow(chla_rgns) # 38386

## observations with no coast_code and no BHI_ID
## distinct locations
loc_no_coastcode_nobhi <- chla_rgns %>% 
  filter(is.na(helcom_coastal_code) & is.na(BHI_ID))%>%
  dplyr::select(lat, lon)%>%
  distinct() # 588

## these locations are only coastal or outside of the Baltic Sea
coastal_noid <- basemap +
  geom_sf(
    data = sf::st_as_sf(
      loc_no_coastcode_nobhi, 
      coords = c("lon", "lat"), 
      crs = 4326
    ),
    size = 1.8, alpha = 0.2
  )

## any coastal observations have NA for bhi id / how many with non-NA bhi id
nrow(filter(chla_rgns, coastal == "Coastal" & is.na(BHI_ID))) # 9658
nrow(filter(chla_rgns, coastal == "Coastal" & !is.na(BHI_ID))) # 23636


## observations with NA for the coast_code but having BHI_ID
## distinct locations
loc_bhi_no_coastcode <- chla_rgns %>% 
  filter(is.na(helcom_coastal_code) & !is.na(BHI_ID)) %>% 
  dplyr::select(lat, lon) %>%
  distinct() # 1506

## no coastal code, all coastal stations
coastal_with_id <- basemap +
  geom_sf(
    data = sf::st_as_sf(
      loc_bhi_no_coastcode, 
      coords = c("lon", "lat"), 
      crs = 4326
    ),
    size = 1.8, alpha = 0.2
  )

## filter data that are only offshore, coast_code == 0 and having BHI_ID
chla_offshore <- filter(chla_rgns, helcom_coastal_code == 0, !is.na(BHI_ID))
nrow(chla_offshore) # 5087
chla_offshore_spatial <- basemap +
  geom_sf(
    mapping = aes(colour = supplier),
    data = sf::st_as_sf(
      chla_offshore, 
      coords = c("lon", "lat"), 
      crs = 4326
    ),
    size = 1.8, alpha = 0.2
  ) +
  labs(x = NULL, y = NULL, colour = "Data Source") +
  theme(legend.position = c(0.9, 0.1)) +
  scale_color_manual(values = c("tomato", "aquamarine2", "gold"))
## taking only offshore data is a substantial reduction in the number of observations, 
## especially in the northern Baltic 


## Check coastal observations
chla_coastal <- filter(chla_rgns, is.na(helcom_coastal_code) & !is.na(BHI_ID))
nrow(chla_coastal) # 23636
chla_coast_spatial <- basemap +
  geom_sf(
    mapping = aes(colour = supplier),
    data = sf::st_as_sf(
      chla_coastal, 
      coords = c("lon", "lat"), 
      crs = 4326
    ),
    size = 1.8, alpha = 0.2
  ) +
  labs(x = NULL, y = NULL, colour = "Data Source") +
  theme(legend.position = c(0.9, 0.1)) +
  scale_color_manual(values = c("tomato", "aquamarine2", "gold"))

## Look at the complete dataset, including both coastal and offshore locations
chla_whole <- filter(chla_rgns, !is.na(BHI_ID))
nrow(chla_whole) # 28723

plot_chla_whole <- sf::st_as_sf(
  mutate(chla_whole, code_na = is.na(code)),
  coords = c("lon", "lat"), 
  crs = 4326
)

chla_whole_spatial_codeareas <- basemap +
  geom_sf(
    mapping = aes(fill = Name),
    data = coastal_simple,
    alpha = 0.7, color = "snow", size = 0.05,
    show.legend = FALSE
  ) +
  geom_sf(
    mapping = aes(shape = code_na),
    data = filter(plot_chla_whole, code_na),
    size = 4, shape = 3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = pal)
## missing codes for a few points in the inner coastal areas
## estuarine areas near Göteborg, Västervik, Rostock, Haapsalu, Luleå

chla_missing_data_or_targets <- basemap +
  ## areas with no data
  geom_sf(
    mapping = aes(fill = Name),
    data = filter(coastal_simple, HELCOM_ID %in% coastalareas_nodata),
    alpha = 0.7, color = "snow", size = 0.05,
    show.legend = FALSE
  ) +
  ## areas with no threshold
  geom_sf(
    data = filter(coastal_simple, HELCOM_ID %in% coastal_nothresh),
    fill = NA, color = "black", size = 0.3, 
    show.legend = FALSE
  ) +
  ## overlay data points locations
  # geom_sf(
  #   data = plot_chla_whole,
  #   size = 0.01, alpha = 0.5, color = "red"
  # ) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = c(0.9, 0.1)) +
  scale_fill_manual(values = pal)
```

![](eut_prep_files/figure-gfm/chla%20offshore%20coastal%20data%20maps-1.png)<!-- -->

<br/>

#### 3.2.2 Visualize Chrorophyll a Data

**Visualization of Chl a data (whole Baltic i.e. both Coastal and
Offshore)**

The four plots generated in this section show temporal trends (top) and
monthly distributions (bottom) by subbasin, for Offshore (right) and
Coastal (left) chlorophyll a, with data source indicated by color. Pink
points correspond to ICES data, turquoise points to Nest data, and gold
to SMHI data.

``` r
plotinfo <- list(
  list("chla_offshore", c("BHI_ID", "Subbasin", "rgn_nam"), "Chlorophyll a (Offshore) by BHI Region", "bhi"),
  list("chla_offshore", "Subbasin", "Chlorophyll a (Offshore) by Subbasin", "basin")
)

## expand cases for equal number of offshore and coastal plots, so visual comparison is easier 
expandcases <- tidyr::expand(chla_whole, tidyr::nesting(Subbasin, BHI_ID, year, month, coastal))
lapply(
  plotinfo,
  function(x){
    ## 1. evaluate data by source
    plottimeseries <- ggplot(full_join(get(x[[1]]), expandcases)) +
      geom_point(
        aes(year, chla_ug_l, color = supplier), 
        size = 0.5, alpha = 0.5, show.legend = FALSE
      ) +
      labs(x = "Year", y = "Chl a (ug/L)", color = "Data Source", title = x[[3]]) +
      scale_color_manual(values = c("tomato", "aquamarine2", "gold"))
    
    ## 2. evaluate data by by coastal/offshore
    # plottimeseries <- ggplot(chla_whole) + 
    #   geom_point(aes(year, chla_ug_l, color = coastal), size = 0.5, alpha = 0.5) +
    #   labs(x = "Year", y = "Chl a (ug/L)", color = "Coastal/Offshore", title = x[[3]]) +
    #   scale_color_manual(values = c("lightsteelblue", "sandybrown")) 
    
    plottimeseries <- plottimeseries + 
      facet_wrap(
        x[[2]], 
        labeller = label_wrap_gen(width = 35, multi_line = FALSE), 
        ncol = 3
      ) 
    assign(sprintf("plot_%s_%s_ts", x[[1]], x[[4]]), plottimeseries, envir = .GlobalEnv)
    createdplots <- sprintf("plot_%s_%s_ts", x[[1]], x[[4]])
  }
)
lapply(
  plotinfo,
  function(x){
    ## 1. evaluate data by source
    plotdf <- left_join(
      get(x[[1]]) %>% 
        full_join(expandcases) %>% 
        mutate(month = as.factor(month)),
      data.frame(month = c("06","07","08","09"), Month = c("Jun","Jul","Aug","Sep"))
    )
    plotdf$Month <- factor(plotdf$Month, levels = c("Jun","Jul","Aug","Sep"))
    
    plotboxplot <- ggplot(plotdf) +
      geom_boxplot(
        aes(Month, chla_ug_l, fill = supplier), 
        size = 0.2, outlier.size = 0.3, alpha = 0.4, show.legend = FALSE
      ) +
      labs(x = NULL, y = "Chl a (ug/L)", fill = "Data Source", title = x[[3]]) +
      scale_fill_manual(values = c("tomato", "aquamarine2", "gold"))
    
    ## 2. evaluate data by by coastal/offshore
    # plotboxplot <- ggplot(mutate(chla_whole, month = as.factor(month))) +
    #   geom_boxplot(aes(month, chla_ug_l, fill = coastal), size = 0.2, outlier.size = 0.3, alpha = 0.4) +
    #   labs(x = "Month", y = "Chl a (ug/L)", fill = "Coastal/Offshore", title = x[[3]]) +
    #   scale_fill_manual(values = c("lightsteelblue", "sandybrown")) + 
    #   ggtitle("Chlorophyll a Monthly")
    
    plotboxplot <- plotboxplot +
      facet_wrap(
        x[[2]], 
        labeller = label_wrap_gen(width = 35, multi_line = FALSE), 
        ncol = 3
      )
    assign(sprintf("plot_%s_%s_box", x[[1]], x[[4]]), plotboxplot, envir = .GlobalEnv)
    createdplots <- sprintf("plot_%s_%s_box", x[[1]], x[[4]])
  }
)
```

``` r
# gridExtra::grid.arrange(
#   plot_chla_offshore_bhi_ts, plot_chla_coastal_bhi_ts,
#   plot_chla_offshore_bhi_box, plot_chla_coastal_bhi_box, 
#   nrow = 2
# )
gridExtra::grid.arrange(
  plot_chla_offshore_basin_ts,
  plot_chla_offshore_basin_box,
  nrow = 1
)
```

![](eut_prep_files/figure-gfm/coastal%20and%20offshore%20chla%20plots%20by%20basin-1.png)<!-- -->

<br>

#### 3.2.3 Mean Chlorophyll a Calculation

*Chlorophyll a* concentration averages are calculated per basin or BHI
region, using offshore measurements (coastal measurements were added in
a separate category just for the analyses). Means are calculated for
each coastal and offshore categories, first within each summer month.
Then the monthly means are averaged across to get an overall summer mean
in each the coastal and offshore category. Variances (standard
deviations squared) are also calculated within each month, then averaged
to get an average variance across summer months for the subbasin or BHI
region and coastal/offshore category. This is presented as a standard
deviation (square root of variance across months) on the plots below.

Along with the mean calculation, we calculate five-year moving averages.
The moving average is an average of summer means from the current and
previous four years.

``` r
## Calculate mean monthly value for each summer month
## For coastal and offshore separately
chla_monthly_means <- chla_whole %>% 
  filter(!is.na(code)) %>% 
  ## now group and take means
  group_by(year, month, coastal, Subbasin, code, area_km2_wfd) %>%
  summarise(
    mean_chla = mean(chla_ug_l, na.rm = TRUE),
    chla_sd = sd(chla_ug_l, na.rm = TRUE),
    n_pts = n()
  ) %>% 
  mutate(chla_sd = ifelse(is.na(chla_sd), 0, chla_sd))

## Calculate summer mean chl a
## each year will have multiple means, for coastal areas and an offshore (open-sea) subbasin
## https://stats.stackexchange.com/questions/25848/how-to-sum-a-standard-deviation
chla_summer_months_mean <- chla_monthly_means %>% 
  group_by(year, coastal, Subbasin, code, area_km2_wfd) %>%
  summarise(
    mean_chla = mean(mean_chla, na.rm = TRUE), 
    chla_sd = sqrt(mean((chla_sd^2), na.rm = TRUE)),
    n_pts = sum(n_pts)
  ) %>% 
  ungroup() %>% 
  mutate(code = as.character(code))

## calculate min and max, and rolling 5 year mean
chla_summer_months_mean <- chla_summer_months_mean %>% 
  arrange(code, coastal, year) %>% 
  group_by(code, Subbasin, coastal) %>%
  mutate(ma5yr = zoo::rollapply(mean_chla, 5, mean, na.rm = TRUE, align = "right", fill = NA)) %>% 
  mutate(chla_min = mean_chla-chla_sd, chla_max = mean_chla+chla_sd)


## DIFFERENT TARGETS FOR COASTAL (WFD AREAS) AND OFFSHORE
wfd_area_target_chla <- file.path(dir_prep, "supplement", "lookup_tabs", "rgns_complete.csv") %>% 
  read_csv() %>% 
  dplyr::select(basin = subbasin, code = helcom_id) %>% 
  distinct() %>% 
  left_join(eut_thresholds, by = "basin") %>% 
  dplyr::select(code, target_chla = summer_chla) %>% 
  mutate(units = "ug_per_l") %>% 
  bind_rows(rename(chla_coastal_thresholds, target_chla = threshold)) %>% 
  ## join with basin-specific eutrophication chlorophyll a targets
  right_join(chla_summer_months_mean, by = "code")


## Plot summer mean secchi
chla_meansplot <- wfd_area_target_chla %>% 
  ggplot(aes(year, mean_chla, group = code)) +  
  geom_errorbar(
    aes(x = year, ymin = chla_min, ymax = chla_max), 
    alpha = 0.6, size = 0.3, color = "snow", width = 0.3,
    show.legend = FALSE
  ) +
  geom_point(size = 0.3, color = "snow", show.legend = FALSE) +
  geom_line(aes(year, ma5yr), size = 0.2, color = "snow", show.legend = FALSE) + 
  geom_point(
    data = filter(wfd_area_target_chla, str_detect(code, "^SEA-")),
    size = 0.7
  ) +
  geom_line(
    data = filter(wfd_area_target_chla, str_detect(code, "^SEA-")),
    aes(year, ma5yr), 
    size = 0.3
  ) + 
  facet_wrap(~Subbasin, scales = "free", ncol = 4) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Chlorophyll a (ug/L)") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    strip.text.y = element_text(size = 6)
  ) +
  theme_dark() 
```

<br>

**Plot summer chlorophyll a with target values indicated**

The following plot shows annual mean summer chlorophyll a concentrations
(ug/l). Horizontal lines are HELCOM target values, as summarized in the
table at the top of section 3. With one row per subbasin, the three
columns show Coastal, Offshore, and Combined summer mean chlorophyll a
concentrations.

``` r
chla_meansplot + ggtitle(sprintf("Mean Summer (June-Sept) Chlorophyll a by WFD Area, organized by Subbasin"))
```

![](eut_prep_files/figure-gfm/summer%20chl%20a%20with%20target-1.png)<!-- -->

<br>

#### 3.2.3 Status Calculations: Coastal & Offshore

**Explore years in data available for status calculation**

Only two basins have data for 2019, which will be used as the status
year, while thirteen have data for 2018 and two for 2016. For these
basins, their most recent year’s data is used for current status
calculations.

We use the summer mean rather than the 5 year moving average because,
when excluding coastal data, the most recent year for Kiel Bay and The
Sound would become 2010, and the following would have no values in our
compiled dataset to base status calculation on: Bothnian Bay, Bothnian
Sea, The Quark, Aland Sea, Gdansk Basin.

Using annual summer means and only offshore data however, we are still
missing The Quark and Gdansk Basin.

``` r
## get the last year of non-NA data
chla_last_year <- wfd_area_target_chla %>%
  filter(!is.na(mean_chla), str_detect(code, "^SEA-")) %>%
  group_by(code, Subbasin) %>%
  summarise(last_year = last(year))

## which are not in 2019
# filter(chla_last_year, last_year < 2019)
```

<br>

**Status calculation with mean summer Chla by basin, from in situ
measurements**

Status is calculated on the basin level and then applied to all regions.
It is calculated in data prep for additional visualization and
evaluation, but the final method of status calculation is also completed
by the EUT function in functions.R of the relevant `bhi` repository
assessment folder.

Status for the *chlorophyll a* indicator is calculated as `basin target`
divided by `basin summer means`, with the value capped at 1. We
calculate status for each Coastal, Offshore, and Combined categories for
evaluation, but select the Combined version for the indicator status.
The plot shows BHI region scores (uniform across regions within the same
basin, as the data is aggregated by basin for the summer means
calculation), and also indicates which data year the status is based on.

``` r
## Define constants for status and trend calculations
min_year <- 2009 # earliest year to use as a start for regr_length timeseries
regr_length <- 10 # number of years to use for regression
future_year <- 5 # the year at which we want the likely future status
min_regr_length <- 5 # min actual number of years with data to use for regression
```

``` r
## calculate for each offshore and coastal separately, then combine
## quick plot, distributions of means
ggplot(wfd_area_target_chla) + 
  geom_histogram(aes(mean_chla), bins = 80, fill = NA, color = "grey") + 
  facet_grid(cols = vars(coastal), scales = "free_x") + 
  labs(x = "Mean Chlorphyll a Concentration", y = NULL)
```

![](eut_prep_files/figure-gfm/eut%20chla%20status%20calculation-1.png)<!-- -->

``` r
## Calculate basin status as: basin_target/basin_mean, constraining values to 1 as maximum score
## ratio inverted compared to secchi because levels of chl a above target are undesireable
chla_status_allyrs <- wfd_area_target_chla %>%
  rename(subbasin = Subbasin) %>% 
  mutate(status = pmin(1, target_chla/mean_chla))
  ## or use five year rolling means (moving average) rather than annual mean?
  # mutate(status = pmin(1, target_chla/ma5yr))
chla_status <- group_by(chla_status_allyrs, coastal, subbasin, code)

## select last year of data for status in each basin, this means status year differs by basin
## joining to basin lookup expands from subbasins to one row per BHI region
chla_status <- chla_status %>% 
  filter(year == max(year)) %>% 
  mutate(status = round(status*100, 2)) %>% # keep decimal places, will round when combine eut indicators
  ungroup() %>% 
  dplyr::select(score = status, code, coastal, subbasin) %>% 
  mutate(dimension = "status", indicator = "chla") %>% 
  left_join(basin_lookup) %>%
  mutate(region = as.factor(rgn_nam))

## plot the status
## BHI region plots will look the same, as BHI status are taken from subbasin means
## unless alternative calculation is used, based on BHI region means rather than subbasin means
set.seed(2)
statuspal <- colorRampPalette(fullpal)(80)[sample(1:80, size = 18)]

chlaplotdf <- chla_status %>% 
  filter(coastal == "Offshore") %>% 
  left_join(chla_last_year, by = c("subbasin" = "Subbasin", "code"))

ggplot(chlaplotdf) + 
  geom_col(
    aes(region, score, fill = subbasin),
    position = position_dodge(), 
    color = "grey", 
    alpha = 0.6,
    size = 0.2,
    show.legend = FALSE
  ) + 
  geom_text(aes(region, score, label = last_year), color = "slategray", size = 3) +
  coord_flip() +
  facet_grid(cols = vars(coastal)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 9)) +
  scale_fill_manual(values = statuspal) +
  ggtitle("Chlorophyll a Status by Region, for Coastal, Offshore, and Combined data")
```

![](eut_prep_files/figure-gfm/eut%20chla%20status%20calculation-2.png)<!-- -->

``` r
chla_status <- chla_status %>% 
  filter(coastal == "Offshore") %>% 
  select(-coastal, -code)
```

#### 3.2.4 Trend Calculations: Coastal & Offshore

In our approach here, we use a 10 year period to calculate the trend
with a minimum of 5 data points. In most cases, the BHI framework uses a
5 year period for the trend, but as *chlorophyll a* is a slower response
variable, we use a longer time period. The trend value is the slope of
the linear regression (mean summer chlorophyll vs year) multiplied by
the year of future interest (5 years from status year) and this value is
constrained to between -1 and 1.

The linear regressions look very different when combining offshore and
coastal categories of data, e.g. for Western Gotland both offshore and
coastal categories separately have slight positive trends (increasing
chlorophyll a), but a linear regression using both offshore/coastal
combined would have a negative trend (decreasing chlorophyll a). For
this reason the *chlorophyll a* indicator trend dimension is calculated
as an average of trend scores calculated for coastal and offshore
chlorophyll a.

The trend plots show chlorophyll measurements by basin, with a red line
indicating where the linear model predicts mean Secchi depths will be
five years in the future, and a blue line indicating current assessment
year summer mean. Vertical lines frame the five-year time frame over
which the trend is projected.

``` r
trendyrs <- min_year:(min_year + regr_length)

## linear model from the basin mean chla values ----

chla_lm_estim <- chla_status_allyrs %>%
  filter(year %in% trendyrs) %>% 
  distinct(subbasin, code, coastal, year, mean_chla) %>%
  group_by(subbasin, code, coastal) %>%
  ## calculate trend only if:
  ## have at least X yrs data (min_regr_length) in the last Y yrs (regr_length) of time series
  do(tail(., n = regr_length)) %>%
  do(data.frame(enough_data = ifelse(sum(!is.na(.$mean_chla)) >= min_regr_length, TRUE, FALSE))) %>%
  right_join(chla_status_allyrs %>% filter(year %in% trendyrs), by = c("subbasin", "code", "coastal")) %>%
  group_by(subbasin, code, coastal, enough_data, target_chla) %>%
  do(trend_mdl = lm(mean_chla ~ year, data = .))

## trend plots ----

trendplotdf <- chla_whole %>%
  rename(subbasin = Subbasin) %>%
  left_join(chla_status_allyrs) %>% 
  filter(year %in% trendyrs) %>% 
  left_join(
    mutate(
      chla_lm_estim,
      current = predict(trend_mdl, data.frame(year = max(trendyrs))),
      prj = predict(trend_mdl, data.frame(year = max(trendyrs) + future_year))
    )
  ) %>% 
  filter(coastal %in% c("Offshore"))

maketrendplot <- function(trendplotdf){
  trendplot <- ggplot(trendplotdf) +
    geom_point(aes(x = year, y = chla_ug_l), size = 0.2, alpha = 0.2) +
    geom_point(aes(x = year, y = mean_chla), size = 0.6) +
    geom_vline(xintercept = max(trendyrs), size = 0.2) +
    geom_vline(xintercept = max(trendyrs) + future_year, size = 0.2) +
    geom_hline(aes(yintercept = prj), color = "tomato") +
    geom_hline(aes(yintercept = current), color = "lightsteelblue") +
    facet_wrap(~subbasin, scales = "free_y", ncol = 4) +
    labs(x = NULL, y = NULL)
  return(trendplot)
}
plottitle <- "Chlorophyll a with 5 Years in Future and Current Estimates (Red and Blue lines Respectively)"
maketrendplot(filter(trendplotdf, coastal == "Offshore")) + ggtitle(paste("Offshore", plottitle))
```

![](eut_prep_files/figure-gfm/eut%20chl%20a%20trend%20calculation-1.png)<!-- -->

``` r
## trend score dataframe ----

## assign basin trend to BHI regions
chla_trend <- chla_lm_estim %>% 
  mutate(
    expectedchange5yrs = coef(trend_mdl)["year"]*future_year,
    ## divide expected 5 year change in chla by respective basin target
    ## i.e get as proportion relative to target (target is status score of 100, so equivalent scales)
    ## then interpretation of trend is roughly expected (percent) change in status
    trend_score = max(-1, min(1, -expectedchange5yrs/target_chla))
  ) %>%
  ## only using offshore data for now:
  filter(coastal == "Offshore", enough_data) %>%
  group_by(subbasin, target_chla) %>% 
  summarize(trend_score = mean(trend_score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  right_join(basin_lookup, by = "subbasin") %>% 
  mutate(
    score = round(trend_score, 3),
    region = rgn_nam,
    dimension = "trend",
    indicator = "chla"
  ) %>% 
  select(score, subbasin, dimension, indicator, bhi_id, rgn_nam, HELCOM_ID, region)
```

#### 3.2.5 Status and Trend Timeseries Plots

**Basin status**

Basin status is initially a value between 0 and 1. Calculated for each
year between 2000 and 2020.

``` r
plotdf <- chla_status_allyrs %>% 
  filter(coastal == "Offshore") %>% 
  distinct(subbasin, year, status, n_pts)
ggplot(plotdf) + 
  geom_col(aes(year, status, fill = n_pts), alpha = 0.7) +
  facet_wrap(~subbasin) +
  scale_fill_viridis_c() +
  geom_hline(yintercept = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = NULL, y = "Chl a Indicator Status\n", fill = "Num. Contributing of Points") +
  ggtitle("Yearly Chlorophyll a Status by Subbasin")
```

![](eut_prep_files/figure-gfm/timeseries%20plot%20of%20basin%20eut%20chla%20status-1.png)<!-- -->

<br>

**Plot region Status and Trend values**

Status values can range from 0-100 – this is the status for the *most
recent* year. In most cases this is 2019. Current trends for Åland,
Bothnian Bay, Bothnian Sea, and Kiel Bay are based only on coastal data
trends, as our offshore data were not sufficient for fitting trends.

``` r
ggplot(rbind(chla_status, chla_trend)) + 
  geom_col(aes(region, score, fill = subbasin), alpha = 0.5, show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~dimension, scales = "free_x", nrow = 1) +
  labs(x = "BHI Region\n", y = NULL) +
  ggtitle("Chlorophyll a Status and Trend Scores by Region") +
  scale_fill_manual(values = statuspal) +
  theme(axis.text.y = element_text(size = 9))
```

![](eut_prep_files/figure-gfm/chlorphyl%20a%20status%20and%20trend%20for%20regions%20plot-1.png)<!-- -->

<br>

#### 3.2.6 Save chlorophyll a eutrophication layer

``` r
write_csv(
  chla_whole %>% 
    filter(!is.na(code)) %>% 
    select(
      region_id = BHI_ID,
      helcom_id = code,
      lat, lon, 
      month, year,
      chla_conc = chla_ug_l
    ), 
  file.path(dir_layers, sprintf("cw_eut_chla_bhi%s.csv", assess_year))
)
```

<br>

### 3.3 Nutrients Indicator

The nutrients indicator measures winter (December-February) dissolved
inorganic nitrogen (DIN: nitrate, nitrite, and ammonium) and winter
dissolved inorganic phosphorus (DIP) concentrations in the surface layer
(0 - 10 m depth).

#### 3.3.1 Match BHI regions

The raw nutrients data obtained from Baltic Nest include nitrate and
nitrite (measured separately or combined), ammonia, and phosphate.
Nitrate, nitrite, and ammonia are summed for DIN, and only phosphate is
used for DIP.

These points are assigned to their respective Subbasins (with HELCOM
Coastal/Offshore division codes) and BHI Regions based on their
latitude/longitude coordinates. Then the data are subsetted to assure
only winter months (December-February) and years 2000 onward are
included. A column ‘winter of year’ is created, to group e.g. January
and February 2006 with December 2005 rather than with December 2006.

``` r
nutrients_rawdata <- nutrients_rawdata %>% 
  mutate(nitrate_and_nitrite = ifelse(NO23N == 0, NO3N + NO2N, NO23N)) %>% 
  rename(
    nitrate = NO3N, nitrite = NO2N, ammonia = NH4N, phosphate = PO4P,
    latitude = LATITUDE, longitude = LONGITUDE, depth_m = OBSDEP, 
    date = OBSDATE, cruise = SHIP, time = OBSTIME
  ) %>%
  mutate(
    date = as.Date(date), 
    year = format(date, "%Y"), 
    month = format(date, "%m")
  ) %>% 
  select(-ID, -NO23N, -TOTN) %>% 
  mutate(din = nitrate_and_nitrite + ammonia, dip = phosphate)

## check for duplicates
## double entries, but measurements taken at different times
nutrients_rawdata %>%
  group_by(
    cruise, date, year, month, 
    latitude, longitude, depth_m,
    nitrate_and_nitrite, ammonia, phosphate
  ) %>%
  mutate(n = n()) %>%
  filter(n > 1)
  
## assign helcom offshore codes
# nrow(filter(nutrients_rawdata, is.na(latitude) | is.na(longitude))) # first check if any lat/lon have NAs
nut_rgns_w_offshore_code <- nutrients_rawdata %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  sf::st_as_sf(
    crs = 4326,
    coords = c("longitude", "latitude"),
    remove = FALSE
  ) %>%  
  sf::st_join(sf::st_transform(offshore_codes, 4326))

## spatial join also the WFD water bodies
nut_rgns_w_offshore_code <- nut_rgns_w_offshore_code %>% 
  sf::st_join(
    coastal_codes %>% 
      sf::st_transform(4326) %>% 
      rename(area_km2_wfd = Area_km2, code = HELCOM_ID) %>% 
      select(area_km2_wfd, code)
  ) %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(latitude) & !is.na(longitude))

## assign bhi basins to nutrients data 
buffer_sf <- st_read(
  file.path(dirname(dir_B), "Shapefiles", "BHI_shapefile_25km_buffer"),
  "BHI_shapefile_25km_buffer"
)
nut_rgns_shp <- join_rgns_info(
  nut_rgns_w_offshore_code, 
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefiles")
  # buffer_shp = buffer_sf
)

## confirm only winter months (December-February) are included
## surface layer (0 - 10 m depth) and years >= 2000
## also need to account for fact that winter spans across new year
nut_rgns <- nut_rgns_shp %>% 
  mutate(month = stringr::str_pad(month, 2, "left", "0")) %>% 
  filter(month == "12" | month == "01" | month == "02") %>% 
  filter(depth_m <= 10) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(winterofyear = ifelse(month == 12, year, year-1)) %>% 
  filter(winterofyear >= 2000)

## reshape some for next steps
nut_rgns <- nut_rgns %>% 
  mutate(coastal = ifelse(is.na(helcom_coastal_code), "Coastal", "Offshore")) %>% 
  select(
    latitude, longitude, depth_m,
    Subbasin, BHI_ID, code, coastal, 
    area_km2_wfd,
    winterofyear, year, month, time, 
    din, dip
  ) %>% 
  tidyr::pivot_longer(c("din", "dip"), names_to = "nutrient", values_to = "value") %>% 
  mutate(nutrient = stringr::str_to_upper(nutrient)) %>% 
  
  ## some cases where code SEA-010 is matched with Northern Baltic Proper, 
  ## by our spatial join should be code SEA-012,
  ## happens because our polygons are different than HELCOM's
  mutate(code = as.character(code)) %>% 
  mutate(area_km2_wfd = ifelse(
    code == "SEA-010" & Subbasin == "Northern Baltic Proper", 
    unique(filter(nut_rgns, code == "SEA-012")$area_km2_wfd), 
    area_km2_wfd
  )) %>% 
  mutate(code = ifelse(code == "SEA-010" & Subbasin == "Northern Baltic Proper", "SEA-012", code)) %>% 
  mutate(area_km2_wfd = ifelse(
    code == "SEA-001" & Subbasin == "The Sound", 
    unique(filter(nut_rgns, code == "SEA-003")$area_km2_wfd), 
    area_km2_wfd
  )) %>% 
  mutate(code = ifelse(code == "SEA-001" & Subbasin == "The Sound", "SEA-003", code)) 
```

<br>

#### 3.3.2 Visualize Nutrients Data

**Spatial Visualization of DIN and DIP data, Offshore vs Coastal
Measurements**

The map shows locations of offshore (pink) and coastal (green)
measurements. The two plots show temporal trends in offshore and coastal
nutrients (right) and monthly distributions (left) by subbasins. Pink
points correspond to offshore data, and green points to coastal data in
both the map and the
plots.

``` r
bhi_rgns_simple <- rmapshaper::ms_simplify(input = BHI_rgns_shp) %>% sf::st_as_sf()

nutrients_obs_mapdf <- nut_rgns %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  filter(winterofyear %in% 2010:2019) %>% 
  mutate(no_bhi_id = is.na(BHI_ID))

basemap +
  geom_sf(data = bhi_rgns_simple, fill = "white", size = 0.2, color = "burlywood") +
  # geom_sf(data = nutrients_obs_mapdf, aes(color = no_bhi_id), size = 1.8, alpha = 0.7, show.legend = FALSE) +
  geom_sf(data = nutrients_obs_mapdf, aes(color = coastal), size = 0.7, alpha = 0.2) +
  scale_color_manual(values = c("darkseagreen", "salmon")) +
  labs(color = "Offshore or Coastal Measurement") +
  theme_bw() +
  ggtitle("Offshore and Coastal Nutrient Measurements") +
  theme(legend.position = c(0.25, 0.9), legend.background = element_rect(fill = "ghostwhite"))
```

![](eut_prep_files/figure-gfm/eut%20nutrients%20map%20of%20raw%20data%20points-1.png)<!-- -->

``` r
## make dataframe to use in plotting
expandcases <- tidyr::expand(nut_rgns, tidyr::nesting(Subbasin, BHI_ID, winterofyear, month, nutrient))
plotdf <- nut_rgns %>% 
  full_join(expandcases) %>%
  filter(!(is.na(Subbasin) & is.na(nutrient)), !is.na(Subbasin)) %>% 
  left_join(data.frame(month = c("12","01","02"), Month = c("Dec","Jan","Feb")))
plotdf$Month <- factor(plotdf$Month, levels = c("Dec","Jan","Feb"))

for(r in c("Subbasin", "BHI_ID")){
  for(n in c("DIN", "DIP")){
    ## timeseries plots
    plottimeseries <- ggplot(filter(plotdf, nutrient == n)) +
      geom_point(
        aes(winterofyear, value, color = coastal), 
        size = 0.5, alpha = 0.6
      ) +
      labs(x = "\nWinter of Year", y = NULL, color = "Coastal/Offshore") +
      ggtitle(paste(n, "vs Year")) +
      scale_color_manual(values = c("darkseagreen", "salmon")) +
      facet_wrap(r, scales = "free_y", ncol = 3) +
      theme(axis.text.x = element_text(size = 7, angle = 90), legend.position = "bottom")
    assign(sprintf("plot_%s_%s_ts", str_to_lower(n), str_to_lower(r)), plottimeseries, envir = .GlobalEnv)
    
    ## boxplots comparing coastal/offshore by region or subbasin
    plotboxplot <- ggplot(filter(plotdf, nutrient == n)) +
      geom_boxplot(
        aes(Month, value, fill = coastal),
        size = 0.2, outlier.size = 0.3, alpha = 0.4, show.legend = FALSE
      ) +
      labs(x = NULL, y = NULL, fill = "Coastal/Offshore") +
      ggtitle(paste("Monthly", n)) +
      scale_fill_manual(values = c("darkseagreen", "salmon")) +
      facet_wrap(r, scales = "free_y", ncol = 3)
    assign(sprintf("plot_%s_%s_box", str_to_lower(n), str_to_lower(r)), plotboxplot, envir = .GlobalEnv)
  }
}

gridExtra::grid.arrange(plot_din_subbasin_ts, plot_dip_subbasin_ts, nrow = 1)
```

![](eut_prep_files/figure-gfm/nutrients%20data%20check%20coastal%20versus%20offshore-1.png)<!-- -->

<br>

#### 3.3.3 Mean Nutrient Calculations

Mean nutrient concentrations are calculated from coastal and offshore
measurements grouped by open-sea sub-basins and WFD coastal spatial
units. Means are calculated for each coastal or offshore zone, first
within each winter month. Then monthly means are averaged to get an
overall winter mean in each the coastal or offshore zone. Variances
(standard deviations squared) are also calculated within each month,
then averaged to get an average variance across winter months for the
subbasin and coastal/offshore category. This is presented as a standard
deviation (square root of variance across months) on the plots below.

Along with the mean calculation, we calculate five-year moving averages.
The moving average (or ‘rolling mean’) is an average of winter means
from the current and previous four winters.

However, the final scores for the nutrient indicators only consider the
offshore means, as the coastal thresholds available for both
[DIN](https://helcom.fi/media/core%20indicators/Dissolved-inorganic-nitrogen-DIN-HELCOM-core-indicator-2018.pdf)
and
[DIP](https://helcom.fi/media/core%20indicators/Dissolved-inorganic-phosphorus-DIP-HELCOM-core-indicator-2018.pdf)
include only Sweden, Latvia, and
Poland.

``` r
## check that each subbasin is actually properly matched with a single code
# nut_rgns %>% 
#   ungroup() %>% 
#   select(Subbasin, code) %>% 
#   distinct() %>% 
#   filter(str_detect(code, "SEA")) %>%
#   group_by(Subbasin) %>% 
#   count()

## Calculate DIN and DIP mean monthly values for each winter month
## For coastal and offshore separately
nutrient_monthly_means <- nut_rgns %>% 
  filter(!is.na(Subbasin), !is.na(BHI_ID), !is.na(code)) %>% 
  ## now group and take means
  group_by(winterofyear, month, coastal, nutrient, Subbasin, code, area_km2_wfd) %>%
  summarise(
    nut_mean = mean(value, na.rm = TRUE),
    nut_sd = sd(value, na.rm = TRUE),
    n_pts = n()
  ) %>% 
  mutate(nut_sd = ifelse(is.na(nut_sd), 0, nut_sd))

## Calculate winter DIN and DIP means
## each year will have multiple means, for coastal areas and an offshore (open-sea) subbasin
nut_winter_months_mean <- nutrient_monthly_means %>% 
  group_by(winterofyear, coastal, nutrient, Subbasin, code, area_km2_wfd) %>%
  summarise(
    nut_mean = mean(nut_mean, na.rm = TRUE), 
    nut_sd = sqrt(mean((nut_sd^2), na.rm = TRUE)),
    n_pts = sum(n_pts)
  )

## calculate min and max, and rolling 5 year mean
nut_winter_months_mean <- nut_winter_months_mean %>% 
  arrange(Subbasin, code, nutrient, coastal, winterofyear) %>% 
  group_by(Subbasin, code, nutrient, coastal) %>%
  mutate(ma5yr = zoo::rollapply(nut_mean, 5, mean, na.rm = TRUE, align = "right", fill = NA)) %>% 
  mutate(nut_min = nut_mean-nut_sd, nut_max = nut_mean+nut_sd)


## join with basin-specific eutrophication chlorophyll a targets
wfd_area_target_nut <- file.path(dir_prep, "supplement", "lookup_tabs", "rgns_complete.csv") %>% 
  read_csv() %>% 
  dplyr::select(basin = subbasin, code = helcom_id) %>% 
  distinct() %>% 
  left_join(eut_thresholds, by = "basin") %>% 
  dplyr::select(code, DIN = winter_DIN, DIP = winter_DIP) %>% 
  mutate(units = "umol_per_l") %>% 
  bind_rows(left_join(
      rename(din_coastal_thresholds, DIN = threshold),
      rename(dip_coastal_thresholds, DIP = threshold)
  )) %>% 
  ## join with basin-specific eutrophication nutrient a targets
  tidyr::pivot_longer(c("DIN", "DIP"), names_to = "nutrient", values_to = "target") %>% 
  right_join(nut_winter_months_mean, by = c("code", "nutrient"))


## Plot winter mean nutrients
for(n in c("DIN", "DIP")){
  meansplot <- ggplot(filter(wfd_area_target_nut, nutrient == n), aes(winterofyear, nut_mean, group = code)) + 
    geom_errorbar(
      aes(x = winterofyear, ymin = nut_min, ymax = nut_max), 
      alpha = 0.4, size = 0.3, color = "snow", width = 0.3,
      show.legend = FALSE
    ) +
    geom_point(size = 0.3, alpha = 0.7, color = "snow", show.legend = FALSE) +
    geom_line(size = 0.2, alpha = 0.7, color = "snow", show.legend = FALSE) + 
    geom_point(
      data = filter(wfd_area_target_nut, str_detect(code, "^SEA-"), nutrient == n),
      size = 0.7
    ) +
    geom_line(
      data = filter(wfd_area_target_nut, str_detect(code, "^SEA-"), nutrient == n),
      size = 0.3
    ) + 
    facet_wrap(~Subbasin, scales = "free", ncol = 4) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      strip.text.y = element_text(size = 6)
    ) +
    theme_dark() 
  assign(sprintf("%s_%s_meansplot", str_to_lower(n), "subbasin"), meansplot)
}
```

**Plot winter nutrients with target values indicated**

Horizontal lines are HELCOM target
values

``` r
din_subbasin_meansplot + ggtitle("Mean Winter Dissolved Inorganic Nitrogen (Dec-Feb) against Target values")
```

![](eut_prep_files/figure-gfm/winter%20nutrients%20with%20target-1.png)<!-- -->

``` r
dip_subbasin_meansplot + ggtitle("Mean Winter Dissolved Inorganic Phosphorus (Dec-Feb) against Target values")
```

![](eut_prep_files/figure-gfm/winter%20nutrients%20with%20target-2.png)<!-- -->

<br>

#### 3.3.4 Status Calculations: Coastal and Offshore

**Explore years in data available for status calculation**

For both DIN and DIP, no basins have data for 2019, which is the
assessment year; after excluding coastal spatial units, thirteen basins
have data for 2018 and for three (Åland, Gulf of Riga, The Quark) the
most recent year is 2017. Most recent ‘offshore’ data for The Sound is
2010. For each basin, the most recent year’s data in that region is used
for *current status* calculations.

``` r
## get the last year of non-NA DIN data
din_last_year <- wfd_area_target_nut %>%
  filter(!is.na(ma5yr), str_detect(code, "^SEA-"), nutrient == "DIN") %>%
  group_by(code, Subbasin) %>%
  summarise(last_year = last(winterofyear)) %>% 
  mutate(indicator = "din") 
## which are not in 2019
# filter(din_last_year, last_year < 2019)

## get the last year of non-NA DIP data
dip_last_year <- wfd_area_target_nut %>%
  filter(!is.na(ma5yr), str_detect(code, "^SEA-"), nutrient == "DIP") %>%
  group_by(code, Subbasin) %>%
  summarise(last_year = last(winterofyear)) %>% 
  mutate(indicator = "dip")
## which are not in 2019
# filter(chla_last_year, `Last Year` < 2019)

knitr::kable(
  bind_rows(
    din_last_year %>% 
      group_by(last_year, indicator) %>%
      summarize(`Num. subbasins with year` = n(), Subbasins = paste(Subbasin, collapse = ", ")),
    dip_last_year %>% 
      group_by(last_year, indicator) %>%
      summarize(`Num. subbasins with year` = n(), Subbasins = paste(Subbasin, collapse = ", "))
  ) %>% 
  mutate(indicator = str_to_upper(indicator)) %>% 
  rename(Indicator = indicator, `Last Year` = last_year)
)
```

| Last Year | Indicator | Num. subbasins with year | Subbasins                                                                                                                                                                                                         |
| --------: | :-------- | -----------------------: | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
|      2010 | DIN       |                        1 | The Sound                                                                                                                                                                                                         |
|      2017 | DIN       |                        3 | Gulf of Riga, Aland Sea, The Quark                                                                                                                                                                                |
|      2018 | DIN       |                       13 | Kattegat, Great Belt, Kiel Bay, Bay of Mecklenburg, Arkona Basin, Bornholm Basin, Gdansk Basin, Eastern Gotland Basin, Western Gotland Basin, Northern Baltic Proper, Gulf of Finland, Bothnian Sea, Bothnian Bay |
|      2010 | DIP       |                        1 | The Sound                                                                                                                                                                                                         |
|      2017 | DIP       |                        3 | Gulf of Riga, Aland Sea, The Quark                                                                                                                                                                                |
|      2018 | DIP       |                       13 | Kattegat, Great Belt, Kiel Bay, Bay of Mecklenburg, Arkona Basin, Bornholm Basin, Gdansk Basin, Eastern Gotland Basin, Western Gotland Basin, Northern Baltic Proper, Gulf of Finland, Bothnian Sea, Bothnian Bay |

<br>

**Status calculation with raw (non-modeled) mean winter nutrients by
basin**

Status is calculated on the basin level and then applied to all regions.
It is calculated in data prep for additional visualization and
evaluation, but the final method of status calculation is also completed
by the EUT function in functions.R of the relevant `bhi` repository
assessment folder.

Status for each the *DIN* and *DIP* indicators is calculated as `basin
target` divided by `basin winter means`, with the value capped at 1. We
calculate status using only Offshore data for evaluation, and using the
5 year moving average. The plot shows BHI region scores (uniform across
regions within the same basin, as the data is aggregated by basin for
the winter means calculation), and also indicates which data year the
status is based on.

``` r
## Define constants for status and trend calculations
min_year <- 2009 # earliest year to use as a start for regr_length timeseries
regr_length <- 10 # number of years to use for regression
future_year <- 5 # the year at which we want the likely future status
min_regr_length <- 5 # min actual number of years with data to use for regression
```

``` r
## distributions of means for offshore and coastal categories
ggplot(wfd_area_target_nut) + 
  geom_histogram(aes(nut_mean), bins = 100, fill = NA, color = "grey") + 
  facet_grid(rows = vars(coastal), cols = vars(nutrient), scales = "free_x") + 
  labs(x = "Mean Nutrient Concentrations", y = NULL)
```

![](eut_prep_files/figure-gfm/eut%20nutrients%20status%20calculation-1.png)<!-- -->

``` r
## Calculate basin status as: basin_target/basin_mean, constraining values to 1 as maximum score
nut_status_allyrs <- wfd_area_target_nut %>%
  rename(subbasin = Subbasin) %>% 
  # mutate(status = pmin(1, target/nut_mean))
  ## or use five year rolling means (moving average) rather than annual mean?
  mutate(status = pmin(1, target/ma5yr))
nut_status <- group_by(nut_status_allyrs, nutrient, coastal, subbasin, code)

## select last year of data for status in each subbasin, this means status year differs by basin
## joining to subbasin lookup expands from subbasins to one row per BHI region 
nut_status <- nut_status %>% 
  filter(winterofyear == max(winterofyear)) %>% 
  mutate(status = round(status*100, 2)) %>% # keep decimal places, will round when combine eut indicators
  ungroup() %>% 
  mutate(
    dimension = "status", 
    indicator = ifelse(nutrient == "DIP", "dip", "din")
  ) %>% 
  dplyr::select(score = status, indicator, dimension, coastal, subbasin, code) %>% 
  left_join(basin_lookup) %>%
  mutate(region = as.factor(rgn_nam))

## plot the status
## BHI region plots will look the same, as BHI status are taken from subbasin means
## unless alternative calculation is used, based on BHI region means rather than subbasin means
set.seed(2)
statuspal <- colorRampPalette(fullpal)(80)[sample(1:80, size = 18)]

plotdf <- nut_status %>% 
  filter(coastal == "Offshore") %>% 
  left_join(
    bind_rows(din_last_year, dip_last_year),
    by = c("subbasin" = "Subbasin", "indicator", "code")
  ) %>% 
  mutate(indicator = ifelse(indicator == "dip", "DIP", "DIN"))

ggplot(plotdf) +
  geom_col(
    aes(region, score, fill = subbasin),
    position = position_dodge(), 
    color = "grey", 
    alpha = 0.6,
    size = 0.2,
    show.legend = FALSE
  ) + 
  geom_text(aes(region, score, label = last_year), color = "slategray", size = 3) +
  coord_flip() +
  facet_grid(cols = vars(coastal), rows = vars(indicator)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 7)) +
  scale_fill_manual(values = statuspal) +
  ggtitle("DIN and DIP indicators Status by Region, for Coastal, Offshore, and Combined data")
```

![](eut_prep_files/figure-gfm/eut%20nutrients%20status%20calculation-2.png)<!-- -->

``` r
nut_status <- nut_status %>% 
  filter(coastal == "Offshore") %>% 
  select(-coastal, -code)
```

<br>

#### 3.3.4 Trend Calculations: Coastal & Offshore

In our approach here, we use a 10 year period to calculate the trend
with a minimum of 5 data points. In most cases, the BHI framework uses a
5 year period for the trend, but as dissolved inorganic nutrients are
slower response variables, we use a longer time period. The trend value
is the slope of the linear regression (mean winter dissolved inorganic
nutrients vs year) multiplied by the year of future interest (5 years
from status year) and this value is constrained to between -1 and 1.

We use the same approach as with the Chlorophyll indicator, calculating
trend as an average of trend scores calculated for coastal and offshore
DIN or DIP.

The trend plots show (inorganic) nutrient measurements by basin, with a
red line indicating where the linear model predicts mean Secchi depths
will be five years in the future, and a blue line indicating current
assessment year summer mean. Vertical lines frame the five-year time
frame over which trend is projected.

**Nutrients with 5 Years in Future and Current Estimates (Red and Blue
lines Respectively)**

``` r
trendyrs <- min_year:(min_year + regr_length)

## linear model from the basin mean nutrient values ----

nut_lm_estim <- nut_status_allyrs %>%
  filter(winterofyear %in% trendyrs) %>% 
  distinct(subbasin, code, coastal, nutrient, winterofyear, nut_mean) %>%
  group_by(subbasin, code, coastal, nutrient) %>%
  ## calculate trend only if:
  ## have at least X yrs data (min_regr_length) in the last Y yrs (regr_length) of time series
  do(tail(., n = regr_length)) %>%
  do(data.frame(enough_data = ifelse(sum(!is.na(.$nut_mean)) >= min_regr_length, TRUE, FALSE))) %>%
  right_join(nut_status_allyrs, by = c("subbasin", "code", "coastal", "nutrient")) %>%
  group_by(subbasin, code, coastal, nutrient, enough_data, target) %>%
  do(trend_mdl = lm(nut_mean ~ winterofyear, data = .))

## trend plots ----

trendplotdf <- nut_rgns %>%
  filter(!is.na(Subbasin)) %>% 
  rename(subbasin = Subbasin) %>%
  left_join(nut_status_allyrs) %>% 
  filter(winterofyear %in% trendyrs) %>% 
  left_join(
    mutate(
      nut_lm_estim,
      current = predict(trend_mdl, data.frame(winterofyear = max(trendyrs))),
      prj = predict(trend_mdl, data.frame(winterofyear = max(trendyrs) + future_year))
    )
  ) %>% 
  filter(coastal %in% c("Offshore"))

maketrendplot <- function(trendplotdf){
  trendplot <- ggplot(trendplotdf) +
    geom_point(aes(x = winterofyear, y = value), size = 0.2, alpha = 0.2) +
    geom_point(aes(x = winterofyear, y = nut_mean), size = 0.6) +
    geom_vline(xintercept = max(trendyrs), size = 0.2) +
    geom_vline(xintercept = max(trendyrs) + future_year, size = 0.2) +
    geom_hline(aes(yintercept = prj), color = "tomato") +
    geom_hline(aes(yintercept = current), color = "lightsteelblue") +
    facet_wrap(~subbasin, scales = "free_y", ncol = 4) +
    labs(x = NULL, y = NULL)
  return(trendplot)
}
gridExtra::grid.arrange(
  maketrendplot(filter(trendplotdf, coastal == "Offshore", nutrient == "DIN")) + ggtitle("Offshore DIN"),
  maketrendplot(filter(trendplotdf, coastal == "Offshore", nutrient == "DIP"))+ ggtitle("Offshore DIP"),
  nrow = 2
)
```

![](eut_prep_files/figure-gfm/eut%20nutrients%20indicators%20trend%20calculation-1.png)<!-- -->

``` r
## trend score dataframe ----

## assign basin trend to BHI regions
nut_trend <- nut_lm_estim %>% 
  mutate(
    expectedchange5yrs = coef(trend_mdl)["winterofyear"]*future_year,
    ## divide expected 5 year change in DIN or DIP by respective basin target
    ## i.e get as proportion relative to target (target is status score of 100, so equivalent scales)
    ## then interpretation of trend is roughly expected (percent) change in status
    trend_score = max(-1, min(1, -expectedchange5yrs/target))
  ) %>% 
  ungroup() %>% 
  ## only using offshore (open-sea) data for now:
  filter(coastal == "Offshore", enough_data) %>%
  group_by(subbasin, target, nutrient) %>% 
  summarize(trend_score = mean(trend_score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  right_join(basin_lookup, by = "subbasin") %>% 
  mutate(
    score = round(trend_score, 3),
    region = rgn_nam,
    dimension = "trend",
    indicator = str_to_lower(nutrient)
  ) %>% 
  select(score, subbasin, dimension, indicator, bhi_id, rgn_nam, HELCOM_ID, region)
```

<br>

#### 3.3.5 Status and Trend Timeseries Plots

**Basin status**

Basin status is initially a value between 0 and 1. Calculated for each
year between 2000 and 2019.

``` r
plotdf <- nut_status_allyrs %>% 
  filter(coastal == "Offshore") %>% 
  distinct(subbasin, winterofyear, nutrient, status, n_pts)

gridExtra::grid.arrange(
  ggplot(filter(plotdf, nutrient == "DIN")) + 
    geom_col(aes(winterofyear, status, fill = n_pts), alpha = 0.7) +
    facet_wrap(~subbasin) +
    scale_fill_viridis_c() +
    geom_hline(yintercept = 1) +
    labs(x = NULL, y = "DIN Indicator Status\n", fill = "Num. Contributing of Points") +
    ggtitle("Yearly DIN Status by Subbasin"),
  ggplot(filter(plotdf, nutrient == "DIP")) + 
    geom_col(aes(winterofyear, status, fill = n_pts), alpha = 0.7) +
    facet_wrap(~subbasin) +
    scale_fill_viridis_c() +
    geom_hline(yintercept = 1) +
    labs(x = NULL, y = "DIP Indicator Status\n", fill = "Num. Contributing of Points") +
    ggtitle("Yearly DIP Status by Subbasin"),
  nrow = 2
)
```

![](eut_prep_files/figure-gfm/timeseries%20plot%20eut%20nutrients%20status-1.png)<!-- -->

<br>

**Plot region Status and Trend values**

Status values can range from 0-100 – this is the status for the *most
recent* year. In most cases this is 2019. Note: Kiel Bay trend for both
DIN and DIP is based only on offshore data as coastal data were not
sufficient to fit a trend.

``` r
rbind(nut_status, nut_trend) %>% 
  mutate(indicator = stringr::str_to_upper(indicator)) %>% 
  filter(!is.na(indicator)) %>% 
  ggplot() + 
  geom_col(aes(region, score, fill = subbasin), alpha = 0.5, show.legend = FALSE) +
  coord_flip() +
  facet_grid(cols = vars(dimension), rows = vars(indicator), scales = "free_x") +
  labs(
    x = "BHI Region\n", 
    y = NULL,
    title = "DIN and DIP Status and Trend Scores by Region",
    caption = "Negative trend values correspond to increasing nutrient concentrations."
  ) +
  scale_fill_manual(values = statuspal) +
  theme(axis.text.y = element_text(size = 7), plot.caption = element_text(hjust = 0))
```

![](eut_prep_files/figure-gfm/nutrient%20status%20and%20trend%20for%20regions%20plot-1.png)<!-- -->

<br>

#### 3.3.6 Save dissolved inorganic nutrients eutrophication layers

``` r
write_csv(
  nut_rgns %>% 
    filter(!is.na(code), nutrient == "DIN") %>% 
    select(
      region_id = BHI_ID,
      helcom_id = code,
      latitude, longitude,
      month, year,
      din_conc = value
    ), 
  file.path(dir_layers, sprintf("cw_eut_din_bhi%s.csv", assess_year))
)
write_csv(
  nut_rgns %>% 
    filter(!is.na(code), nutrient == "DIP") %>% 
    select(
      region_id = BHI_ID,
      helcom_id = code,
      latitude, longitude,
      month, year,
      dip_conc = value
    ), 
  file.path(dir_layers, sprintf("cw_eut_dip_bhi%s.csv", assess_year))
)
```

<br>

### 3.4 Oxygen Debt

For details on data preparation for oxygen debt layer, see [this github
repository](https://github.com/OHI-Baltic/HEAT), forked from ICES github
page for the HEAT assessment tool for fitting oxygen debt profiles to
oxygen data measurements. Oxygen data measurements used were obtained
from ICES and merged as documented in section
2.2.4.

``` r
heat_output <- "https://raw.githubusercontent.com/OHI-Baltic/HEAT/master/analysis/output/"
oxydebt <- read_csv(paste0(heat_output,"OxygenDebt/uncorrected_indicator_table_by_year_2000_2019.csv"))
```

#### 3.4.1 Match BHI regions

``` r
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
  full_join(basin_lookup) %>% 
  left_join(select(eut_thresholds, subbasin = basin, oxydebt_target = oxyg_debt))
```

<br>

#### 3.4.2 Status Calculaion

``` r
## Define constants for status and trend calculations
min_year <- 2009 # earliest year to use as a start for regr_length timeseries
regr_length <- 10 # number of years to use for regression
future_year <- 5 # the year at which we want the likely future status
min_regr_length <- 5 # min actual number of years with data to use for regression
```

``` r
## Calculate basin status as: basin_mean/basin_target, constraining values to 1 as maximum score
oxydebt_status_allyrs <- oxydebt_rgns %>%
  filter(year >= min_year) %>% 
  mutate(status = pmin(1, oxydebt_target/ES)) %>% 
  group_by(subbasin)

## select last year of data for status in each subbasin
oxydebt_status <- oxydebt_status_allyrs %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  dplyr::select(score = status, subbasin, bhi_id, rgn_nam, HELCOM_ID) %>%
  mutate(dimension = "status", indicator = "oxydebt") %>% 
  mutate(region = as.factor(rgn_nam))

## plot the status
set.seed(2)
statuspal <- colorRampPalette(fullpal)(80)[sample(1:80, size = 18)]

ggplot(oxydebt_status) +
  geom_col(
    aes(region, score, fill = subbasin),
    position = position_dodge(), 
    color = "grey", 
    alpha = 0.6,
    size = 0.2,
    show.legend = FALSE
  ) + 
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 9)) +
  scale_fill_manual(values = statuspal) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Oxygen Debt Indicator Status by Region")
```

![](eut_prep_files/figure-gfm/eut%20oxy%20debt%20status%20calculation-1.png)<!-- -->
<br>

#### 3.4.3 Trend Calculation

#### 3.4.4 Status and Trend Timeseries Plots

**Basin status**

Basin status is initially a value between 0 and 1. Calculated for each
year between 2000 and 2019.

**Plot region Status and Trend values**

Status values can range from 0-100 – this is the status for the *most
recent* year. In most cases this is 2019. Note: Kiel Bay trend for both
DIN and DIP is based only on offshore data as coastal data were not
sufficient to fit a trend.

<br>

#### 3.3.6 Save oxygen debt eutrophication layer

``` r
write_csv(
  oxydebt_rgns %>% 
    select(
      region_id = bhi_id,
      helcom_id = HELCOM_ID,
      year,
      oxygendebt = ES
    ), 
  file.path(dir_layers, sprintf("cw_eut_oxydebt_bhi%s.csv", assess_year))
)
```

<br>

## 4\. Visualizing Data Layers

### 4.1 Status and Trend Maps by Subbasin

``` r
statusmapcols <- c("indianred", "coral", "goldenrod1", "khaki", "lightblue", "steelblue")
trendmapcols <- c("indianred", "coral", "white", "lightblue", "steelblue")
```

<br>

**Secchi Status and Trend Maps**

``` r
mapdf_secchi <- left_join(
  bhi_rgns_simple %>% 
    mutate(Subbasin = as.character(Subbasin)),
  rbind(secchi_status, secchi_trend) %>% 
    select(Subbasin = subbasin, BHI_ID = bhi_id, score, dimension, region),
  by = c("Subbasin", "BHI_ID")
)
status_secchi_map <- basemap + 
  geom_sf(data = filter(mapdf_secchi, dimension == "status"), aes(fill = score), size = 0.1, alpha = 0.7) +
  scale_fill_gradientn(colors = statusmapcols, limits = c(0, 100), na.value = "gainsboro") +
  theme(
    legend.background = element_rect(color = "grey"),
    legend.position = c(0.15, 0.75)
  ) +
  labs(fill = "Score", title = "Secchi Status Map")

trend_secchi_map <- basemap + 
  geom_sf(data = filter(mapdf_secchi, dimension == "trend"), aes(fill = score), size = 0.1, alpha = 0.7) +
  scale_fill_gradientn(colors = trendmapcols, limits = c(-0.5, 0.5), na.value = "gainsboro") +
  theme(
    legend.background = element_rect(color = "grey"),
    legend.position = c(0.15, 0.75)
  ) +
  labs(fill = "Score", title = "Secchi Trend Map")

gridExtra::grid.arrange(status_secchi_map, trend_secchi_map, nrow = 1)
```

![](eut_prep_files/figure-gfm/secchi%20status%20and%20trend%20maps-1.png)<!-- -->

<br>

**Chl a Status and Trend Maps**

``` r
mapdf_chla <- left_join(
  bhi_rgns_simple %>% 
    mutate(Subbasin = as.character(Subbasin)),
  rbind(chla_status, chla_trend) %>% 
    select(Subbasin = subbasin, BHI_ID = bhi_id, score, dimension, region),
  by = c("Subbasin", "BHI_ID")
)
status_chla_map <- basemap + 
  geom_sf(data = filter(mapdf_chla, dimension == "status"), aes(fill = score), size = 0.1, alpha = 0.7) +
  scale_fill_gradientn(colors = statusmapcols, limits = c(0, 100), na.value = "gainsboro") +
  theme(
    legend.background = element_rect(color = "grey"),
    legend.position = c(0.15, 0.75)
  ) +
  labs(fill = "Score", title = "Chlorophyll a Status Map")

trend_chla_map <- basemap + 
  geom_sf(data = filter(mapdf_chla, dimension == "trend"), aes(fill = score), size = 0.1, alpha = 0.7) +
  scale_fill_gradientn(colors = trendmapcols, limits = c(-0.5, 0.5), na.value = "gainsboro") +
  theme(
    legend.background = element_rect(color = "grey"),
    legend.position = c(0.15, 0.75)
  ) +
  labs(fill = "Score", title = "Chlorophyll a Trend Map")

gridExtra::grid.arrange(status_chla_map, trend_chla_map, nrow = 1)
```

![](eut_prep_files/figure-gfm/chla%20status%20and%20trend%20maps-1.png)<!-- -->
<br>

**DIN and DIP Status and Trend Maps**

``` r
mapdf_nut <- left_join(
  bhi_rgns_simple %>% 
    mutate(Subbasin = as.character(Subbasin)),
  rbind(nut_status, nut_trend) %>% 
    select(Subbasin = subbasin, BHI_ID = bhi_id, score, dimension, indicator, region),
  by = c("Subbasin", "BHI_ID")
)
lapply(
  list(
    list("status", "din", "statusmapcols"), list("trend", "din", "trendmapcols"),
    list("status", "dip", "statusmapcols"), list("trend", "dip", "trendmapcols")
  ),
  function(x){
    if(x[[1]] == "status"){lims = c(0, 100)} else {lims = c(-0.5, 0.5)}
    assign(
      sprintf("%s_%s_map", x[[1]], x[[2]]), 
      basemap + 
        geom_sf(
          data = filter(mapdf_nut, dimension == x[[1]], indicator == x[[2]]), 
          aes(fill = score), 
          size = 0.1, alpha = 0.7
        ) +
        scale_fill_gradientn(
          colors = get(x[[3]]), 
          limits = lims, 
          na.value = "gainsboro"
        ) +
        theme(
          legend.background = element_rect(color = "grey"),
          legend.position = c(0.15, 0.75)
        ) +
        labs(fill = "Score", title = paste(str_to_upper(x[[2]]), str_to_title(x[[1]]), "Map")), 
      envir = .GlobalEnv
    )
    createdplots <- sprintf("%s_%s_map", x[[1]], x[[2]])
  }
)
```

``` r
gridExtra::grid.arrange(
  status_din_map, trend_din_map, 
  status_dip_map, trend_dip_map, 
  nrow = 2
)
```

![](eut_prep_files/figure-gfm/eut%20nutrients%20status%20and%20trend%20maps-1.png)<!-- -->

<br>

## 5\. Considerations for `BHI3.0`

**Regarding coastal thresholds for nutrients**: Could use the nationally
set nutrient indicators and their targets in the coastal areas. In some
cases that could be DIN\&DIP, in other cases total nutrients; in some
cases for the winter period, sometimes summer, sometimes all-year.

<br>

## 6\. References

[HELCOM Eutrophication Supplementary
report](http://stateofthebalticsea.helcom.fi/wp-content/uploads/2019/09/BSEP156-Eutrophication.pdf).
HELCOM (2018): HELCOM Thematic assessment of eutrophication 2011-2016.
Baltic Sea Environment Proceedings No. 156

[HELCOM Chlorophyll a core
indicator](https://helcom.fi/media/core%20indicators/Chlorophyll-a-HELCOM-core-indicator-2018.pdf).
HELCOM (2018) Chlorophyll-a. HELCOM core indicator report. Online. ISSN
2343-2543

[HELCOM Water clarity core
indicator](https://helcom.fi/media/core%20indicators/Water-clarity-HELCOM-core-indicator-2018.pdf).
HELCOM (2018) Water clarity. HELCOM core indicator report. Online. ISSN
2343-2543

[HELCOM Oxygen debt core
indicator](https://helcom.fi/media/core%20indicators/Oxygen-debt-HELCOM-core-indicator-2018.pdf).
HELCOM (2018) Oxygen debt. HELCOM core indicator report. Online. ISSN
2343-2543

[HELCOM Dissolved Inorganic Nitrogen core
indicator](https://helcom.fi/media/core%20indicators/Dissolved-inorganic-nitrogen-DIN-HELCOM-core-indicator-2018.pdf).
HELCOM (2018) Dissolved inorganic nitrogen (DIN). HELCOM core indicator
report. Online. ISSN 2343-2543

[HELCOM Dissolved Inorganic Phosphorus core
indicator](https://helcom.fi/media/core%20indicators/Dissolved-inorganic-phosphorus-DIP-HELCOM-core-indicator-2018.pdf).
HELCOM (2018) Dissolved inorganic phosphorus (DIP). HELCOM core
indicator report. Online. ISSN 2343-2543 <br>
