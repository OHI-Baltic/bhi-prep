Bottom Trawling Pressure Layer Data Preparation
================

## 1\. Background

Bottom Trawling can negatively affect the benthic life, in fact it can
affect the score for the Biodiversity (BD) goal.

## 2\. Data

This prep document is used to generate and explore the following data
layer:

  - `hab_bottom_trawl_bhi2019.csv`

This is saved to the `layers/v2019` folder and is derived from or
informed by the raw datasets from HELCOM database.

<br>

``` r
source(file.path(here::here(), "R", "data.R"))
source(file.path(here::here(), "R", "common.R"))
source(file.path(here::here(), "R", "spatial.R"))

## root location of the raw data
dir_B <- file.path(dirname(dir_prep), "bhi-data", "BHI 2.0")
dir_rawdata <- file.path(dir_B, "Pressure", "bottom_trawling")
```

## 2\. Data

### 2.1 Data sources

#### 2.1.1 Fishing Effort mobile bottom-contacting gear

<!-- dataset save location BHI_share/BHI 2.0/Pressure/bottom_trawling/ -->

| Option                          | Specification                                            |
| :------------------------------ | :------------------------------------------------------- |
| Pressures and Human Activities: | Fisheries                                                |
| Effort (2009-2013):             | Fishing Effort mobile bottom-contacting gear (2010-2013) |
| **There is no updated data**    |                                                          |

Source: [HELCOM Map and Data
Service](http://metadata.helcom.fi/geonetwork/srv/eng/catalog.search#/metadata/7986ee68-389c-4126-b36f-15407f61cbb5)
<br/>

<br/>

**HELCOM metadata**  
*The following text is copied directly from the [HELCOM
metadata](http://maps.helcom.fi/website/getMetadata/htm/All/Fishing%20effort%20mobile%20bottom-contacting%20gear%202013.htm)
file*  
"This dataset describes fishing effort (hours/c-square) for mobile
bottom-contacting gear based on VMS/Log book data processed by ICES
Working Group on Spatial Fisheries Data (WGSFD).

HELCOM requires spatially explicit information on fishing activity
affecting the Baltic Sea marine ecosystem for policy purposes. In order
to obtain this information a joint ICES/HELCOM/OSPAR data call was
issued to relevant authorities of contracting parties to deliver
information on fishing activity based on VMS/Log book data. The raw data
was submitted to ICES and processed to advice data products by ICES
Working Group for Spatial Fisheries (WGSFD) as requested by HELCOM.
Processing of the raw data requires specific resources, knowledge and
guarantee of anonymity for specific vessels, thus the process was done
by ICES WGSFD following Conditions for VMS data use. In 2015 ICES
collated Vessel Monitoring System (VMS) and logbook data received; data
from Russia were not received. ICES provided to HELCOM advice as fishing
abrasion pressure maps as well as fishing effort maps.

The correct data product citation is following: [ICES. 2015. Fishing
abrasion pressure maps for mobile bottom-contacting gears in HELCOM
area](http://ices.dk/sites/pub/Publication%20Reports/Data%20outputs/HELCOM_mapping_fishing_intensity_and_effort_data_outputs_2015.zip).

**Data caveates**  
When using the data for analysis/assessments the following caveats need
to be taken into consideration:The methods for identifying fishing
activity from the VMS data varied between countries; therefore there may
be some country-specific biases that ICES cannot evaluate. Additionally,
activities other than active towing of gear may have been incorrectly
identified as fishing activity. This would have the effect of
overestimating the apparent fishing intensity in ports and in areas used
for passage.The data for 2012 and 2013 is not directly comparable to the
data of previous years in the data call (2010–2011) due to the gradual
increase in VMS-enabled vessels in the range of 12–15 m. This is likely
to be most relevant when examining trends in effort for inshore areas.
Many countries have substantial fleets of smaller vessels that are not
equipped with VMS (The fishing abrasion pressure methodology is based on
very broad assumptions in terms of the area affected by abrasion. A
single speed and gear width was applied across each gear category in
most cases, which can lead to both underestimates and overestimates in
actual surface and subsurface abrasion.

#### 2.1.2 Fishing intensity Dredge 2016

This dataset describes fishing intensity for Dredge mobile bottom
contacting gear in 2016 based on VMS/Log book data processed by ICES
Working Group on Spatial Fisheries Data (
[WGSFD](http://ices.dk/community/groups/Pages/WGSFD.aspx)).

\_(For further technical information on the advice request, see
<http://www.ices.dk/sites/pub/Publication%20Reports/Advice/2017/Special_requests/helcom.2017.18.pdf)_>

ICES secretariat collected during 2017 relevant VMS and logbook data for
2009-2016 to produce, as a technical service to HELCOM, updated spatial
data layers on fishing intensity/pressure. Improved data quality control
checks were implemented. Submitted data across the HELCOM area have
improved in quality compared to previous data 2009-2013 published in
2015. Standardized methods were used to produce the requested data
layers. The code used to create the data products is available here:
<https://github.com/ices-eg/wg_WGSFD>

<!-- dataset save location BHI_share/BHI 2.0/Pressure/bottom_trawling/ -->

| Option                          | Specification                    |
| :------------------------------ | :------------------------------- |
| Pressures and Human Activities: | Fisheries                        |
| Intensity (2009-2016):          | Fishing intensity Dredge 2016 v2 |

Source: [HELCOM Map and Data
Service](http://metadata.helcom.fi/geonetwork/srv/eng/catalog.search#/metadata/dd24a7a8-3552-4bfd-b3a5-ce436a7be237)
<br/>

*We decided to use these data, as is seems the latest compilation for
the whole Baltic (this is under “Intesity” instead of “Effort”)*

**Data caveates** Several caveats, listed below, should be taken into
account when considering this technical service and its data. These
caveats relate to issues concerning the provision of vessel data and its
interpretation, and the scale at which data are informative. - Data on
fishing locations for vessels less than 12 m is not available and are
therefore not included in the technical service. This introduces a bias
in the assessment that is expected to be strongest in coastal areas. -
VMS data from Russia were not supplied, introducing a downward bias in
pressure in areas fished by Russia. The greatest bias will be in Russian
waters. - Fishing pressure (SAR, swept area reatio) depends on the
spatial resolution of the fishing pressure data. Pressure is calculated
at a resolution of 0.05 × 0.05 degrees. - It is possible that the
valuation of landings has been treated differently by different
countries, potentially introducing bias. - Data outputs represent
vessels over 15 m (2009–2011) and vessels over 12 m (2012–2015). - Data
outputs in this technical service assume a uniform distribution of
trawling within each c-square. When using the data products of this
technical service it should be noted that the above assumption will
apply when trawling is evaluated over longer time periods
(e.g. 2012–2015). However, at shorter, yearly time scales the
proportion of the sea floor trawled will be overestimated because
trawling is randomly distributed at small spatial scales (Rijnsdorp et
al., 1998; Ellis et al., 2014; Eigaard et al., 2016).

### 2.2 Data attributes

**SubsurfSAR**: Subsurface area ratio (Swept area, subsurface =\>2 cm
penetration depth of the gear components) will be the parameter used to
calculate the pressure for the bottom trawling (differently from the BHI
1.0, were ‘fishing hours per area’ was used from the fishing effort
data).

#### 2.2.1 Spatial reference information

*Provided in the HELCOM file*  
Spatial Reference, ArcGIS coordinate system

Type: Projected  
Geographic coordinate reference:GCS\_ETRS\_1989  
Projection: ETRS\_1989\_LAEA

Coordinate reference details:  
Well-known identifier:3035  
X origin: -8426600  
Y origin: -9526700  
XY scale: 10000  
Z origin: 0  
Z scale: 1  
M origin: 0  
M scale: 1  
XY tolerance: 0.001  
Z tolerance: 0.001  
M tolerance: 0.001  
High precision: true  
Latest well-known identifier: 3035  
Well-known text:
PROJCS\[“ETRS\_1989\_LAEA”,GEOGCS\[“GCS\_ETRS\_1989”,DATUM\[“D\_ETRS\_1989”,SPHEROID\[“GRS\_1980”,6378137.0,298.257222101\]\],PRIMEM\[“Greenwich”,0.0\],UNIT\[“Degree”,0.0174532925199433\]\],PROJECTION\[“Lambert\_Azimuthal\_Equal\_Area”\],PARAMETER\[“False\_Easting”,4321000.0\],PARAMETER\[“False\_Northing”,3210000.0\],PARAMETER\[“Central\_Meridian”,10.0\],PARAMETER\[“Latitude\_Of\_Origin”,52.0\],UNIT\[“Meter”,1.0\],AUTHORITY\[“EPSG”,3035\]\]

HELCOM secretariat did the following processing to the shapefile
provided by ICES: - Conversion to ETRS89LAEA coordinate system - Added
feature and attribute “Reported” to display unreported areas.

<br/>

## 3\. Prepare bottom trawl pressure layer

### 3.1 Read in Data and Explore

``` r
## read in bhi shapefile and bottom trawling pressure data from 2009
bhi <- sf::st_read(file.path(dir_A, "Shapefiles", "BHI_shapefile", "BHI_shapefile.shp"))
```

    ## Reading layer `BHI_shapefile' from data source `/Users/andreadecervo/github/bhi-data/Shapefiles/BHI_shapefile/BHI_shapefile.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 42 features and 6 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 9.420778 ymin: 53.60164 xmax: 30.34708 ymax: 65.90708
    ## CRS:            4326

``` r
# CRS: 4326

trawling_raw <- sf::st_read(file.path(dir_rawdata, "HELCOM_intensity_Dredge_2016_v2", "HELCOM_intensity_Dredge_2016_v2.shp"))
```

    ## Reading layer `HELCOM_intensity_Dredge_2016_v2' from data source `/Users/andreadecervo/github/bhi-data/BHI 2.0/Pressure/bottom_trawling/HELCOM_intensity_Dredge_2016_v2/HELCOM_intensity_Dredge_2016_v2.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 111 features and 13 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 4216363 ymin: 3521695 xmax: 5438130 ymax: 4320324
    ## CRS:            3035

``` r
# CRS: 3035

## change trawling coord. system to the bhi coord system
trawling_tranfs <- sf::st_transform(trawling_raw, crs = 4326)
```

### 3.2 Intersect/Join trawling effort with BHI shapefiles

``` r
# trawling_bhi <- sf::st_intersection(
#   st_geometry(trawling_tranfs), 
#   st_geometry(bhi)
# )

# join the two adjusted shapefiles
trawling_bhi <- sf::st_join(
  trawling_tranfs, 
  bhi
)

## save intersected shapefile
# st_write(trawling_bhi, dsn = path.expand(dir_intermediate), layer = 'trawling_bhi_raw', driver = 'ESRI Shapefile', overwrite = TRUE) 
```

## 4\. Pressure Model

Overlay trawl effort with BHI shape files so can assign intensity per
each BHI region.

### 4.1 Current pressure data

Use most recent year: 2016  
Xtrawl\_r\_y = sum hours within a BHI region in each year *y* in each
region *r* / area of BHI region *r* (hours/km2)

### 4.2 Rescale data

Calculate Xtrawl\_r\_y for all data years.

min value = 0

max value = max(Xtrawl\_r\_y) in any area  
*It should be discussed with the team if this is okay, or if think even
higher pressure in the past, should take pressure\_max =
max(XtrawL\_r\_y) x 1.2 or some greater percentage?*

for now:

rescaled value = (max - current) / (max - min)

### 4.3 Read joined shapefile and explore it

### 4.3.1 Extract swept area (SubsurfSAR) by BHI region

**Only 2 BHI regions have reported swept area: 3 and
4**

``` r
trawling_bhi_raw <- sf::st_read(file.path(dir_intermediate, "trawling_bhi_raw.shp"))
```

    ## Reading layer `trawling_bhi_raw' from data source `/Users/andreadecervo/github/bhi-prep/data/pressures/bottom_trawling/v2019/intermediate/trawling_bhi_raw.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 122 features and 19 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 8.3 ymin: 54.4367 xmax: 30.25205 ymax: 60.77241
    ## CRS:            4326

``` r
# st_geometry(trawling_bhi_raw) <- NULL

swept_area <- trawling_bhi_raw %>%
  mutate(swept = ifelse(SubsurfSAR >= 0, SubsurfSAR, NA)) %>% # the negative values are considered "missing info"
  filter(!is.na(BHI_ID)) %>% # those with not assigned BHI_ID are points out of the Baltic Sea area
  filter(!is.na(swept)) %>% 
  dplyr::select(swept, BHI_ID) %>%
  group_by(BHI_ID) 
```

### 4.5 Pressure layer for BHI regions

``` r
# rescale swept_area
bottom_trawling_score <- swept_area %>%
  mutate(max = max(swept), 
         min = min(swept),
         score = round((swept - min) / (max - min), 1),
         score = ifelse(is.na(score), min, score)) %>% # only one value for bhi_id = 4
  group_by(BHI_ID) %>% 
  summarize(mean_score = round(mean(score, na.rm = TRUE), 3)) %>% 
  dplyr::select(BHI_ID, 
         score = mean_score) 

# join bhi regions and include final pressure scores
bottom_trawling_score <- full_join(bhi_basin_lookup, bottom_trawling_score, 
                                   by = "BHI_ID") %>% 
  mutate(score = ifelse(is.na(score), 0, score)) %>% 
  dplyr::select(BHI_ID, 
                pressure_score = score)

# save pressure layer
# write_csv(bottom_trawling_score, file.path(dir_layers, 'hab_bottom_trawl_bhi2019.csv')) 
```

## 5\. Considerations for `BHI3.0`

## 6\. References
