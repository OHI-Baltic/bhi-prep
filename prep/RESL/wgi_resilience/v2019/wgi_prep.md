Worldwide Governance Indicators (WGI) Resilience Layer Data Preparation
================

<br>

``` r
## root location of the raw data
source(here::here("R", "setup.R"))
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
source(here::here("R", "data.R"))
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     intersect, setdiff, union

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## Loading required package: foreach

    ## Loading required package: iterators

    ## Loading required package: parallel

``` r
source(here::here("R", "spatial.R"))
```

    ## Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE, results = "hide", fig.width = 9.5, fig.height = 6)
loc <- here::here("prep", "resilience", version_year, "wgi")
data_path <- here::here("data", "resilience", version_year, "wgi_data.rmd")

dir_A <- file.path(dirname(dir_prep), "bhi-data")
dir_B <- file.path(dirname(dir_prep), "bhi-data", "BHI 2.0")
dir_rawdata <- file.path(dir_B, "Pressure", "wgi_social")
```

The rescaled value of the WGI score is a resilience data layer (eg.
higher WGI, greater resilience).

## 1\. Background

The [Worldwide Governance Indicators (WGI)
project](http://info.worldbank.org/governance/wgi/index.aspx#home)
reports aggregate and individual governance indicators for 215 economies
over the period 1996–2014, for six dimensions of governance:

  - Voice and Accountability  
  - Political Stability and Absence of Violence/Terrorism  
  - Government Effectiveness  
  - Regulatory Quality  
  - Rule of Law  
  - Control of Corruption

## 2\. Data

This prep document is used to generate and explore the following data
layer:

  - `wgi_all_bhi2019.csv`

This is saved to the `layers` folder. These are derived from or informed
by the raw dataset obtained from OHI github webpage.

<br>

### 2.1 Datasets with Sources

<br/>

#### 2.1.1 Data

OHI has already extracted these data by country.  
For BHI, we obtained these data on 21 August 2020 from [the file
‘wgi\_combined\_scores\_by\_country.csv’ on the OHI github
site](https://github.com/OHI-Science/ohiprep/tree/master/globalprep/prs_res_wgi/v2017/intermediate).

The value for each country in 2015.

#### 2.1.1 Data Attributes

Colname names in ‘wgi\_combined\_scores\_by\_country.csv’ “country”:
reporting country “year”: reporting year “score\_wgi\_scale”: mean score
of six indicators: Voice and Accountability Political Stability and
Absence of Violence/Terrorism Government Effectiveness Regulatory
Quality Rule of Law Control of Corruption “score\_ohi\_scale”:
score\_wgi\_scale rescaled between 0 and 1 (score\_wgi\_scale range was
- 2.5 and 2.5)

<br/>

## 3\. Prep: Wrangling & Derivations, Checks/Evaluation, Gapfilling

### 3.1 Read in and organize data

``` r
## read in data
wgi_global <- read_csv("https://raw.githubusercontent.com/OHI-Science/ohiprep/master/globalprep/prs_res_wgi/v2017/intermediate/wgi_combined_scores_by_country.csv")

bhi_lookup <- read.csv(file.path(dir_prep, "supplement", "lookup_tabs", "bhi_basin_country_lookup.csv"),
                       sep=";", stringsAsFactors = FALSE) %>%
  dplyr::select(BHI_ID, rgn_nam) %>%
  dplyr::rename(
    rgn_id = BHI_ID,
    country = rgn_nam)
```

### 3.2 Select Baltic Sea countries

``` r
wgi_baltic <- wgi_global %>%
  filter(grepl("Denmark|Estonia|Finland|Germany|Latvia|Lithuania|Poland|Sweden|Russia", country))
```

### 3.3 Plot WGI scores

#### 3.3.1 Plot WGI scores (WGI scale)

``` r
ggplot(wgi_baltic) + 
  geom_point(aes(year, score_wgi_scale, colour = country ))+
   geom_line(aes(year, score_wgi_scale, colour = country ))+
  ylab("WGI Score")+
  ylim(-2.5,2.5)+
  ggtitle("WGI Scores (WGI scale")
```

![](wgi_prep_files/figure-gfm/Plot%20raw%20Baltic%20WGI%20scores-1.png)<!-- -->

#### 3.3.2 Plot WGI scores - OHI global rescale

``` r
ggplot(wgi_baltic) + 
  geom_point(aes(year, score_ohi_scale, colour = country ))+
   geom_line(aes(year, score_ohi_scale, colour = country ))+
  ylab("Score")+
  ylim(0,1)+
  ggtitle("WGI Scores (OHI global rescale")
```

![](wgi_prep_files/figure-gfm/Plot%20Baltic%20WGI%20ohi%20global%20rescale%20scores-1.png)<!-- -->

### 3.4 Rescale to the Baltic region

#### 3.4.1 Select Min and Max

The WGI scores have a range -2.5 to 2.5. They need to be rescaled to
between 0 and 1. Use minimum and maximum value in the dataset. We
compare the pressure layer difference if all countries are included in
the dataset when selecting min and max to rescale or if only Baltic
region values are used obtain min and max values to rescale.

``` r
## min
baltic_min <- wgi_baltic %>%
  dplyr::select(score_wgi_scale)%>%
  min()%>%
  as.numeric()
baltic_min

## country and year of min
filter(wgi_baltic, score_wgi_scale == baltic_min)
#         country year score_wgi_scale score_ohi_scale
# Russian Federation 2000      -0.8629691      0.3274062    


##max
baltic_max <- wgi_baltic %>%
  dplyr::select(score_wgi_scale)%>%
  max()%>%
  as.numeric()

## country and year of max
filter(wgi_baltic, score_wgi_scale == baltic_max)

# country year score_wgi_scale score_ohi_scale
#Finland 2004        1.985402      0.8970803
```

#### 3.4.2 Rescale Baltic

``` r
wgi_baltic = wgi_baltic %>%
  mutate(min = baltic_min,
         max=baltic_max,
         score_bhi_scale = (score_wgi_scale - baltic_min)/(baltic_max - baltic_min))%>%
  dplyr::select(-min,-max)
```

#### 3.4.3 Plot rescaled to Baltic Region

``` r
ggplot(wgi_baltic) + 
  geom_point(aes(year, score_bhi_scale, colour = country ))+
   geom_line(aes(year, score_bhi_scale, colour = country ))+
  ylab("Score")+
  ylim(0,1)+
  ggtitle("WGI Scores (BHI region rescale")
```

![](wgi_prep_files/figure-gfm/plot%20rescaled%20to%20baltic-1.png)<!-- -->

#### 3.4.4 Plot and Compare Rescaling options

Using the BHI region rescaling penalizes Russia. Does also spread the
scores
somewhat.

``` r
ggplot(tidyr::gather(wgi_baltic, score_type, score, -country,-year) %>% filter(score_type != "score_wgi_scale")) + 
  geom_point(aes(year, score, colour = country ))+
   geom_line(aes(year, score, colour = country ))+
  facet_wrap(~score_type)+
  ylab("Score")+
  ylim(0,1)+
  ggtitle("Rescale comparison WGI Scores")
```

![](wgi_prep_files/figure-gfm/Plot%20and%20Compare%20Rescaling%20options-1.png)<!-- -->

### 3.5 Assign country scores to BHI regions

#### 3.5.1 Join BHI region lookup to country scores

``` r
wgi_rgn <- wgi_baltic %>%
  dplyr::select(-score_wgi_scale)%>%
  tidyr::gather(., score_type, resilience_score, -country,-year)%>%
  mutate(country = ifelse(country == "Russian Federation", "Russia", country))%>%
  full_join(., bhi_lookup, by = "country")
```

## 4 Prepare and Save Objects

Select global rescaling so that Russia is not so heavily penalized.

### 4.1 Prepare object

``` r
wgi_all <- wgi_rgn %>%
  filter(score_type == "score_ohi_scale")%>%
  dplyr::select(-score_type, -country)%>%
  group_by(rgn_id) %>%
  filter(year == max(year))%>%
  ungroup() %>%
  dplyr::select(rgn_id, year, resilience_score) %>%
  arrange(rgn_id)

wgi_all <- wgi_all %>%
  dplyr::select(-year)
```

### 4.2 Save object

``` r
write.csv(wgi_all, file.path(dir_layers, 'wgi_all_bhi2019.csv'), row.names=FALSE)
```
