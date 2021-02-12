Worldwide Governance Indicators (WGI) Pressure Layer Data Preparation
================

<br>

``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = TRUE, results = "hide", fig.width = 9.5, fig.height = 6)
source(here::here("R", "setup.R"))
loc <- here::here("prep", "pressures", "wgi_social")
data_path <- here::here("data", "pressures", "wgi_social", version_year, "wgi_social_data.rmd")
```

<br>

## 1\. Background

The [Worldwide Governance Indicators (WGI)
project](http://info.worldbank.org/governance/wgi/index.aspx#home)
reports aggregate and individual governance indicators for 215 economies
over the period 1996–2018, for six dimensions of governance:

  - Voice and Accountability  
  - Political Stability and Absence of Violence
  - Government Effectiveness  
  - Regulatory Quality  
  - Rule of Law  
  - Control of Corruption

The rescaled value of the WGI score is a resilience data layer (eg.
higher WGI, greater resilience). The inverse is the WHI pressure score.

## 2\. Data

This prep document is used to generate and explore the following data
layer:

  - `ss_wgi_bhi2019.csv`

This is saved to the `layers` folder. These are derived from or informed
by the raw dataset obtained from OHI github webpage.

<br>

### 2.1 Datasets with Sources

<br/>

OHI has already extracted these data by country.  
For BHI, we obtained these data on 21 August 2020 from [the file
‘wgi\_combined\_scores\_by\_country.csv’ on the OHI github
site](https://github.com/OHI-Science/ohiprep/tree/master/globalprep/prs_res_wgi/v2017/intermediate).

The value for each country in 2015

#### 2.1.1 Data attributes

Colname names in ‘wgi\_combined\_scores\_by\_country.csv’ “country”:
reporting country “year”: reporting year “score\_wgi\_scale”: mean score
of six indicators: Voice and Accountability Political Stability and
Absence of Violence Government Effectiveness Regulatory Quality Rule of
Law Control of Corruption “score\_ohi\_scale”: score\_wgi\_scale
rescaled between 0 and 1 (score\_wgi\_scale range was - 2.5 and 2.5)

<br/>

## 3\. Prep: Wrangling & Derivations, Checks/Evaluation, Gapfilling

``` r
## Libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RMySQL)
library(stringr)
library(tools)
library(rprojroot) # install.packages('rprojroot')
```

### 3.1 Read in and organize data

``` r
## read in data
wgi_global <- read_csv("https://raw.githubusercontent.com/OHI-Science/ohiprep/master/globalprep/prs_res_wgi/v2017/intermediate/wgi_combined_scores_by_country.csv")

bhi_lookup <- read.csv(file.path(dir_prep, "supplement", "lookup_tabs", "bhi_basin_country_lookup.csv"),
                       sep=";", stringsAsFactors = FALSE) %>%
  select(BHI_ID, rgn_nam) %>%
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
  geom_point(aes(year, score_wgi_scale, colour = country)) +
  geom_line(aes(year, score_wgi_scale, colour = country)) +
  ylab("WGI Score") +
  ylim(-2.5,2.5) +
  ggtitle("WGI Scores (WGI scale)")
```

![](wgi_social_prep_files/figure-gfm/Plot%20raw%20Baltic%20WGI%20scores-1.png)<!-- -->

#### 3.3.2 Plot WGI scores - OHI global rescale

``` r
ggplot(wgi_baltic) + 
  geom_point(aes(year, score_ohi_scale, colour = country)) +
  geom_line(aes(year, score_ohi_scale, colour = country)) +
  ylab("Score") +
  ylim(0,1) +
  ggtitle("WGI Scores (OHI global rescale)")
```

![](wgi_social_prep_files/figure-gfm/Plot%20Baltic%20WGI%20ohi%20global%20rescale%20scores-1.png)<!-- -->

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
  select(score_wgi_scale) %>%
  min() %>%
  as.numeric()

## country and year of min
filter(wgi_baltic, score_wgi_scale == baltic_min)
#     country         year   score_wgi_scale    score_ohi_scale
# Russian Federation 2000      -0.8629691       0.3274062


##max
baltic_max <- wgi_baltic %>%
  select(score_wgi_scale) %>%
  max() %>%
  as.numeric()

## country and year of max
filter(wgi_baltic, score_wgi_scale == baltic_max)

# country year     score_wgi_scale   score_ohi_scale
# Finland 2004        1.985402         0.8970803
```

#### 3.4.2 Rescale Baltic

``` r
wgi_baltic <- wgi_baltic %>%
  mutate(min = baltic_min,
         max=baltic_max,
         score_bhi_scale = (score_wgi_scale - baltic_min)/(baltic_max - baltic_min)) %>%
  select(-min,-max)
```

#### 3.4.3 Plot rescaled to Baltic Region

``` r
ggplot(wgi_baltic) + 
  geom_point(aes(year, score_bhi_scale, colour = country)) +
   geom_line(aes(year, score_bhi_scale, colour = country)) +
  ylab("Score") +
  ylim(0,1) +
  ggtitle("WGI Scores (BHI region rescale")
```

![](wgi_social_prep_files/figure-gfm/plot%20rescaled%20to%20baltic-1.png)<!-- -->

#### 3.4.4 Plot and Compare Rescaling options

Using the BHI region rescaling penalizes Russia. Does also spread the
scores
somewhat.

``` r
ggplot(gather(wgi_baltic, score_type, score, -country,-year) %>% filter(score_type != "score_wgi_scale")) + 
  geom_point(aes(year, score, colour = country ))+
   geom_line(aes(year, score, colour = country ))+
  facet_wrap(~score_type)+
  ylab("Score")+
  ylim(0,1)+
  ggtitle("Rescale comparison WGI Scores")
```

![](wgi_social_prep_files/figure-gfm/Plot%20and%20Compare%20Rescaling%20options-1.png)<!-- -->

### 3.5 Convert to Pressure score

#### 3.5.1 Convert to pressure score

Inverse of the score

``` r
wgi_baltic_pressure <- wgi_baltic %>%
  select(-score_wgi_scale) %>%
  gather(., score_type, score, -country, -year) %>%
  mutate(pressure_score = 1 - score) %>%
  select(-score)
```

#### 3.5.2 Plot pressure score

``` r
ggplot(wgi_baltic_pressure) + 
  geom_point(aes(year, pressure_score, colour = country)) +
   geom_line(aes(year, pressure_score, colour = country)) + 
  facet_wrap(~score_type) +
  ylab("Pressure Score") +
  ylim(0,1) +
  ggtitle("Rescale comparison WGI Pressure Scores")
```

![](wgi_social_prep_files/figure-gfm/Plot%20and%20Compare%20Rescaling%20options%20with%20Pressure%20score-1.png)<!-- -->

### 3.6 Assign country scores to BHI regions

#### 3.6.1 Join BHI region lookup to country scores

``` r
wgi_pressure_rgn <- wgi_baltic_pressure %>%
  mutate(country = ifelse(country == "Russian Federation", "Russia", country)) %>%
  full_join(., bhi_lookup, by = "country")
```

## 4\. Prepare and save Objects

Select global rescaling so that Russia is not so heavily penalized.

### 4.1 Prepare object

``` r
ss_wgi <- wgi_pressure_rgn %>%
  filter(score_type == "score_ohi_scale") %>%
  select(-score_type, -country) %>%
  group_by(rgn_id) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(rgn_id, year, pressure_score) %>%
  arrange(rgn_id)

ss_wgi <- ss_wgi %>%
        select(-year)

# Save object
#write.csv(ss_wgi, file.path(dir_layers, 'ss_wgi_bhi2019.csv'), row.names=FALSE)
```
