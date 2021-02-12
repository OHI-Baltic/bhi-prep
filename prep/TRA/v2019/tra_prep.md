Clean Water (CW) - Trash (TRA) Subgoal Data Preparation
================

<br>

``` r
source(here::here("R", "setup.R"))
loc <- here::here("prep", "CW", "trash")
dir_intermediate <- file.path(here::here("data", "CW", "trash", version_year, "intermediate"))

bkgd_path <- here::here("supplement", "goal_summaries", "TRA.Rmd")
data_path <- here::here("data", "CW", "trash", version_year, "tra_data.rmd")
refs_path <- file.path(loc, "tra_references.Rmd")
```

## 1\. Background

### 1.1 Goal Description

The Trash sub-goal assesses a region’s ability to manage litter in order
to prevent it from entering the ocean and causing harm to the coastal
and marine environments. Marine litter is a large global concern,
impacting all marine environments of the world and, therefore, needs to
be monitored.

### 1.2 Model & Data

Currently (2020), the [HELCOM Expert Network on Marine
Litter](https://helcom.fi/helcom-at-work/groups/pressure/en-marine-litter/)
is working on facilitating the implementation of the Regional Action
Plan on Marine Litter, and developing [core
indicators](http://stateofthebalticsea.helcom.fi/pressures-and-their-status/marine-litter/)
to be used for the second HELCOM holistic assessment of the ecosystem
health.

Marine litter can be found on beaches and shorelines, floating on the
surface, submerged in the water column or sunk to the bottom. Therefore,
different main indicators might be used: - beach litter monitoring -
floating litter monitoring - seafloor litter monitoring

However, the majority of litter monitoring methods are not standardized
specifically for the Baltic Sea. There is a need to harmonize data
collection, give more information for reference, improve comparability
of results and allow benchmarking. These proposed new actions will be
discussed during 2020 by the relevant HELCOM Working Groups and probably
inserted in the updated BSAP.

The status is calculated by a country’s amount of mismanaged plastic
litter that has the potential to enter the ocean:

[Modelled data by *Jambeck et al. 2015*: Plastic waste inputs from land
into the
ocean](http://science.sciencemag.org/content/347/6223/768.full.pdf+html).

The modelled data have been downweighted for Russia, Germany, Denmark
and Sweden (proportion of coastal pop/national pop) to include only the
litter that reaches the the Baltic Sea from these countries.

### 1.3 Reference points

The good environmental status from the Marine Strategy Framework
Directive is qualitatively framed as the situation where ‘properties and
quantities of marine litter do not cause harm to the coastal and marine
environment’. Currently, there is no official quantitative reference
point set. Therefore, for this assessment the upper reference point was
set as the maximum amount of litter among all Baltic surrounding
countries in 2010, and the lower reference point is zero litter in the
Baltic Sea.

**Beach litter threshold proposal (for next BHI iteration)** The JRC
presented a [beach litter threshold
proposal](https://circabc.europa.eu/ui/group/326ae5ac-0419-4167-83ca-e3c210534a69/library/254a8e7b-443f-4889-912a-941889b29cf3/details)
for an EU-level beach litter threshold value for MSFD criterion D10C1.
The threshold value proposed of a total abundance of 13 items/100m is
based on the value achieved by the top 10 percent EU beaches in the
baseline data set in 2015-2016.

### 1.4 Considerations for BHI 3.0

**Standardization:** The majority of litter monitoring methods are not
standardized specifically for the Baltic Sea. Data collection needs to
be harmonized, to improve comparability of results and thus allow
benchmarking. **Microplastics:** Including data on microplastics would
result in a more complete picture, but these data are not currently
available. **Work-in-progress:** In HELCOM, assessment approaches based
on core indicators are currently underway for beach litter, litter on
the seafloor and microlitter. Threshold values for the assessment are
being developed in an EU-process which can be used in the next BHI
iteration.

### 1.5 Other information

No expert for this particular sub-goal.

<br/>

## 2\. Data

## 2\. Data

### 2.1 Datasets with Sources

#### 2.1.1 Marine Litter

**Modelled Marine Litter and Trash Data**

Data for litter/trash from [Plastic waste inputs from land into the
ocean(Jambeck et
al 2015)](http://science.sciencemag.org/content/347/6223/768.full.pdf+html)

Raw data were stored in `bhi-1.0-archive/baltic2015/prep/CW/trash/raw`
and now also stored in

The Jambeck et al method includes:

1.  the mass of waste generated per capita annually;
2.  the percentage of waste that is plastic; and
3.  the percentage of plastic waste that is mismanaged and, therefore,
    has the potential to enter the ocean as marine debris (12) (data
    S1). By applying a range of conversion rates from mismanaged waste
    to marine debris, we estimated the mass of plastic waste entering
    the ocean from each country in 2010, used population growth data
    (13) to project the increase in mass to 2025, and predicted growth
    in the percentage of waste that is plastic.

Footnotes from .xls file column headers 1 - Based upon 2010 Gross
National Income 2 - Based upon a 50 km coastal buffer created in GIS
with global population densities 3 - Bold data were taken directly from
World Bank estimates 4 - Bold data were taken directly from World Bank
estimates 5 - Using a model developed for this study (see Supplemental
Information) 6 - From U.S. national litter study 7 - Calculated values 8
- Economic status changed from 2005 to 2010; waste generation rate and
%plastic were assigned using average values for the 2010 economic
category

**Litter Exchange Data = (HH and LT) – continuously updated**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/TRA/DATRAS -->

For the Baltic Sea, data are delivered by Denmark, Estonia, Germany,
Latvia, Lithuania, Poland, Russia and Sweden and are related mainly to
BITS. In the Baltic Sea, the gears TVS and TVL are mostly used. Even if
the coverage by year and country is quite good, some data gaps are still
remaining (for details and vocab see [DATRAS
vocabulary](https://vocab.ices.dk/?theme=1)).

| Option         | Specification                            |
| :------------- | :--------------------------------------- |
| Data products: | Litter exchange data                     |
| Survey:        | BITS (Baltic International Trawl Survey) |
| Quarter(s):    | All                                      |
| Year(s):       | All                                      |

Source: [ICES database
(DATRAS)](https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx)
<br/> Downloaded 8 May 2020 by Andrea De Cervo

-----

<br/>

**Litter Assessment Output (OSPAR) = (merged HH, LT, and GIS-info) –
updated once a year**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/TRA/DATRAS -->

For the Baltic Sea, data are delivered by Denmark, Estonia, Germany,
Latvia, Lithuania, Poland, Russia and Sweden and are related mainly to
BITS. In the Baltic Sea, the gears TVS and TVL are mostly used. Even if
the coverage by year and country is quite good, some data gaps are still
remaining (for details and vocab see [DATRAS
vocabulary](https://vocab.ices.dk/?theme=1)).

| Option         | Specification                            |
| :------------- | :--------------------------------------- |
| Data products: | Litter Assessment Output                 |
| Survey:        | BITS (Baltic International Trawl Survey) |
| Quarter(s):    | All                                      |
| Year(s):       | All                                      |

Source: [ICES database
(DATRAS)](https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx)
<br/> Downloaded 8 May 2020 by Andrea De Cervo

**Plastic Marine Pollution Global
Dataset**  
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/TRA/Eriksen_PlasticMarine -->

Dataset posted on 03.05.2014, 13:14 by Marcus Eriksen. *This is a global
dataset of 1571 locations where surface manta tows were conducted.
Samples were divided into 4 size categories. Weights and particle counts
were recoreded for each category.* Dataset supplement to paper [Plastic
Pollution in the World’s Oceans: More than 5 Trillion Plastic Pieces
Weighing over 250,000 Tons Afloat at
Sea](https://doi.org/10.1371/journal.pone.0111913) by Marcus Eriksen et
al.

| Specification                                     |
| :------------------------------------------------ |
| Sheet1                                            |
| All columns                                       |
| Remove column P and O (Comments) and save as .csv |

Source: [Plastic Marine Pollution Global Dataset, shared via
figshare](http://dx.doi.org/10.6084/m9.figshare.1015289) <br/>
Downloaded 10 March, 2020 by Ellie Campbell

<br/>

**Overview of monitoring methods in the Baltic
Sea**

| Litter monitoring | Method                                   | Lead Country | Years |
| :---------------- | :--------------------------------------- | :----------- | :---- |
| Beach litter      | Marlin/ OSPAR                            | Poland       | years |
| Floating litter   | Net curtains / from ships                | Estonia      | years |
| Seafloor litter   | BITS (Baltic International Trawl Survey) | Denmark (?)  | years |
| Microplastics     |                                          | Finland      | years |

Source:
[Blastic](https://www.sei.org/wp-content/uploads/2017/12/blastic-overview-of-available-monitoring-methods-final.pdf)
<br/> [HELCOM Meeting
notes](https://portal.helcom.fi/workspaces/MARINE%20LITTER-92/Beach%20litter%20pre%20core%20indicator/Memo%20on-line%20meeting%20HELCOM%20EN-ML_27_1_2020.pdf)Downloaded
18 May 2020 by Andrea De Cervo

<br>

### 2.2 Centralization & Normalization

**Marine litter data**

``` r
## root location of the raw data
dir_rawdata <- file.path(dir_B, "Goals", "CW", "TRA")
marinepastic_raw <- readr::read_csv(file.path(dir_rawdata, "Eriksen_PlasticMarine", "marineplastic_eriksen.csv"))
litterdata_raw <- readr::read_csv(file.path(dir_rawdata, "DATRAS", "Litter Exchange Data", "litter_exchange_data.csv"))
litterassess_raw <- readr::read_csv(file.path(dir_rawdata, "DATRAS", "Litter Assessment Output", "litter_assessment_output.csv"))
```

<br/>

## 3\. Prep: Wrangling & Derivations, Checks/Evaluation

### 3.1 Initial Data Wrangling/Harmonizing

**Load datasets**

``` r
# raw jambeck data
trash_file <- read_csv(file.path(dir_intermediate, 'trash_jambeck_baltic.csv'))

## BHI regional pop
bhi_pop <- read_csv(file.path(dir_intermediate, 'NUTS2_BHI_ID_Pop_density_in_buffer.csv')) %>% 
  dplyr::select(country = rgn_nam, 
                pop_rgn = PopTot) %>% 
  filter(!country == "Russia") %>% 
  group_by(country) %>% 
  summarize(bhi_pop = round(sum(pop_rgn), 0))

## lookup tables
lookup_bhi <- read.csv(file.path(here::here(), "supplement", "lookup_tabs", 'bhi_basin_country_lookup.csv'),
                                 sep=";", stringsAsFactors = FALSE) %>% 
  dplyr::select(rgn_id =BHI_ID, 
                country = rgn_nam) %>%
  mutate(country = as.character(country))

baltic_lookup <- read_csv(file.path(here::here(), "supplement", "lookup_tabs",'country_id.csv'))

## remove Total and footnotes (NAs in Country column)
data_clean <- trash_file %>%
  filter(!is.na(Country)) %>% #head(data_raw); summary(data_raw)
  dplyr::select(
    country = Country,
    coastal_pop = `Coastal population2`,
    modeled_waste_2010_orig = `Mismanaged plastic waste in 2010\r\n [tonnes]7`,
    modeled_waste_2025_orig = `Mismanaged plastic waste in 2025\r\n [tonnes]7`) %>%
  mutate(country = str_replace_all(country, '8', '')) # remove footnotes from country names

## filter Baltic data, excluding Russia, save intermediate
baltic_all <- data_clean %>%
  filter(country %in% baltic_lookup$country_name) %>% 
  filter(!country == "Russia")
#  write.csv(baltic, '~/github/bhi/baltic2015/prep/CW/trash/intermediate/trash_jambeck_baltic.csv')
```

<br/>

### 3.2 Downweight for Germany, Denmark, Sweden

Amount of trash proportional of coastal pop/national pop

``` r
##combine coastal and bhi pop & downweight for Germany, Denmark, Sweden 
baltic_data <- full_join(bhi_pop, baltic_all, by = "country") %>% 
  mutate(proportion = bhi_pop/coastal_pop,
         ## only downweight for Germany, Denmark, and Sweden
         proportion = ifelse(country == "Germany" | country == "Denmark" | country == "Sweden", proportion, 1), 
         modeled_waste_2010 = modeled_waste_2010_orig * proportion, 
         modeled_waste_2025 = modeled_waste_2025_orig * proportion) %>% 
  dplyr::select(country, modeled_waste_2010, modeled_waste_2025)

# baltic_pop_proportion = full_join(bhi_pop, baltic_all, by = "country") %>%
#   mutate(proportion = bhi_pop/coastal_pop) %>%
#   dplyr::select(country, bhi_pop, coastal_pop, proportion)
```

Note: Because Russia is reported as a whole country, we used the
proportion of population in Kaliningrad+St Petersburg to the coastal
population reported by Jambeck et al. See figure below.

#### 3.2.1 Downweight Russia Coastal Trash

It made sense to downweight Kaliningrad and St Petersburg by coastal
population density instead of total Russia population density because of
the distribution along the
coasts

<!-- Then find ref point as the max of Europe, but compare 2010 v. 2025 to determine whether it's worth trying to interpolate/model to find a reference point in ~2015. Conclusion, probably not worth it, so we could just pick either 2010 or 2025 since those data are already modeled/reported by Jambeck et al.  -->

``` r
## havea another look at the data
# baltic = read.csv('~/github/bhi/baltic2015/prep/CW/trash/intermediate/trash_jambeck_baltic.csv')
# summary(baltic) 
# summary(data_clean)

# ###### NOT NEEDED? ###############
# ## read in lookup table for European countries
# lookup = read.csv('~/github/bhi/baltic2015/prep/EUcountrynames.csv', sep=';') %>%
#   mutate(country_abb = as.character(country_abb),
#          country     = as.character(country)); # head(lookup)
# 
# ## join lookup table to trash data; remove inland NAs and Kosovo, Macedonia, Serbia (no trash data)
# europe = lookup %>%
#   left_join(data_clean, by = 'country') %>%
#   filter(!is.na(coastal_pop)) %>%
#   dplyr::select(-country_abb); # head(europe)

## downweight russia **very hacky for now**
#  4597600 # St Petersbergy: from Wikipedia https://en.wikipedia.org/wiki/Demographics_of_Saint_Petersburg
#  431902  # Kaliningrad https://en.wikipedia.org/wiki/Kaliningrad
rus_trash_pop <- data_clean %>%
  filter(country == 'Russia') %>%
  dplyr::select(coastal_pop)

rus_coastal_pop_proportion <- as.numeric((4597600+431902) / rus_trash_pop); rus_coastal_pop_proportion

rus <- data_clean %>%
  filter(country == 'Russia') %>%
  mutate(modeled_waste_2010 = modeled_waste_2010_orig * rus_coastal_pop_proportion, 
         modeled_waste_2025 = modeled_waste_2025_orig * rus_coastal_pop_proportion) %>% 
  dplyr::select(country, modeled_waste_2010, modeled_waste_2025)

## create dataframe of europe + russia
eur_rus <- rbind(
  baltic_data,
  rus) 
```

### 3.3 Calculate Mismanaged plastic waste 2015

Assuming it’s a linear trend between 2010 and 2025, 2015 values are the
1/3-point between 2010 and 2025.

#### 3.3.1 Plot Modeled Mismanaged plastic waste 2010, 2015, and 2025

``` r
modeled_waste_plot <- modeled_waste %>% 
  tidyr::gather(scenario, waste, 2:4) 

ggplot(modeled_waste_plot, aes(x = country, y = waste, fill = scenario)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) + 
  labs(title = 'Modeled waste 2010, 2015, & 2025',
      x = 'Country', 
      y = 'Waste (tonns)')
```

![](tra_prep_files/figure-gfm/plot%20modeled%20mismanaged%20plastic%20waste-1.png)<!-- -->

``` r
## SAVE 2015 DATA for VISUALIZE
tra_value_data <- modeled_waste%>%
  dplyr::select(value=modeled_waste_2015,
                location =country)%>%
  mutate(unit= "tonnes",
         bhi_goal = "TRA",
         data_descrip="modeled mismanaged plastic waste 2015" )

# write.csv(tra_value_data, file.path(dir_baltic,'visualize/tra_value_data.csv'), row.names=TRUE)
```

### 3.4 Prepare data layer for Toolbox

**Plan:** We will create scores for each country for 2015 related to the
reference point, and then apply to all of that country’s BHI regions
under the assumption that the management for the country is probably
equal.

Modeled Mismanaged plastic waste data (tonnes) are reported for each
country. We will make a linear model between 2010 and 2025 and choose
2015.

Note: for BHI 2.0, would be better to recalculate their model using
coastal population for each BHI region.

**Reference points** Need a min and a max reference point (they will
ultimately be inverted for CW but not as a pressure)

  - **Minimum modeled trash entering the ocean = 0** (we want no trash
    entering the ocean)
  - **Maximum modeled trash entering the ocean = highest BHI country in
    2010**

#### 3.4.1 find ref point

``` r
## set min and max reference points ----
## rescale to reference point -- whole European
ref_point_min = 0

## 2010 ref point
ref_point_max_2010 <- eur_rus %>%
 filter(modeled_waste_2010 == max(modeled_waste_2010))

sprintf('max trash for 2010 is %s (%s)', 
        round(ref_point_max_2010$modeled_waste_2010),
        ref_point_max_2010$country)

ref_point_max_2010 <- ref_point_max_2010$modeled_waste_2010
# "max trash for 2010 is 37566 (Russia)"
```

<!-- ### 3.5 Plot exploring ref point as max of Europe (including Russia) in 2010 v. 2025 -->

<!-- ```{r plot and save layer} -->

<!-- # compare reference points - makes minimal differences in scores, country order does not change.  Larger differences for Germany & Russia (lesser degree Poland) -->

<!-- baltic_explore2 = baltic_explore %>%  -->

<!--   dplyr::select(country, trash_score_2010, trash_score_2025) %>%  -->

<!--   gather(key = ref_point, value = score, 2:3) %>%  -->

<!--   mutate(status_score = (1-score)*100) -->

<!-- ggplot(baltic_explore2, aes(x = country, y = status_score, fill = ref_point)) + -->

<!--   geom_point(aes(color = ref_point)) +  -->

<!--   theme(axis.text.x = element_text(angle = 75, hjust = 1)) + -->

<!--   labs(title = "Trash scores comparison: \n max(2010) vs. max(2025) as reference point") -->

<!-- ## assign to BHI regions ---- -->

<!-- lookup_bhi = read.csv('~/github/bhi/baltic2015/prep/CW/trash/bhi_basin_country_lookup.csv',sep=";", stringsAsFactors = FALSE) %>% -->

<!--   dplyr::select(rgn_id =BHI_ID, -->

<!--          country = rgn_nam) %>% -->

<!--   mutate(country = as.character(country)) -->

<!-- ## join baltic_explore (baltic countries) to bhi regions, repeating the trash score for each baltic region. -->

<!-- baltic_layer = lookup_bhi %>% -->

<!--   left_join(baltic_explore, -->

<!--             by = 'country') %>% -->

<!--   dplyr::select(rgn_id, -->

<!--          score = trash_score_2010); # baltic_layer -->

<!-- ## save layer to layers folder, and register layer in layers.csv by hand -->

<!-- # write.csv(baltic_layer, '~/github/bhi/baltic2015/layers/po_trash_bhi2015.csv',row.names=FALSE) -->

<!-- ## calculate trend; maybe make a linear model between 2010 and 2025, take trend as 5 years out? -->

<!-- ``` -->

### 3.6 Explore status score 2010 and 2015

**Status**: 2015 modeled values / ref point 2010  
**Trend**: 5 year liear trend 2010-2015

2015: Using modeled 2015 data for status but use max 2010 as a reference
point. This approach was used as the final candidate for status
calculation.

``` r
## scores by country
scores_by_country <- modeled_waste %>% 
  mutate(ref_point = 37566,  # "max trash for 2010 is 37566 (Russia)"
         score_2010 = (1 - pmin(1, modeled_waste_2010/ref_point))*100, 
         score_2015 = (1 - pmin(1, modeled_waste_2015/ref_point))*100 ) %>% 
  dplyr::select(country, score_2010, score_2015)

scores_by_country_plot <- scores_by_country %>% 
  tidyr::gather(year, score, 2:3)

# plot by country
ggplot(scores_by_country_plot, aes(x = country, y = score, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  ggtitle("Trash scores by country: 2010 & 2015")
```

![](tra_prep_files/figure-gfm/explore%20tra%20status%202015-1.png)<!-- -->

``` r
## scores by bhi region
## join baltic_explore (baltic countries) to bhi regions, repeating the trash score for each baltic region.
scores_by_bhi <- lookup_bhi %>%
  left_join(scores_by_country, 
            by = 'country') %>%
  dplyr::select(-country) 

scores_by_bhi_plot <- scores_by_bhi %>% 
  tidyr::gather(year, score, 2:3)
  
ggplot(scores_by_bhi_plot, aes(x = rgn_id, y = score, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  ggtitle("Trash scores by BHI region: 2010 & 2015")
```

![](tra_prep_files/figure-gfm/explore%20tra%20status%202015-2.png)<!-- -->

``` r
# save as CW-TRA data layer
tra_status <- scores_by_bhi %>% 
  dplyr::select(rgn_id, score = score_2015)

# write_csv(tra_status, file.path(dir_layers, 'cw_tra_status_bhi2015.csv'))
```

### 3.7 Trend calculation

Based on linear model of status scores in 2010 and 2015.

``` r
tra_trend <- scores_by_bhi %>% 
  tidyr::gather(year, score, 2:3) %>% 
  mutate(year = substr(year, 7, 10)) %>% 
  group_by(rgn_id) %>% 
  summarize(trend_score = round((score[year == 2015] - score[year == 2010])/5 * 0.05, 2)) %>% 
  # NOT used: could not select coefficient of year from linear model 
  # do(lmd = lm(score ~ year, data = .)) %>% 
  # mutate(trend_score = coef(lmd)[['year']])
  dplyr::select(rgn_id, score = trend_score)

# write_csv(tra_trend, file.path(dir_layers, 'cw_tra_trend_bhi2015.csv'))
```

## 4\. Save as Pollution Pressures layer

``` r
press_scores <- tra_status %>% 
  mutate(pressure_score = 1 - score/100) %>% 
  dplyr::select(rgn_id, pressure_score)

# write_csv(press_scores, file.path(dir_layers, "po_trash_bhi2015.csv"))
```

## 5\. References

Data for litter/trash from [**Jambeck et al 2015: Plastic waste inputs
from land into the
ocean**](http://science.sciencemag.org/content/347/6223/768.full.pdf+html)

[Interim Report of the Working Group on Marine Litter
(WGML)](http://ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/HAPISG/2018/01%20WGML%20-%20Report%20of%20the%20Working%20Group%20on%20Marine%20Litter.pdf)

[Overview of available methods to monitor marine plastic litter in the
Baltic
Sea](https://www.sei.org/wp-content/uploads/2017/12/blastic-overview-of-available-monitoring-methods-final.pdf)

[Manual for the Baltic International Trawl Surveys
(BITS)](http://www.ices.dk/sites/pub/Publication%20Reports/ICES%20Survey%20Protocols%20%28SISP%29/2017/SISP7%20BITS%202017.pdf)
