Clean Water (CW) - Contaminants (CON) Subgoal Data Preparation
================

-   [1. Background](#background)
    -   [1.1 Goal Description](#goal-description)
    -   [1.2 Model & Data](#model-data)
    -   [1.3 Reference points](#reference-points)
    -   [1.4 Other information](#other-information)
-   [2. Data](#data)
    -   [2.1 Datasets with Sources](#datasets-with-sources)
    -   [2.2 Centralization & Normalization](#centralization-normalization)
    -   [2.3 Initial Data Exploration](#initial-data-exploration)
-   [3. Wrangling, Evaluation, and Gapfilling](#wrangling-evaluation-and-gapfilling)
    -   [3.1 PCB Indicator](#pcb-indicator)
    -   [3.2 PFOS Indicator](#pfos-indicator-1)
    -   [3.3 Dioxin Indicator](#dioxin-indicator)
    -   [3.4 Concerning Substances Indicator](#concerning-substances-indicator)
    -   [3.4.1 Regional monitoring of Concerning Substances](#regional-monitoring-of-concerning-substances)
    -   [3.4.2 Save concerning substances layer and intermediate datasets](#save-concerning-substances-layer-and-intermediate-datasets)
-   [4. Visualizing Contaminants Data Layers](#visualizing-contaminants-data-layers)
    -   [4.1 Contaminants data layers Maps & Subbasin Trends](#contaminants-data-layers-maps-subbasin-trends)
-   [5. Considerations for `BHI3.0`](#considerations-for-bhi3.0)
-   [6. References](#references)

<br>

<br>

1. Background
-------------

### 1.1 Goal Description

The Contaminant sub-goal of the Clean Water goal captures the degree to which local waters are unpolluted by contaminants. This sub-goal scores highest when the contamination level is below a threshold, which is defined by the Marine Framework Directive. For the BHI three contaminants indicators are proposed, describing different aspects of toxicity: dioxin and dioxin like compounds, polychlorinated biphenyl compounds (PCBs), and perfluorooctanesulfonic acid (PFOS). In addition, a penalty factor has been applied to account for the fact that current monitoring programs do not cover all the registered and harmful contaminants.

### 1.2 Model & Data

All contaminant data were downloaded from the open-accessible ICES database (see below).

-   [ICES Database for Contaminants in Biota (PCBs, Dioxins, PFOS, and Concerning Substances)](http://dome.ices.dk/views/ContaminantsBiota.aspx)
-   [ICES Database for Contaminants in Sediment (PCBs, Dioxins, and Concerning Substances)](http://dome.ices.dk/views/ContaminantsSediment.aspx)
-   [Reference codes for PCB Congeners](https://vocab.ices.dk/?CodeID=26983)
-   [Reference codes for Dioxin Congeners](https://vocab.ices.dk/?CodeID=26986)

Table to calculate the Concerning Substances indicator (penalty factor):

-   [European Chemical Agency Candidate List of substances of very high concern for Authorisation](https://echa.europa.eu/candidate-list-table?p_p_id=disslists_WAR_disslistsportlet&p_p_lifecycle=1&p_p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_pos=2&p_p_col_count=3&_disslists_WAR_disslistsportlet_javax.portlet.action=searchDissLists)

### 1.3 Reference points

#### 1.3.1 PCB Concentration Indicator Biota

Non-dioxin like PCBs: sum of congeners (28, 52, 101, 138, 153, 180). Target is set at the threshold of 75 ug/kg ww (wet weight) fish muscle.

This is similar to the ICES-7 except that PCB 118 is excluded, since it is metabolized by mammals.

75 ng/g wet weight is the [EU threshold for fish muscle. See Section 5 Annex, 5.3](http://eur-lex.europa.eu/LexUriServ/LexUriServ.do?uri=OJ:L:2011:320:0018:0023:EN:PDF). This threshold was also agreed upon as GES boundary at the meeting of the [Working Group on the State of the Environment and Nature Conservation](http://helcom.fi/helcom-at-work/groups/state-and-conservation) April 11-15, 2016. *Recevied the draft report from Elisabeth Nyberg.*

<br/>

#### 1.3.2 TEQ values for Dioxins and Dioxin-like PCBs in Biota

Dioxin and dioxin-like compounds. Target is set at 0.0065 TEQ ug/kg ww fish, crustaceans or molluscs (source of target: EQS biota human health). Secondary GES boundary: CB-118 24 ug/kg lw fish liver or muscle (source: EAC).

This threshold was agreed upon as GES indicator at the meeting of the [Working Group on the State of the Environment and Nature Conservation](http://helcom.fi/helcom-at-work/groups/state-and-conservation) April 11-15, 2016. *Recevied the draft report from Elisabeth Nyberg.*

This is consistent with the [EU human health thresholds for dioxin and dioxin-like compounds - 6.5 pg/g](http://eur-lex.europa.eu/LexUriServ/LexUriServ.do?uri=OJ:L:2011:320:0018:0023:EN:PDF)

TEQ values from the [World Health Organization 2005](http://www.who.int/ipcs/assessment/tef_values.pdf)

<br/>

#### 1.3.3 PFOS Indicator

According to [HELCOM PFOS core indicator document, p.3](http://www.helcom.fi/Core%20Indicators/PFOS_HELCOM%20core%20indicator%202016_web%20version.pdf), the "GES boundary is set to 9.1 μg/kg wet weight (or 9.1 ng/g ww) with the protection goal of human health".

"The GES boundary is an environmental quality standard (EQS), derived at EU level as a substance included on the list of priority substances under the Water Framework Directive (European Commission 2000, 2013). GES, in accordance with the MSFD is defined as 'concentrations of contaminants at levels not giving rise to pollution effects'. EQS are derived from ecotoxicological studies to protect freshwater and marine ecosystems from potential adverse effects of chemicals, as well as adverse effects on human health via drinking water and food from aquatic environments. Quality Standards (QS) are derived for different protection goals, i.e.: pelagic and benthic communities, top-predators in these ecosystems, and human health. The most stringent of these QS is the basis for the EQS. The EQS boundary for PFOS is based on the QS set for biota to protect human health (9.1 μg/ kg fish ww), defined for edible parts in fish. For harmonization purposes the EC Guidance Document No. 32 on biota monitoring (the implementation of EQS biota) under the WFD was developed (European Commission 2014). This guidance document recommends that the results from the monitoring should be standardized to represent fish at a trophic level of 4, which is an estimate of the general trophic level in commercial fish in Europe. The recommendation to obtain PFOS data in fish at a trophic level of 4 is to adjust the values from monitoring in accordance with trophic magnification factors and trophic level." − [HELCOM PFOS core indicator document, p.8](http://www.helcom.fi/Core%20Indicators/PFOS_HELCOM%20core%20indicator%202016_web%20version.pdf)

**HELCOM core indicator report uses liver PFOS concentrations converted to muscle equivalent values** as in [Faxneld et al. 2014b](https://www.diva-portal.org/smash/get/diva2:767385/FULLTEXT01.pdf).

<br/>

#### 1.3.4 TEQ values for PCBs in Sediment

Only a few environmental quality standards (EQS) are to date defined in the Baltic Sea for marine surface sediments. The Norwegian threshold values are often used when a comparison with ecotoxicological effects is needed. In particular, the [Norwegian Environment Agency](https://www.miljodirektoratet.no/globalassets/publikasjoner/m1132/m1132.pdf) has defined EQS in sediment for 28 EU priority substances.

PCB7 (sum PCB 28, 52, 101, 118, 138, 153, and 180): Target is set at 4.1 TS ug/kg dw. [Environmental quality classification of water bodies](http://www.vannportalen.no/globalassets/nasjonalt/dokumenter/veiledere-direktoratsgruppa/Klassifisering-av-miljotilstand-i-vann-02-2018.pdf)

**Recommended threshold values are given only for total PCB7, and not for each individual congener. This is because toxicity data are available for only a minority of congeners.**

<br/>

#### 1.3.5 TEQ values for Dioxins in Sediment

Dioxin and dioxin-like compounds. Target is set at 0.00086 TEQ ug/kg dw (Total TEQ). [Norwegian Environment Agency](https://www.miljodirektoratet.no/globalassets/publikasjoner/m1132/m1132.pdf) TEQ values from the [World Health Organization 2005](http://www.who.int/ipcs/assessment/tef_values.pdf)

<br/>

### 1.4 Other information

External advisors/goalkeepers: Anna Sobek

<br/>

2. Data
-------

This prep document is used to generate and explore the following data layers:

-   `cw_con_pcb_bhi2019.csv`
-   `cw_con_pfos_bhi2019.csv`
-   `cw_con_dioxin_bhi2019.csv`
-   `cw_con_penalty_bhi2019.csv`

These are saved to the `layers` folder. Intermediate datasets saved to `data/CW/contaminants/v2019/intermediate` include: `pcb_bio_cleaned.csv`, `pcb_sed_cleaned.csv`, `pfos_bio_cleaned.csv`, `dioxin_bio_cleaned.csv` and `dioxin_sed_cleaned.csv`. All these are derived from or informed by the following raw datasets.

### 2.1 Datasets with Sources

<br/>

#### 2.1.1 PCB Data

**PCB Data in Biota** <!-- dataset save location BHI_share/BHI 2.0/Goals/CW/CON/ContaminantsBiota_PCBs/ContaminantsBiota_PCBs.csv --> <!-- w/ one year more of data...BHI_share/BHI 2.0/Goals/CW/CON/ContaminantsBiota_PCBs/ContaminantsBiota_20Feb07_PCBs.csv -->

| Option                | Specification                  |
|:----------------------|:-------------------------------|
| Year                  | 1990-2017                      |
| Purpose of monitoring | All                            |
| Country               | All                            |
| Monitoring Program    | All                            |
| Parameter Group       | Chlorobiphenyls                |
| Reporting Laboratory  | All                            |
| Analytical laboratory | All                            |
| Geographical Areas    | (HELCOM) ALL HELCOM sub-basins |

**PCB Congeners** <!-- reference codes found here: https://vocab.ices.dk/?CodeID=26983 -->

| Data Code |                                                               |
|:----------|:--------------------------------------------------------------|
| CB180     | 2,2',3,4,4',5,5'-heptachlorobiphenyl                          |
| SCB7      | sum of CBs.- Sum of                                           |
| CB28      | 2,4,4'-trichlorobiphenyl                                      |
| CB52      | 2,2',5,5'-tetrachlorobiphenyl                                 |
| CB101     | 2,2',4,5,5'-pentachlorobiphenyl                               |
| CB118     | 2,3',4,4',5-pentachlorobiphenyl                               |
| CB153     | 2,2',4,4',5,5'-hexachlorobiphenyl                             |
| CB138     | 2,2',3,4,4',5'-hexachlorobiphenyl                             |
| PCB       | polychlorinated biphenyls - Deprecated- Report as single PCBs |
| CB194     | 2,2',3,3',4,4',5,5'-octachlorobiphenyl                        |
| CB105     | 2,3,3',4,4'-pentachlorobiphenyl                               |
| CB110     | 2,3,3',4',6-pentachlorobiphenyl                               |
| CB126     | 3,3',4,4',5-pentachlorobiphenyl                               |
| CB128     | 2,2',3,3',4,4'-hexachlorobiphenyl                             |
| CB149     | 2,2',3,4',5',6-hexachlorobiphenyl                             |
| CB151     | 2,2',3,5,5',6-hexachlorobiphenyl                              |
| CB156     | 2,3,3',4,4',5-hexachlorobiphenyl                              |
| CB157     | 2,3,4,3',4',5'-hexachlorobiphenyl                             |
| CB170     | 2,2',3,3',4,4',5-heptachlorobiphenyl                          |
| CB44      | 2,2',3,5'-tetrachlorobiphenyl                                 |
| CB49      | 2,2',4,5'-tetrachlorobiphenyl                                 |
| CB99      | 2,2',4,4',5-pentachlorobiphenyl                               |
| CB77      | 3,3',4,4'-tetrachlorobiphenyl                                 |
| CB169     | 3,3',4,4',5,5'-hexachlorobiphenyl                             |
| CB31      | 2,4',5-trichlorobiphenyl                                      |
| CB81      | 3,4,4',5-tetrachlorobiphenyl                                  |
| SCB       | sum of CBs.- Specify in method data                           |
| CB189     | 2,3,3',4,4',5,5'-heptachlorobiphenyl                          |
| CB114     | 2,3,4,4',5-pentachlorobiphenyl                                |
| CB123     | 1,1'-Biphenyl, 2,3',4,4',5'-pentachloro-                      |
| CB167     | 2',3,4,4',5,5'-hexachlorobiphenyl                             |
| CB187     | 2,2',3,4',5,5',6-heptachlorobiphenyl                          |
| CB66      | 2,3',4,4'-tetrachlorobiphenyl                                 |
| CB60      | 1,1'-Biphenyl, 2,3,4,4'-tetrachloro-                          |
| CB141     | 2,2',3,4,5,5'-hexachlorobiphenyl                              |
| CB74      | 2,4,4',5-tetrachlorobiphenyl                                  |
| CB206     | 2,2',3,3',4,4',5,5',6-nonachlorobiphenyl                      |
| CB33      | 2',3,4-trichlorobiphenyl                                      |
| CB18      | 2,2',5-trichlorobiphenyl                                      |
| CB183     | 2,2',3,4,4',5',6-heptachlorobiphenyl                          |
| CB47      | 2,2',4,4'-tetrachlorobiphenyl                                 |
| CB209     | 2,2',3,3',4,4',5,5',6,6'-decachlorobiphenyl                   |
| CB51      | 2,2',4,6'-Tetrachlorobiphenyl                                 |
| CB122     | 1,1'-Biphenyl, 2,3,3',4',5'-pentachloro-                      |
| CB138+163 | 2,2',3,4,4',5'-hexachlorobiphenyl                             |

<br/>

**PCBs in Sediment** <!-- dataset save location BHI_share/BHI 2.0/Goals/CW/CON/ContaminantsSediment_PCBs/ContaminantsSediment_PCBs.csv -->

| Option                | Specification         |
|:----------------------|:----------------------|
| Year                  | All8                  |
| Purpose of monitoring | All                   |
| Country               | All                   |
| Monitoring Program    | All                   |
| Parameter Group       | Chlorobiphenyls       |
| Reporting Laboratory  | All                   |
| Analytical laboratory | All                   |
| Geographical Areas    | (ICES) All ICES Areas |

<br/>

#### 2.1.2 PFOS Data

**PFOS in Biota**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/CON/ContaminantsBiota_PFOS/ContaminantsBiota_PFOS.csv --> <!-- w/ one year more of data...BHI_share/BHI 2.0/Goals/CW/CON/ContaminantsBiota_PFOS/ContaminantsBiota_20Feb07_PFOS.csv -->

| Option                | Specification                     |
|:----------------------|:----------------------------------|
| Year                  | 2005-2017 (2005 earliest allowed) |
| Purpose of monitoring | All                               |
| Country               | All                               |
| Monitoring Program    | All                               |
| Parameter Group       | Organofluorines                   |
| Reporting Laboratory  | All                               |
| Analytical laboratory | All                               |
| Geographical Areas    | (HELCOM) ALL HELCOM sub-basins    |

<br/>

#### 2.1.3 Dioxin Data

**Dioxins in Biota**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/CON/ContaminantsBiota_Dioxins/ContaminantsBiota_Dioxins.csv --> <!-- w/ one year more...BHI_share/BHI 2.0/Goals/CW/CON/ContaminantsBiota_Dioxins/ContaminantsBiota_20Feb07_Dioxins.csv -->

| Option                | Specification                     |
|:----------------------|:----------------------------------|
| Year                  | 1998-2017 (1998 earliest allowed) |
| Purpose of monitoring | All                               |
| Country               | All                               |
| Monitoring Program    | All                               |
| Parameter Group       | Dioxins                           |
| Reporting Laboratory  | All                               |
| Analytical laboratory | All                               |
| Geographical Areas    | (HELCOM) ALL HELCOM sub-basins    |

**Dioxin Congeners** <!-- reference codes found here: https://vocab.ices.dk/?CodeID=26986 -->

| Data Code |                                              |
|:----------|:---------------------------------------------|
| CDD1N     | 1 2 3 7 8-pentachlorodibenzo-p-dioxin        |
| CDD4X     | 1 2 3 4 7 8-hexachlorodibenzo-p-dioxin       |
| CDD6P     | 1 2 3 4 6 7 8-heptachlorodibenzo-p-dioxin    |
| CDD6X     | 1 2 3 6 7 8-hexachlorodibenzo-p-dioxin       |
| CDD9X     | 1 2 3 7 8 9-hexachlorodibenzo-p-dioxin       |
| CDDO      | 1 2 3 4 6 7 8 9-octachlorodibenzo-p-dioxin   |
| CDF2N     | 2 3 4 7 8-pentachlorodibenzofuran            |
| CDF2T     | 2 3 7 8-tetrachloro-dibenzofuran             |
| CDF4X     | 2 3 4 6 7 8-hexachlorodibenzofuran           |
| CDF6P     | 1 2 3 4 6 7 8-heptachlorodibenzofuran        |
| CDF6X     | 1 2 3 6 7 8-hexachlorodibenzofuran           |
| CDF9P     | 1 2 3 4 7 8 9-heptachlorodibenzofuran        |
| CDF9X     | 1 2 3 7 8 9-hexachlorodibenzofuran           |
| CDFDN     | 1 2 3 7 8/1 2 3 4 8-pentachloro-dibenzofuran |
| CDFO      | octachloro-dibenzofuran (group)              |
| TCDD      | 2 3 7 8-tetrachlorodibenzo-p-dioxin          |

<br>

**Dioxins in Sediment**
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/CON/ContaminantsSediment_Dioxins/ContaminantsSediment_Dioxins.csv -->

| Option                | Specification         |
|:----------------------|:----------------------|
| Year                  | All                   |
| Purpose of monitoring | All                   |
| Country               | All                   |
| Monitoring Program    | All                   |
| Parameter Group       | Dioxins               |
| Reporting Laboratory  | All                   |
| Analytical laboratory | All                   |
| Geographical Areas    | (ICES) All ICES Areas |

<br>

#### 2.1.4 Concerning Substances Datasets

<!-- datasets save location BHI_share/BHI 2.0/Goals/CW/CON/ContaminantsBiota_ConcerningSubstances -->
<!-- datasets save location BHI_share/BHI 2.0/Goals/CW/CON/ContaminantsSediment_ConcerningSubstances -->
<!-- dataset save location BHI_share/BHI 2.0/Goals/CW/CON/concerning_substances_lookup.csv -->
A list of concerning substances was obtained from [European Chemical Agency Candidate List of substances of very high concern for Authorisation](https://echa.europa.eu/candidate-list-table?p_p_id=disslists_WAR_disslistsportlet&p_p_lifecycle=1&p_p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_pos=2&p_p_col_count=3&_disslists_WAR_disslistsportlet_javax.portlet.action=searchDissLists) (published in accordance with Article 59(10) of the REACH Regulation). These substances were compared with those in the [ICES database](http://dome.ices.dk/views/ContaminantsBiota.aspx). Datasets for substances with ICES records were downloaded and saved in a similar manner as the PCBs, PFOS, and Organofluorines datasets.

------------------------------------------------------------------------

<br>

### 2.2 Centralization & Normalization

<!-- note: remainder of this document incorporates from BHI1.0, along with preliminary contaminants_prep.Rmd sections: -->
<!-- raw_pcb_data_prep.R, raw_dioxin_data_prep.R, and raw_pfos_data_prep.R -->
<!-- station_attribute_prep.R, congener_description.R -->
This preliminary data wrangling includes steps to harmonize and check the 5 datasets used in the Contaminants subgoal:

-   Check number of years with samples by Country and Species, for Contaminants in Biota datasets
-   Rename columns and some variables within columns to improve clarity
-   Standardize by converting units and also between methods of measurement (e.g. wet vs. dry weight or muscle vs liver matrix analyzed)

More details about the standardization and convertions in section 2.2.2.

``` r
## root location of the raw data
dir_rawdata <- file.path(dir_B, "Goals", "CW", "CON")

lapply(
  list(
    c("ContaminantsBiota_20Feb07_PCBs", "pcb_rawdata_bio"),
    c("ContaminantsSediment_PCBs", "pcb_rawdata_sed"),
    c("ContaminantsBiota_20Feb07_PFOS", "pfos_rawdata_bio"),
    c("ContaminantsBiota_20Feb07_Dioxins", "dioxin_rawdata_bio"),
    c("ContaminantsSediment_Dioxins", "dioxin_rawdata_sed")
  ),
  function(x){
    read_csv(
      file.path(dir_rawdata, x[1], paste(x[1], "csv", sep = ".")),
      ## where not read in correctly, column types must be specified ...
      col_types = cols(
        .default = col_character(),
        MYEAR = col_number(), 
        Latitude = col_number(), 
        Longitude = col_number(), 
        NOINP = col_number(),
        Value = col_number(), 
        DETLI = col_number(), 
        LMQNT = col_number(), 
        UNCRT = col_number(),
        tblAnalysisID = col_number(), 
        tblParamID = col_number(), 
        tblBioID = col_number(), 
        tblSampleID = col_number()
      )
    ) %>% assign(x = x[2], envir = .GlobalEnv)
  }
)
```

##### Check number of years with samples by Country

###### PCBs

![](con_prep_files/figure-markdown_github/check%20number%20of%20years%20sample%20by%20country%20in%20pcb%20data-1.png)

------------------------------------------------------------------------

<br>

###### PFOS

![](con_prep_files/figure-markdown_github/check%20number%20of%20dates%20and%20years%20sample%20by%20country%20in%20pfos%20data-1.png)

------------------------------------------------------------------------

<br>

###### Dioxins

![](con_prep_files/figure-markdown_github/check%20number%20of%20dates%20and%20years%20sample%20by%20country%20in%20dioxin%20data-1.png)

------------------------------------------------------------------------

<br>

#### 2.2.1 Rename Fields/Variables

Renaming columns and some variables within columns for clarity. See references: [Contaminants in Biota](http://dome.ices.dk/Download/Contaminants%20and%20effects%20of%20contaminants%20in%20biota.pdf) and [Contaminants in Sediment](http://dome.ices.dk/Download/Contaminants%20and%20effects%20of%20contaminants%20in%20sediment.pdf). For more information about the ICES reporting format for environmental data see [this document](http://ices.dk/marine-data/Documents/ENV/ERF3.2.doc), also referenced in [this document from HELCOM BalticBOOST workshop](https://portal.helcom.fi/meetings/HELCOM%20BalticBOOST%20HZ%20WS%201-2016-323/MeetingDocuments/1-9%20Comparison%20of%20COMBINE%20data%20fields%20to%20EIONET%20data%20fields.pdf) on the HOLAS II hazardous substance assessment.

``` r
rename_vars <- function(dataset){
  
  df_vars_renamed <- dataset %>% 
    
    ## create date, month, year, day columns
    dplyr::mutate(date = as.Date(DATE, "%d/%m/%Y")) %>% 
    
    dplyr::mutate(
      day = lubridate::day(date),
      month = lubridate::month(date), 
      year = lubridate::year(date)) %>% 
    
    ## change column names
    dplyr::rename(
      country = Country, report_institute = RLABO,
      station = STATN, 
      monit_year = MYEAR, date_ices = DATE,
      
      latitude = Latitude, longitude = Longitude,
      
      param_group = PARGROUP, variable = PARAM, 
      value = Value, unit = MUNIT,
      
      species = Species, sex_specimen = SEXCO, num_indiv_subsample = NOINP,
      test_organism = `Test Organism`,
      matrix_analyzed = MATRX, not_used_in_datatype = NODIS,
      
      monit_program = MPROG, monit_purpose = PURPM,
      
      basis_determination = BASIS, qflag = QFLAG,
      vflag = VFLAG, detect_lim = DETLI,
      quant_lim = LMQNT, uncert_val = UNCRT, method_uncert = METCU,
      
      analyt_lab = ALABO, ref_source = REFSK, 
      method_storage = METST, method_pretreat = METPT, 
      method_pur_sep = METPS, method_chem_fix = METFP,
      method_chem_extract = METCX, method_analysis = METOA, 
      formula_calc = FORML,
      
      sub_samp_id = SUBNO, bulk_id = BULKID,
      sampler_type = SMTYP, factor_compli_interp = FINFL,
      
      analyt_method_ref = tblAnalysisID, 
      measurement_ref = tblParamID,
      samp_ref = tblSampleID
    ) %>% 
    
    ## improve clarity of 'basis_determination' and 'matrix_analyzed' column content
    mutate(basis_determination = case_when(
      basis_determination == "L" ~ "lipid weight",
      basis_determination == "W" ~ "wet weight",
      basis_determination == "D" ~ "dry weight"
    )) %>% 
    mutate(matrix_analyzed = case_when(
      matrix_analyzed == "LI" ~ "liver",
      matrix_analyzed == "MU" ~ "muscle",
      matrix_analyzed == "WO" ~ "wholeorganism"
    ))
  
  return(df_vars_renamed)
}

## same names and structures so can apply function to all datasets
lapply(
  list("pcb_rawdata_bio", "pfos_rawdata_bio", "dioxin_rawdata_bio"),
  function(x){
    renameddf <- rename_vars(get(x)) %>% 
      rename(sub_samp_ref = tblBioID)
    assign(str_remove(x, "_rawdata"), renameddf, envir = .GlobalEnv)
  }
)
lapply(
  list("pcb_rawdata_sed", "dioxin_rawdata_sed"),
  function(x){
    renameddf <- rename_vars(get(x)) %>% 
      rename(press_depth = DEPHU, depth_low = DEPHL) %>% 
      mutate(press_depth = as.numeric(press_depth), depth_low = as.numeric(depth_low))
    assign(str_remove(x, "_rawdata"), renameddf, envir = .GlobalEnv)
  }
)
```

------------------------------------------------------------------------

#### 2.2.2 Standardize Units

The main objective of the code below is to clean the contaminants datases set so they contain only values based on wet weight for biota or dry weight for sediment-matrix measurements, in standardized units of ug/kg. This includes the following pre-processing steps:

-   Remove flagged and deprecated data entries
-   Separate `B-BIO` data from the other param group data (`OC-CB`, `OC-DX`, or `O-FL` for PCB, Dioxin, PFOS resp.) for use in lipid basis to wet weight conversion for biota or for conversion to dry weight for sediments; also for calculation of muscle equivalent in PFOS data
-   Take averages where there are duplicate data samples, based on the `sub_samp_ref` value groupings for biota or `samp_ref` value groupings for sediment-matrix measurements
-   If wrangling PFOS dataset, calculate the muscle equivalent of values where matrix analyzed was liver, using [Faxneld et al 2014, table 8](https://www.diva-portal.org/smash/get/diva2:767385/FULLTEXT01.pdf)
-   Convert all congener concentration data to ug/kg, and to wet weight for biota or dry weight for sediment
-   If data were presented in lipid weight, they were converted to wet weight by: (EXLIP%/100)\*(CB conc. lipid weight)

The datasets each contain two main categories (param\_groups) one of which is BBIO. BBIO consists of variables with information about the sample like dry/lipid/wet weight percentage, age, weight and length. The second category contains the congeners concentration information. In stages, these are each manipulated and later rejoined by the sample (`samp_ref` or `sub_samp_ref`) ID numbers.

<br>

**Check Sediment data Coverage before Filtering and Standardizing Units**

Setting the cutoff to 0.05m (concentration measurements from sediment samples taked from within 5cm of the surface) means we retain coverage in the Gulf of Finland for PCBs indicator, and many more measurements in Germany around Kiel. Setting the depth cutoff to any smaller value would result in poor coverage.

``` r
## basemap with baltic countries borders and BHI regions with ID numbers
bhi_rgns_simple <- rmapshaper::ms_simplify(
  input =  sf::st_read(
    dsn = file.path(dirname(dir_B), "Shapefiles", "BHI_shapefile_corrected"), 
    layer = "BHI_shapefile_corrected",
    quiet = TRUE
  )) %>% sf::st_as_sf()

basemap <- ggplot2::ggplot() + 
  geom_sf(
    data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      sf::st_crop(xmin = 0, xmax = 40, ymin = 53, ymax = 67),
    fill = "ivory", color = "lightsteelblue",
    size = 0.1, alpha = 0.8
  ) +
  geom_sf(data = bhi_rgns_simple, fill = NA, size = 0.15, color = "lightsteelblue") +
  scale_x_continuous(limit = c(4, 32)) +
  scale_y_continuous(limit = c(53.5, 66)) +
  theme(panel.background = element_rect(fill = "#F8FBFC", color = "#E2EEF3"))
```

<br>

![](con_prep_files/figure-markdown_github/spatial%20coverage%20for%20sediment%20measurement%20depths-1.png)

<br>

<!-- the following code chunks are modified from raw_data_prep.r scripts in bhi-1.0-archive/baltic2015/prep/CW/contaminants -->
``` r
standardize_con_data <- function(data_renamed, matrix_biota = TRUE, contam_param, rm_vars = NULL){
  
  contam_param <- substr(contam_param, str_length(contam_param)-1, str_length(contam_param))
  
  ## what are the specific parameters?
  ## unique parameter groups and associated variables:
  chk_params <- data_renamed %>% 
    select(param_group, variable) %>% 
    distinct() %>% 
    arrange(param_group, variable)
 
  ## filter and/or remove unneeded variables from data ----
  message("STEP 1: filter and/or unselect unneeded variables and congeners from dataset\n")
  df <- data_renamed %>% 
    ## remove 'suspect' and other non-'acceptable': https://vocab.ices.dk/?ref=58
    filter(!vflag %in% c("S", "C")) %>% 
    ## remove certain variables-- summarized data or depreciated codes
    filter(!variable %in% rm_vars)
  
  if(contam_param != "FL"){
    ## remove samples for liver tissue, keep muscle, and whole organism for length x weight
    df <- filter(df, matrix_analyzed != "liver" | is.na(matrix_analyzed))
  }
  ## for PFOS (contaminant pargroup 'O-FL'), keep liver measurements-- 
  ## all compounds except parameter 'PFOS' measured only in the liver
  ## DRYWT% and EXLIP% have been measured for both muscle and liver
  
  
  ## sediment versus biota datasets, different sample/id variables
  sampvars <- c("sub_samp_id", "samp_ref", "sub_samp_ref")
  if(!matrix_biota){
    sampvars <- setdiff(sampvars, "sub_samp_ref")
  }
  ## check alignment of all measurements with sub_samp_ref for biota, samp_ref for sediment
  ## (most unique besides measurement_ref)
  ## there are more unique measurement_ref but these are for different vars w/in sub_samp_id
  chk_ids <- c()
  for(i in c(sampvars, "measurement_ref")){
    chk_ids <- c(chk_ids, sprintf("%s: %s", i, length(unique(df[[i]]))))
  }
  if(nrow(df %>% filter(is.na(sampvars[length(sampvars)]))) != 0){
    warning(sprintf("there are observations without %s in the dataset", sampvars[length(sampvars)]))
  }
  message(sprintf("NOTE will align by %s when grouping to identify duplicates\n", sampvars[length(sampvars)]))
  
  
  ## BBIO DATA
  
  ## separate out bbio data for lipid to wet-weight conversion ----
  ## B-BIO variable code: http://vocab.ices.dk/?CodeID=51634
  ## b-bio data -- length, weight, fat and lipid content
  ## lipid content, etc. needed to convert lipid basis samples into the wet weight are in the B-BIO column
  message("STEP 2: separate out bbio data and wrangle for use in lipid to wet-weight conversion...\n")
  df_bbio <- df  %>% 
    filter(param_group == "B-BIO") %>%
    select(
      station, latitude, longitude, date, year, 
      species, matrix_analyzed, basis_determination, qflag,
      variable, unit, value,
      !!!syms(sampvars)
    )
  
  ## if there are cm, convert to mm
  df_bbio <- df_bbio %>% 
    left_join(
      read_csv(
        here::here("supplement", "lookup_tabs", "unit_conversion.csv"),
        col_types = cols()
      ) %>% filter(ConvertUnit == "mm"),
      by = c("unit" = "OriginalUnit")
    ) %>% 
    mutate(
      value = ifelse(is.na(ConvertUnit), value, value*ConvertFactor),
      unit = ifelse(is.na(ConvertUnit), unit, "mm")
    ) %>% 
    select(-ConvertUnit, -ConvertFactor)
  
  ## confirm that units of bbio, are one-to-one
  chk_bbio_units <- df_bbio %>% 
    select(variable, unit) %>% 
    distinct()
  
  ## if dealing with sediment, need to keep units for now...
  if(matrix_biota){df_bbio <- select(df_bbio, -unit)}

  chk_num_bbio_dup <- nrow(df_bbio) - nrow(distinct(df_bbio %>% select(-value)))
  
  ## BBIO DUPLICATES
  ## identify, explore, handle bbio duplicates ----
  ## group to identify duplicates; for a given sub_samp_ref only some variables may have duplicates
  grpvars <- c(
    "station", "year", "species", "basis_determination", "matrix_analyzed", "qflag", 
    sampvars, "date", "variable", "longitude", "latitude"
  )
  chk <- c()
  for(i in 1:6){
    bbio_duplicates <- df_bbio %>% 
      group_by(!!!syms(c(grpvars[i], grpvars[7:length(grpvars)]))) %>%
      summarise(n = n()) %>% 
      filter(n > 1) %>% 
      ungroup()
    chk <- c(chk, nrow(filter(bbio_duplicates, n > 1)))
  }
  if(var(chk) != 0){
    message(sprintf(
      "NOTE bbio duplicates dataframe dims vary with grpvars: %s",
      paste(grpvars[which(chk != chk[which.max(tabulate(match(chk, chk)))])], collapse = ", ")
    ))
  }
  
  ## join duplicate table by grouped vars to locate within full bbio dataset
  bbio_duplicates <- df_bbio %>% 
      group_by(!!!syms(grpvars)) %>%
      summarise(n = n()) %>% 
      filter(n > 1) %>% 
      ungroup()
  df_bbio <- left_join(df_bbio, bbio_duplicates, by = grpvars)
  
  ## take averages for variable/sample duplicates
  df_bbio <- bind_rows(
    ## averages of duplicates
    df_bbio %>% 
      filter(n > 1) %>% 
      group_by(!!!syms(grpvars)) %>% 
      summarize(value = mean(value)) %>% 
      ungroup(),
    ## rows of bbio data with no duplicates
    df_bbio %>% filter(is.na(n)) %>% select(-n)
  )
  
  ## check for unique variables and matrix_analyzed, per species
  chk_bbio_vars <- NULL
  if(matrix_biota){
    chk_bbio_vars <- do.call(
      rbind, 
      lapply(
        as.list(unique(df_bbio$species)), 
        function(x){
          cbind(
            df_bbio %>% 
              filter(species == x) %>%
              select(matrix_analyzed, basis_determination, variable) %>% 
              distinct() %>% 
              arrange(variable, matrix_analyzed, basis_determination),
            species = x
          )
        }
      )
    )
  }

  
  ## WIDE BBIO DATA
  ## bbio data wide format ----
  
  ## remove basis_determ. as some bio vars are recorded for whole organism, others for liver or muscle,
  ## no concentration variables measured for whole organism matrix, so don't need for joining...
  ## no problem pivoting wider also indicates uniquely identified by remaining grouping variables
  # unique(filter(df, param_group != "B-BIO")$matrix_analyzed)
  
  if(contam_param == "FL"){
    ## for PFOS we have liver and muscle measurements
    ## need to deal with matrix_analyzed to get 'muscle equivalent' from conc. measurements in liver
    df_bbio_wide <- df_bbio %>% 
      mutate(variable = paste(matrix_analyzed, variable, sep = "_")) %>% 
      select(-matrix_analyzed, -basis_determination)
  } else {
    df_bbio_wide <- df_bbio %>% 
      select(-basis_determination)
  }
  df_bbio_wide <- df_bbio_wide %>% 
    tidyr::pivot_wider(names_from = variable, values_from = value)
  

  ## which columns are needed to uniquely identify and later rejoin with congener conc. data
  ## i.e. at this stage, which subset of grpvars gives groupings such that all n are one?
  # for(i in grpvars){print(paste(i, length(unique(df_bbio_wide[[i]]))))}
  chkgrpvars <- intersect(grpvars, names(df_bbio_wide))
  chk <- chkgrpvars
  for(i in 1:length(chkgrpvars)){
    bbio_wide_duplicates <- df_bbio_wide %>% 
      group_by(!!!syms(setdiff(chk, chkgrpvars[i]))) %>%
      summarise(n = n()) %>% 
      ungroup() %>% 
      filter(n > 1)
    if(nrow(bbio_wide_duplicates) == 0){
      chk <- setdiff(chk, chkgrpvars[i])
    }
  }
  message(sprintf(
    "NOTE minimum group ID variables for later rejoining uniquely with congener data:\n%s\n", 
    paste(chk, collapse = ", ")
  ))
  chk_bbio_wide_ids <- chk
  ## sub_samp_id and samp_ref together are as unique as sub_samp_ref
  # nrow(select(df_bbio_wide, sub_samp_ref) %>% distinct())
  # nrow(select(df_bbio_wide, sub_samp_id, samp_ref) %>% distinct())
  
  
  ## CONGENER CONCENTRATIONS DATA
  ## check congener concentration data, units and duplicates ----
  
  if(!matrix_biota){
    ## units of depth variables meters
    ## only keep sediment measurements in top 2cm, 
    ## or maximum 5cm if filtering to within 2cm of surface yeilds too few data pts...
    df <- df %>% filter(depth_low <= 0.05) 
  }
  
  df_conc <- df %>%
    filter(str_detect(param_group, pattern = contam_param)) %>%
    select(
      station, latitude, longitude, date, year,
      species, matrix_analyzed, basis_determination,
      variable, unit, value, detect_lim, quant_lim, qflag,
      !!!syms(sampvars)
    )
  
  ## convert congener concentration units all to ug/kg
  message("STEP 3: convert congener concentrations units to ug/kg")
  df_conc <- df_conc %>%
    left_join(
      read_csv(
        here::here("supplement", "lookup_tabs", "unit_conversion.csv"),
        col_types = cols()
      ) %>% filter(ConvertUnit == "ug/kg"),
      by = c("unit" = "OriginalUnit")
    ) %>% 
    mutate(
      value = value*ConvertFactor, 
      detect_lim = detect_lim*ConvertFactor,
      unit = "ug/kg"
    ) %>% 
    select(-ConvertUnit, -ConvertFactor)
  
  ## check whether variables vs units relationship is one-to-one
  chk_conc_units <- df_conc %>% 
    select(variable, unit) %>% 
    distinct() %>%
    arrange(variable)
  
  df_conc <- select(df_conc, -unit)
  
  
  ## take averages for congener conc. variable x sample duplicates ----
  
  ## identify duplicates
  ## group to identify duplicates, 
  ## for a given sub_samp_ref only some vars/congeners may have duplicates
  message("STEP 4: averaging, where multiple congener concentrations observations per sub sample\n")
  chk_num_conc_dup <- nrow(df_conc) - nrow(distinct(df_conc %>% select(-value, -detect_lim, -quant_lim)))

  conc_duplicates <- df_conc %>% 
    group_by(!!!syms(grpvars)) %>%
    summarise(n = n()) %>% 
    ## no cases where duplicates with-and-without qflags, else would keep cases unflagged observations...
    # group_by(!!!syms(setdiff(grpvars, "qflag"))) %>%
    # mutate(allflagged = !any(is.na(qflag))) %>%
    # filter(n > 1, !allflagged) %>% ...
    filter(n > 1) %>% 
    ungroup()
  
  ## join duplicates tables by grouped vars to locate them in original dataset
  df_conc <- left_join(df_conc, conc_duplicates, by = grpvars)
  chk_conc_dup <- filter(df_conc, !is.na(n))
  
  df_conc <- bind_rows(
    ## averages of duplicates
    df_conc %>% 
      filter(n > 1) %>%
      group_by(!!!syms(grpvars)) %>% 
      summarize(
        value = mean(value), 
        detect_lim = mean(detect_lim, na.rm = TRUE), 
        quant_lim =  mean(detect_lim, na.rm = TRUE)
      ) %>% 
      ungroup(),
    ## rows of conc data with no duplicates
    df_conc %>% filter(is.na(n)) %>% select(-n)
  )
  
  ## remaining duplicates for biota datasets are due to dual wet/lipid weight records;
  ## of these, keep wet weight records, when has a value
  if(matrix_biota){
    chk <- df_conc %>%
      group_by(!!!syms(setdiff(grpvars, c("basis_determination", "matrix_analyzed")))) %>%
      mutate(n = n(), ww_val = any(basis_determination == "wet weight" & !is.na(value))) %>%
      filter(n > 1) %>% 
      mutate(
        rm_duplicate = 
          (ww_val & basis_determination == "lipid weight")|
          (!ww_val & basis_determination == "wet weight")
      ) %>% 
      ungroup()
    df_conc <- df_conc %>%
      left_join(chk, by = intersect(names(df_conc), names(chk))) %>% 
      mutate(rm_duplicate = ifelse(is.na(rm_duplicate), TRUE, rm_duplicate)) %>% 
      filter(rm_duplicate) %>% 
      select(-n, -ww_val, -rm_duplicate)
  }
  
  ## do not want to spread the data because the qflag, detli etc is unique to each congener
  ## but, spread to check if one row for every unique sub_samp_ref for biota, samp_ref for sediment
  df_conc_wide <- df_conc %>%
    select(-detect_lim, -quant_lim) %>% 
    tidyr::pivot_wider(names_from = variable, values_from = value)
  if(length(unique(df_conc[[sampvars[length(sampvars)]]])) != nrow(distinct(df_conc_wide))){
    message(
      "NOTE conc. subsamples not uniquely identified even after averaging; 
            multiple obs per ID, probably due to qflags\n"
    )
  }
  
  ## for biota convert lipid weight measurements to wet weight ----
  ## UNITS HAVE ALL BEEN CONVERTED TO MICROGRAMS PER KILOGRAM
  ## use the df_bbio_wide datatable to convert values in df_conc table
  df_final <- left_join(df_conc, df_bbio_wide, by = intersect(grpvars, names(df_bbio_wide))) 
  
  if(matrix_biota){
    
    message("STEP 5: converting all congener concentrations in biota to wet weight, muscle equivalence")
    
    ## for PFOS data convert liver values into those for muscle 
    ## using conversion values from Faxneld et al 2014
    ## https://www.diva-portal.org/smash/get/diva2:767385/FULLTEXT01.pdf)
    ## follow methods in the HELCOM core indicator p.9
    ## http://www.helcom.fi/Core%20Indicators/PFOS_HELCOM%20core%20indicator%202016_web%20version.pdf
    ## use the mean liver:muscle ratio for all species (17.9), see Table 8 in Faxneld et al 2014
    if(contam_param == "FL"){
      df_final <- df_final %>% 
        mutate_at(
          vars(c("value", "detect_lim", "quant_lim")), 
          list(pfos_muscle_equiv = ~ case_when(
            variable == "PFOS" & matrix_analyzed == "liver" ~ ./17.9,
            variable == "PFOS" & matrix_analyzed == "muscle" ~ .,
            
            ## NOT IN BHI1.0, BUT INCLUDING FOR INITIAL EXPLORATION...
            variable == "PFOA" & species == "Clupea harengus" & matrix_analyzed == "liver" ~ ./13.4,
            variable == "PFOA" & species == "Clupea harengus" & matrix_analyzed == "muscle" ~ .,
            variable == "PFOA" & species == "Zoarces viviparus" & matrix_analyzed == "liver" ~ ./6.13,
            variable == "PFOA" & species == "Zoarces viviparus" & matrix_analyzed == "muscle" ~ .,
            variable == "PFOA" & species == "Perca fluviatilis" & matrix_analyzed == "liver" ~ ./13.7,
            variable == "PFOA" & species == "Perca fluviatilis" & matrix_analyzed == "muscle" ~ .,
            variable == "PFDA" & species == "Clupea harengus" & matrix_analyzed == "liver" ~ ./19.4,
            variable == "PFDA" & species == "Clupea harengus" & matrix_analyzed == "muscle" ~ .,
            variable == "PFDA" & species == "Zoarces viviparus" & matrix_analyzed == "liver" ~ ./5.69,
            variable == "PFDA" & species == "Zoarces viviparus" & matrix_analyzed == "muscle" ~ .,
            variable == "PFDA" & species == "Perca fluviatiliss" & matrix_analyzed == "liver" ~ ./5.69,
            variable == "PFDA" & species == "Perca fluviatilis" & matrix_analyzed == "muscle" ~ .,
            variable == "PFNA" & species == "Clupea harengus" & matrix_analyzed == "liver" ~ ./15.5,
            variable == "PFNA" & species == "Clupea harengus" & matrix_analyzed == "muscle" ~ .,
            variable == "PFNA" & species == "Zoarces viviparus" & matrix_analyzed == "liver" ~ ./5.62,
            variable == "PFNA" & species == "Zoarces viviparus" & matrix_analyzed == "muscle" ~ .,
            variable == "PFNA" & species == "Perca fluviatilis" & matrix_analyzed == "liver" ~ ./13.7,
            variable == "PFNA" & species == "Perca fluviatilis" & matrix_analyzed == "muscle" ~ .,
            variable == "PFPEDA" & matrix_analyzed == "liver" ~ ./7.4,
            variable == "PFPEDA" & matrix_analyzed == "muscle" ~ .,
            variable == "PFUnDA" & matrix_analyzed == "liver" ~ ./10.1,
            variable == "PFUnDA" & matrix_analyzed == "muscle" ~ .
          ))
        ) %>% 
        mutate_at(
        vars(c("value_pfos_muscle_equiv", "detect_lim_pfos_muscle_equiv", "quant_lim_pfos_muscle_equiv")), 
        ## will probably all be in wet weight already, but just in case...
        list(wet_wgt = ~ case_when(
          basis_determination == "lipid weight" ~ .*(`muscle_EXLIP%`/100), # IS THIS HOW EXLIP% VARIABLE WORKS??
          basis_determination == "dry weight" ~ .*(100/`muscle_DRYWT%`), # IS THIS HOW DRYWT% VARIABLE WORKS????
          basis_determination == "wet weight" ~ .
        ))
      )
    } else {
      df_final <- df_final %>% 
      ## if value is not wet weight convert, otherwise keep value
      mutate_at(
        vars(c("value", "detect_lim", "quant_lim")), 
        list(wet_wgt = ~ case_when(
          basis_determination == "lipid weight" ~ .*(`EXLIP%`/100), # IS THIS HOW EXLIP% VARIABLE WORKS????
          basis_determination == "dry weight" ~ .*(100/`DRYWT%`), # IS THIS HOW DRYWT% VARIABLE WORKS????
          basis_determination == "wet weight" ~ .
        ))
      )
    }
    
  }
  ## for sediment data, still need to convert to dry weights
  if(!matrix_biota){
    ## what to do with the dry weights....
    message("STEP 5: converting all congener concentrations in sediment to dry weight")
    df_final <- df_final %>% 
      ## if value is not dry weight convert, otherwise keep value
      ## even though some have units of ug/kg treat the DRYWT% as percentage...
      mutate_at(
        vars(c("value", "detect_lim", "quant_lim")), 
        list(dry_wgt = ~ case_when(
          basis_determination == "wet weight" ~ .*(`DRYWT%`/100), # IS THIS HOW DRYWT% VARIABLE WORKS????
          basis_determination == "dry weight" ~ .
        ))
      )
  }
  
  ## rejoin with meta information ----
  ## rejoin with info on country and monitoring program info, qflaugs, etc
  message("FINAL STEP: rejoining with meta information\n")
  
  df_final <- df %>% 
    filter(str_detect(param_group, pattern = contam_param)) %>%
    select(
      country, monit_program, monit_purpose, 
      report_institute, 
      monit_year, date_ices, day, month,
      num_indiv_subsample, bulk_id,  
      !!!syms(grpvars)
    ) %>% 
    distinct() %>% 
    right_join(df_final, by = grpvars) %>% 
    mutate(qflagged = ifelse(is.na(qflag), FALSE, TRUE))
  
  ## return wrangled data and checks ----
  
  ## list of checks as part of result
  chks <- list(
    chk_params, chk_ids, 
    chk_bbio_units, chk_num_bbio_dup, chk_bbio_vars,
    chk_conc_units, chk_num_conc_dup, chk_conc_dup
  )
  
  df_final <- filter(df_final, !is.na(value))
    
  if(!all(mutate(group_by(df_final, !!!syms(grpvars)), n = n())$n == 1)){
    warning("NOTE rows of final dataframe are not uniquely identified by grouping variables")
  }
  
  return(list(
    df_final = df_final, 
    groupingvars = grpvars,
    df_conc_wide = df_conc_wide, 
    df_bbio_wide = df_bbio_wide, 
    checks = chks
  ))
}
```

<br>

#### 2.2.3 Wrangle all and Save

Apply the Standardization function defined above to all 5 Contaminants datasets and save the cleaned, harmonized data for later analysis. For PCBs in biota data, remove PCB, SCB, SCB7 variables as these are summarized data or depreciated codes; for PCBs in sediments remove PCB and SCB.

<!-- ideally at this stage we would save to a postgres+postgis database; still in the process of building this database now -->
``` r
dir_interm <- here::here("data", "CW", "contaminants", version_year, "intermediate")

lapply(
  list(
    ## for param definitions see //vocab.ices.dk/?ref=37
    
    ## PCB datasets
    ## remove PCB, SCB, SCB7 - these are summarized data or depreciated codes
    ## OC-CB variable codes: vocab.ices.dk/?CodeID=26983
    list("pcb_bio", TRUE, "OC-CB", c("PCB", "SCB", "SCB7")),
    list("pcb_sed", FALSE, "OC-CB", c("PCB", "SCB")),
    
    ## Dioxin datasets
    list("dioxin_bio", TRUE, "OC-DX", NULL),
    list("dioxin_sed", FALSE, "OC-DX", NULL),

    ## PFOS dataset
    list("pfos_bio", TRUE, "O-FL", NULL)
  ),
  
  function(dat){
    dat_final <- standardize_con_data(get(dat[[1]]), dat[[2]], dat[[3]], dat[[4]]) 
    assign(
      x = paste(dat[[1]], "cleaned", sep = "_"), 
      value = dat_final, 
      envir = .GlobalEnv
    )
    assign(
      x = paste(dat[[1]], "clean_df", sep = "_"), 
      value = dat_final[["df_final"]], 
      envir = .GlobalEnv
    )
    write_csv(
      dat_final[["df_final"]], 
      file.path(dir_interm, paste(dat[[1]], "cleaned.csv", sep = "_"))
    )
  }
)
```

------------------------------------------------------------------------

### 2.3 Initial Data Exploration

<br>

#### 2.3.2 Spatial Distributions of Measurements {.tabset .tabset-fade .tabset-pills}

The maps below show data distribution spatially. Size and color correspond to number of distinct dates measurements were collected for a variable, for the given location within the specified time period (2016 through 2019); larger points with more yellow color indicate greater number of observations while smaller more red points indicate fewer observations. Opacity corresponds to number of years with greater transparency indicating fewer years of data. These faceted plots are created for multiple species; the species selected for visualization are those with the greatest number of measurements recorded across the three indicators.

``` r
make_map <- function(mapdata){
  
  ## make data spatial using lat/lon columns
  df <- sf::st_as_sf(
    x = mapdata, 
    agr = "identity",
    coords = c("longitude", "latitude"), 
    crs = 4326
  )
  ## contaminants datasets overlaying basemap
  speciesmap <- basemap + 
    geom_sf(
      data = df,
      aes(
        size = num_distinct_dates,
        color = num_distinct_dates), 
      show.legend = FALSE,
      alpha = 0.5, 
      shape = 19
    ) +
    scale_color_gradient2(
      low = "red3", 
      mid = "lightcoral",
      high = "lightgoldenrod",
      midpoint = 2.5
    ) +
    facet_wrap(
      c("indicator", "variable"), 
      labeller = label_wrap_gen(width = 35, multi_line = FALSE), 
      ncol = 5
    )

  return(speciesmap)
}
# for(i in unique(plotdf$indicator)){
#   species_obs <- plotdf %>%
#     filter(matrix == "biota", indicator == i) %>%
#     group_by(species) %>%
#     summarise(n = sum(num_distinct_dates)) %>%
#     arrange(desc(n))
#   print(head(species_obs))
# }
```

<br>

##### Clupea Harengus

``` r
make_map(filter(plotdf, matrix == "biota", str_detect(species, "Clupea harengus")))
```

![](con_prep_files/figure-markdown_github/sampling%20maps%20herring-1.png)

------------------------------------------------------------------------

<br>

##### Zoarces Viviparus

``` r
make_map(filter(plotdf, matrix == "biota", str_detect(species, "Zoarces viviparus")))
```

![](con_prep_files/figure-markdown_github/sampling%20maps%20eelpout-1.png)

------------------------------------------------------------------------

<br>

##### Perca Fluviatilis

``` r
make_map(filter(plotdf, matrix == "biota", str_detect(species, "Perca fluviatilis")))
```

![](con_prep_files/figure-markdown_github/sampling%20maps%20perch-1.png)

------------------------------------------------------------------------

<br>

##### Platichthys Flesus

``` r
make_map(filter(plotdf, matrix == "biota", str_detect(species, "Platichthys flesus")))
```

![](con_prep_files/figure-markdown_github/sampling%20maps%20flounder-1.png)

------------------------------------------------------------------------

<br>

##### Mytilus Edulis

``` r
make_map(filter(plotdf, matrix == "biota", str_detect(species, "Mytilus edulis")))
```

![](con_prep_files/figure-markdown_github/sampling%20maps%20blue%20muscles-1.png)

------------------------------------------------------------------------

<br>

##### Uria Aalge

``` r
make_map(filter(plotdf, matrix == "biota", str_detect(species, "Uria aalge")))
```

![](con_prep_files/figure-markdown_github/sampling%20maps%20common%20murre-1.png)

------------------------------------------------------------------------

<br>

##### Sediments

``` r
make_map(filter(plotdf, matrix == "sediment"))
```

![](con_prep_files/figure-markdown_github/sampling%20maps%20sediments-1.png)

------------------------------------------------------------------------

<br>

<br/>

3. Wrangling, Evaluation, and Gapfilling
----------------------------------------

``` r
read_clean_df <- function(filename){
  read_csv(
    file.path(dirname(data_path), "intermediate", filename),
    ## ensure column types are read correctly...
    col_types = cols(
      bulk_id = col_character(),
      qflag = col_character(),
      `LIPIDWT%` = col_number(),
      `DRYWT%` = col_number(),
      `EXLIP%` = col_number(),
      WTMEA = col_number(),
      WTMIN = col_number(),
      WTMAX = col_number(),
      sub_samp_id = col_number(),
      LNMEA = col_number(), 
      LNMAX = col_number(),
      LNMIN = col_number(),
      unit = col_character()
    )
  )
}
```

<!-- **Station Impact Codes** -->
<!-- Note: as of `BHI2.0` the original station library has been replaced by a new web application -->
<!-- [Site monitoring purpose](https://vocab.ices.dk/?ref=42) `monit_purpose` vocabulary reference for codes. -->
<!-- Some sites have had the site type recorded in the ICES station dictionary ([see ICES vocabulary reference for codes](https://vocab.ices.dk/?ref=177). It is pertinent to know which sites are catagorized as:   -->
<!-- **RH** = WFD R(HZ) - Representative of general conditions in terms of hazardous substances   -->
<!-- **B** = WFD B - Baseline/Reference station   -->
<!-- **Any of the codes containing "I"** (Starting with IH or IP) which refers to a specific type of impact at the site.   -->
<!-- **RP** = WFD R(PHY) - Representative of general conditions for nutrients/organic matter  -->
<!-- It appears that only Swedish sites have this information entered. Given only Swedish sites have this information recorded, it seems difficult to use this information to include or exclude sites.   -->
<!-- From the station dictionary definitions: -->
<!-- All_Biota_Data: Data type (DTYPE) CF - all parameters - contaminants and biological effects of contaminants including disease in biota   -->
<!-- Contaminant_parameters_in_biota: Data type (DTYPE) CF - Contaminant parameter groups   -->

------------------------------------------------------------------------

<br>

### 3.1 PCB Indicator

#### 3.1.1 Match BHI Regions

``` r
## use 'read_clean_df' function from above to read cleaned data with correct data types for columns
## use 'join_rgns_info' helper function defined in R/spatial.R
pcb_bio <- join_rgns_info(
  read_clean_df("pcb_bio_cleaned.csv"),
  latlon_vars = c("latitude", "longitude"),
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefiles"),
  return_spatial = FALSE,
  buffer_shp = buffer_sf
)
pcb_sed <- join_rgns_info(
  read_clean_df("pcb_sed_cleaned.csv"),
  latlon_vars = c("latitude", "longitude"),
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefiles"),
  return_spatial = FALSE,
  buffer_shp = buffer_sf
)
```

#### 3.1.2 Filter PCBs to ICES6 Set or PCB7 for Sediment

The PCBs indicator uses only the ICES6 congeners: CB28, CB52, CB101, CB138, CB153, CB180. Additionally, only herring are used from the biota datasets, as they are fairly equally spatially distributed across the baltic sea, while many other species are predominantly in the south.

``` r
## for the PCBs indicator we use the ICE6 congeners:
ices6_congeners <- c("CB101", "CB138", "CB153", "CB180", "CB28", "CB52")
## for PCBs in sediment we use the seven from the Norwegian Environment Agency EQS in sediment for 28 EU priority substances
## note: this sum is also included in the raw dataset as 'SCB7' but as of BHI2.0 all such observations are for the UK 
pcb7sed_congeners <- c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")

all_pcb_bio <- pcb_bio %>% 
  filter(!is.na(value_wet_wgt)) %>% 
  filter(str_detect(species, pattern = "Clupea harengus")) %>%
  ## check that rows are distinct: nrow(pcb_bio) == nrow(distinct(pcb_bio))...
  select(
    variable, value_wet_wgt, detect_lim_wet_wgt, quant_lim_wet_wgt,
    species, num_indiv_subsample, monit_program, monit_purpose, monit_year,
    sub_samp_ref,
    qflagged, qflag, 
    country, Subbasin, HELCOM_ID, BHI_ID, latitude, longitude, station,
    date, year, month, day
  )
## keep all_pcb_bio as separate so can get dioxin-like pcbs from it later
pcb_bio <- filter(all_pcb_bio, variable %in% ices6_congeners)

all_pcb_sed <- pcb_sed %>% 
  filter(!is.na(value_dry_wgt)) %>%
  ## check that rows are distinct: nrow(pcb_sed) == nrow(distinct(pcb_sed))...
  select(
    variable, value_dry_wgt, detect_lim_dry_wgt, quant_lim_dry_wgt,
    num_indiv_subsample, monit_program, monit_purpose, monit_year,
    samp_ref,
    qflagged, qflag, 
    country, Subbasin, HELCOM_ID, BHI_ID, latitude, longitude, station,
    date, year, month, day
  ) %>% 
  filter(!is.na(BHI_ID)|!is.na(Subbasin))
## keep all_pcb_sed as separate so can get dioxin-like pcbs from it later
pcb_sed <- filter(all_pcb_sed, variable %in% pcb7sed_congeners)
```

<br>

##### Checking Outliers/Questionable Measurements {.tabset .tabset-fade .tabset-pills}

###### PCBs in Biota

``` r
## something funky with some stations are these outliers or incorrectly recorded data??
## points in 2014 in poland highlighted in BHI1.0 as questionable, but kept in analysis...
## for some reason it is the nonflagged data that are the problem?
pcb_bio <- mutate(pcb_bio, chk = !(country == "Poland" & year == 2014 & !qflagged))
plotdf <- mutate(pcb_bio, value_adj = ifelse(qflagged, value_wet_wgt/2, value_wet_wgt)) 

makeplot <- function(data){
  ggplot(data) + 
    geom_point(aes(date, value_adj), show.legend = FALSE, size = 0.5, alpha = 0.5, color =  "grey") +
    facet_wrap(~variable, scales = "free_y", nrow = length(unique(data$variable))) +
    labs(x = NULL, y = NULL) +
    theme(plot.title = element_text(size = 8)) 
}
gridExtra::grid.arrange(
  makeplot(plotdf) + 
    geom_point(data = filter(plotdf, !chk), aes(date, value_adj), shape = 1) +
    ggtitle("All Data"),
  makeplot(filter(plotdf, chk)) + 
    ggtitle("W/O Poland year 2014"),
  ncol = 2
)
```

![](con_prep_files/figure-markdown_github/checking%20pcb%20bio%20data%20and%20outliers-1.png)

``` r
# pcb_bio <- filter(pcb_bio, chk) %>% select(-starts_with("chk"))
```

------------------------------------------------------------------------

<br>

###### PCBs in Sediment

``` r
## only a few samples yielding all high concentration values
# distinct(filter(pcb_sed, value_dry_wgt > 60), country, year, month, station, samp_ref)
# distinct(filter(pcb_sed, value_dry_wgt > 120), country, year, month, station, samp_ref)
pcb_sed <- pcb_sed %>% 
  mutate(
    # chk = !(station == "FYN 3" & samp_ref == 293730),
    chk = !(samp_ref %in% c(293793, 293730, 308450, 308411, 2224866))
  )
plotdf <- mutate(pcb_sed, value_adj = ifelse(qflagged, value_dry_wgt/2, value_dry_wgt))

gridExtra::grid.arrange(
  makeplot(plotdf) +
    geom_point(data = filter(plotdf, !chk), aes(date, value_adj), shape = 1) +
    ggtitle("All Data"),
  makeplot(filter(plotdf, chk)) + 
    ggtitle("W/O station FYN 3 sample #293730 Oct2000"),
  ncol = 2
)
```

![](con_prep_files/figure-markdown_github/checking%20pcb%20sed%20data%20and%20outliers-1.png)

``` r
# pcb_sed <- filter(pcb_sed, chk) %>% select(-starts_with("chk"))
```

------------------------------------------------------------------------

<br>

##### Visualize flagged data in Timeseries

Below the data are plotted in congener-subbasin combinations. Colorscale corresponds to number of observations, with lighter blues/greens indicating more observations for the given date-congener-subbasin group, while darker colors indicate fewer observations in the dataset for the given date-congener-subbasin combination.

``` r
## pcb qflag adjustment congeners by basin and congener, 
## red x's indicate flagged data, colorscale indicates number of observations
qflag_timeseries_plot <- function(data, valvarname, plottitle){
  
  plotdf <- data %>% 
    mutate(value_adj = ifelse(qflagged, !!sym(valvarname)/2, !!sym(valvarname))) %>% 
    mutate(value_adj_original = ifelse(qflagged, !!sym(valvarname), NA)) %>% 
    group_by(date, variable, Subbasin) %>% 
    mutate(countObs = n(), meanAdjww = mean(value_adj, na.rm = TRUE)) %>% 
    ungroup()
  
  ggplot(plotdf) + 
    scale_color_distiller(palette = "GnBu", direction = -1) +
    geom_point(aes(date, value_adj, color = countObs), show.legend = FALSE, size = 0.9, alpha = 0.4) +
    geom_point(aes(date, value_adj_original), color = "red", shape = 4, size = 1.2, alpha = 0.7) +
    facet_wrap(
      c("variable", "Subbasin"), 
      labeller = label_wrap_gen(width = 35, multi_line = FALSE), 
      scales = "free_y", 
      ncol = 4
    ) +
    labs(x = NULL, y = NULL, title = plottitle) +
    theme_bw()
}

qflag_timeseries_plot(
  pcb_bio, "value_wet_wgt", 
  "Congener Concentration (ug/kg wet weight) measured in Clupea harengus\n"
)
```

``` r
qflag_timeseries_plot(
  pcb_sed, "value_dry_wgt", 
  "Congener Concentration (ug/kg dry weight) measured in Sediments\n"
)
```

------------------------------------------------------------------------

<br>

#### 3.1.3 Evaluate Flagged Data, Station Impact, & Sampling Patterns

<br>

**What do the `qflags` indicate? Quantification limits, Flags Codes**

-   **&lt;** = less than

-   **&gt;** = greather than

-   **D** = reported value is less than the detection limit (detect\_lim)

-   **Q** = reported value is less than the limit of quantification (quant\_lim)

-   **~** separates multiple flags

``` r
## what do the qflags actually indicate?
chk_pcb_bio_qflags <- pcb_bio %>% 
  select_at(vars(matches("qflag|value|quant|detect"))) %>% 
  filter(qflagged) %>% 
  mutate(qflag = str_split(qflag, "~")) %>% 
  tidyr::unnest(qflag) %>%
  mutate(
    ## from guess-check-plot, it appears the following is mostly correct, with minimal inconsistencies...
    ## < and Q flags indicates less than or equal to quantification limit, 
    ## D flag indicates less than detection limit
    chk = case_when(
      qflag == "D" ~ value_wet_wgt < detect_lim_wet_wgt,
      qflag == "Q" ~ value_wet_wgt <= quant_lim_wet_wgt,
      qflag == "<" ~ value_wet_wgt <= quant_lim_wet_wgt
    )
  )
## from guess-check-plot, it appears the following is mostly correct, with minimal inconsistencies...
## < and Q flags indicates less than or equal to quantification limit, 
## D flag indicates less than detection limit
ggplot(chk_pcb_bio_qflags) +
  geom_bar(aes(chk, fill = chk), position = position_dodge()) +
  scale_fill_manual(values = c("firebrick", "paleturquoise3"), na.value = "whitesmoke") +
  facet_wrap(~qflag, nrow = 1, scales = "free") +
  labs(
    x = "\nConc. value ≤ Quantification Limit ('<' and 'Q'), Conc. value < Detection Limit ('D')", 
    y = NULL, 
    fill = NULL
  )  +
  theme_dark()
```

![](con_prep_files/figure-markdown_github/what%20do%20the%20qflags%20indicate-1.png)

------------------------------------------------------------------------

<br>

##### Congener-Sums Timeseries by Country {.tabset .tabset-fade .tabset-pills}

**Defining two approaches to deal with flagged data**

The function below aggregates the data spreading by variable (so each congner has a column and each date/location/sample is a single row) and summing the congeners rowwise to get date/location/sample specific concentration sums of the specified congners. The function allows for two approaches to handling flagged data: "NoQflag" which removes the flagged observations or "Adjusted" which adjust values. Adjustment of values would ideally use detect\_lim/2, however since detect\_lim values are not always provided, we apply the transformation to the reported data value.

``` r
qflag_adjust <- function(dataset, indicator, congeners = NULL, matrix = "bio", sumcongeners = TRUE, approach = "Adjusted"){
  
  ## Two alternative approaches for summarizing the data:
  ## Adjusted: sum the adjusted values, for the 6 ICES congeners
  ## NoQflag: sum only observation with no qflagged values
  if(approach == "NoQflag"){
    dataset <- filter(dataset, !qflagged)
  }
  
  if(indicator != "dioxin"){
    if(matrix == "bio"){
      dataset <- dataset %>% 
        rename(value_plot = value_wet_wgt) %>% 
        select(-detect_lim_wet_wgt, -quant_lim_wet_wgt)
    } else {
      dataset <- dataset %>% 
        rename(value_plot = value_dry_wgt) %>% 
        select(-detect_lim_dry_wgt, -quant_lim_dry_wgt)
    }
  } else {
    dataset <- dataset %>% 
      rename(value_plot = value_teq) %>% 
      select(-ends_with("_lim_wet_wgt"))
  }
  
  ## aggregate into single date-location values:
  ## sum concentration after flagged data adjustment and date-location averages per congener
  data_Qadjust_sums <- dataset %>% 
    ## adjust for qflags
    ## NOTE if all qflagged are removed, all adjusted values equal original values
    mutate(value_adj = ifelse(qflagged, value_plot/2, value_plot)) %>% 
    ## after dealing with flagged values, may have duplicates; take mean of these 
    select(-qflag, -qflagged, -value_plot) %>% 
    tidyr::pivot_wider(
      names_from = variable, 
      values_from = value_adj,
      values_fn = list(value_adj = mean)
    )
  
  if(indicator == "dioxin"){
    data_Qadjust_sums <- data_Qadjust_sums %>% 
      mutate(sum_congeners = select(., congeners) %>% rowSums(na.rm = TRUE)) %>% 
      mutate(num_congener_in_sum = rowSums(!is.na(select(., congeners))))
  } else {
    ## sum congeners by date and location
    ## remove cases where not all congeners are observed by filtering NAs
    if(sumcongeners & !is.null(congeners)){
      data_Qadjust_sums <- data_Qadjust_sums %>% 
        mutate(sum_congeners = select(., congeners) %>% rowSums()) %>% 
        filter(!is.na(sum_congeners))
    }
  }
  return(data_Qadjust_sums)
}

pcb_bio_qflag_adjust <- qflag_adjust(pcb_bio, "pcb", congeners = ices6_congeners)
pcb_sed_qflag_adjust <- qflag_adjust(pcb_sed, "pcb", congeners = pcb7sed_congeners, matrix = "sed")

pcb_bio_noqflag <- qflag_adjust(pcb_bio, "pcb", congeners = ices6_congeners, approach = "NoQflag")
pcb_sed_noqflag <- qflag_adjust(pcb_sed, "pcb", congeners = pcb7sed_congeners, matrix = "sed", approach = "NoQflag")
```

<!-- To visualize by country or basin pass "country" or "subbasin" respectively to `cntry_or_basin` arguement  -->
``` r
## timeseries plots, congeners by country
plot_cwcon_initial <- function(dataset, col_pal, contam_param = "OC-CB", matrix = "bio", cntry_or_basin = "subbasin"){
  
  ## one of OC-CB, OC-DX or O-FL; becomes OC, DX, or FL respectively
  contam_param <- substr(contam_param, str_length(contam_param)-1, str_length(contam_param))
  if(contam_param == "FL"){contam_param <- "PF"}
  if(contam_param == "DX"){contam_param <- "CD"}
  cntry_or_basin <- str_to_lower(cntry_or_basin)
  
  dat <- dataset %>% 
    rename(subbasin = Subbasin) %>% 
    select(month, Year = year, zone = cntry_or_basin, contains(contam_param)) %>% 
    filter(!is.na(zone), Year > 2000) %>% 
    tidyr::pivot_longer(
      cols = starts_with(contam_param), 
      names_to = "Congener", 
      values_to = "value_plot"
    ) %>%  
    group_by(Congener, month, Year, zone) %>%
    summarize(Value = mean(value_plot, na.rm = TRUE) %>% round(3)) %>%
    ungroup() %>%
    mutate(Date = as.Date(paste("1", month, Year, sep = "-"), tryFormats = "%d-%m-%Y"))
  
  ## reorder factor levels of congeners so colors will match across plots for same congeners
  if(contam_param == "CB"){
    if(isTRUE(all.equal(names(select(dataset, starts_with(contam_param))), ices6_congeners))){
      dat$Congener <- factor(dat$Congener, levels = ices6_congeners)
    }
    if(isTRUE(all.equal(names(select(dataset, starts_with(contam_param))), pcb7sed_congeners))){
      dat$Congener <- factor(dat$Congener, levels = c(ices6_congeners, "CB118"))
    }
    dioxinlike_pcbs <- c("CB118","CB156","CB105","CB169","CB167","CB77","CB157")
    if(isTRUE(all.equal(names(select(dataset, starts_with(contam_param))), dioxinlike_pcbs))){
      dat$Congener <- factor(dat$Congener, levels = dioxinlike_pcbs)
    }
  }
  if(contam_param == "CD"){
    sed_dioxins <- c(
      "CDD1N","CDD4X","CDD6P","CDD6X","CDD9X","CDDO","CDF2N","CDF2T",
      "CDF4X","CDF6P","CDF6X","CDF9P","CDF9X","CDFO","TCDD"
    )
    if(matrix == "sed"){
      dat$Congener <- factor(dat$Congener, levels = sed_dioxins)
    } else {dat$Congener <- factor(dat$Congener, levels = c(sed_dioxins, "CDFDN"))}
  }
  
  
  ## timeseries plots of all congeners by country
  plot <- ggplot(data = dat, aes(Date, Value, fill = Congener)) +
    geom_col(position = "stack", show.legend = FALSE) +
    scale_x_date(
      breaks = function(x){seq.Date(from = min(dat$Date), to = max(dat$Date), by = "3 months")},
      date_labels = "%b %Y"
    ) +
    scale_fill_manual(values = col_pal) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 45, size = 6)) +
    facet_wrap(~zone, scales = "free_y", ncol = 1)
  
  return(plot) # return(plotly::ggplotly(plot))
}
```

<br>

**Visualize Timeseries, Monthly Sums**

###### PCB Congeners in Biota

``` r
## create color palettes
fullpal <- c(
  RColorBrewer::brewer.pal(8, "Dark2"),
  RColorBrewer::brewer.pal(9, "Set1")
)
cols <- colorRampPalette(fullpal)(42)[sample(1:42, size = 6)]
plot_cwcon_initial(pcb_bio_qflag_adjust, col_pal = cols, contam_param = "OC-CB", cntry_or_basin = "country")
```

![](con_prep_files/figure-markdown_github/congeners%20timeseries%20monthly%20by%20basin%20for%20biota-1.png)

``` r
# plotly::ggplotly(plot_cwcon_initial(pcb_bio_qflag_adjust, col_pal = cols, contam_param = "OC-CB", cntry_or_basin = "country"))
```

------------------------------------------------------------------------

<br>

###### PCB Congeners in Sediments

``` r
plot_cwcon_initial(pcb_sed_qflag_adjust, c(cols, "turquoise"), "OC-CB", "sed", "country")
```

![](con_prep_files/figure-markdown_github/congeners%20timeseries%20monthly%20by%20basin%20for%20sediments-1.png)

------------------------------------------------------------------------

<br>

##### Spatial distributions of PCB sampling Locations

``` r
if(!exists("bhi_rgns_simple")|!"sf" %in% class(bhi_rgns_simple)){
  bhi_rgns_simple <- rmapshaper::ms_simplify(
  input =  sf::st_read(
    dsn = file.path(dirname(dir_B), "Shapefiles", "BHI_shapefile_corrected"), 
    layer = "BHI_shapefile_corrected",
    quiet = TRUE
  )) %>% sf::st_as_sf()
}

map_cwcon_initial <- function(dataset, yr, obs_count_limits, matrix = NULL){
  
  if(!"Matrix" %in% names(dataset)){
    dataset <- dataset %>% 
      mutate(Matrix = matrix)
  } else {
    dataset <- dataset %>% 
      mutate(Matrix = ifelse(
        str_detect(Matrix, "bio"), "biota",
        ifelse(str_detect(Matrix, "sed"), "sediment", NA))
      )
    if(nrow(filter(dataset, is.na(Matrix))) > 0){
      message("missing matrix value (bio or sed) in some rows")
    }
  }
  
  df <- left_join(
    bhi_rgns_simple %>% 
      # select(BHI_ID) %>% 
      # mutate(BHI_ID = as.character(BHI_ID)),
      select(HELCOM_ID) %>% 
      mutate(HELCOM_ID = as.character(HELCOM_ID)),
    dataset %>% 
      filter(year %in% yr) %>% 
      # group_by(BHI_ID, Matrix) %>% 
      group_by(Subbasin, HELCOM_ID, Matrix) %>% 
      summarise(
        meanVal = mean(sum_congeners_datemean, na.rm = TRUE),
        matrix_ObservCount = n(),
        numLocation = n_distinct(latitude, longitude)
      ) %>% 
      # group_by(BHI_ID) %>% 
      group_by(Subbasin, HELCOM_ID) %>% 
      mutate(ObservCount = sum(matrix_ObservCount)),
    # by = "BHI_ID"
    by = "HELCOM_ID"
  )
  
  map <- ggplot(df) + 
    geom_sf(aes(fill = ObservCount), color = "burlywood", size = 0.2, alpha = 0.95) +
    scale_fill_gradient(
      low = "midnightblue", 
      high = "lightcyan", 
      na.value = "whitesmoke", 
      limits = obs_count_limits,
      breaks = round(seq(0,6)*max(obs_count_limits)/6, 1)
    ) +
    geom_sf(
      data = st_as_sf(dataset, coords = c("longitude", "latitude"), crs = 4326) %>% filter(year %in% yr), 
      mapping = aes(color = Matrix, shape = Matrix),
      size = 3
    ) +
    scale_color_manual(values = c("limegreen", "brown1")) +
    scale_shape_manual(values = c(13, 13)) +
    theme_linedraw() +
    theme(legend.position = c(0.1, 0.78), legend.background = element_rect(color = "grey")) +
    guides(fill = guide_legend(order = 2))
  
  df <- df %>% 
    st_drop_geometry() %>% 
    filter(!is.na(Matrix)) %>% 
    distinct() %>% 
    right_join(
      tidyr::expand_grid(
        Subbasin = as.character(unique(bhi_rgns_simple$Subbasin)), 
        Matrix = c("biota", "sediment")
      ), 
      by = c("Subbasin", "Matrix")
    )
  
  if(length(unique(dataset$Matrix)) == 1){df <- filter(df, Matrix == "biota")}
  
  basinplot <- ggplot(df) +
    geom_point(
      aes(Subbasin, meanVal, fill = matrix_ObservCount, size = numLocation),
      color = "burlywood", shape =  21
    ) +
    labs(x = NULL, y = "Mean concentration (ug/kg)\n", color = NULL) +
    scale_fill_gradient(
      low = "midnightblue", 
      high = "lightcyan", 
      na.value = "whitesmoke", 
      limits = obs_count_limits
    ) +
    facet_grid(rows = vars(Matrix), scales = "free") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1), 
      legend.position = c(0.87, 0.82),
      legend.background = element_rect(color = "grey", fill = NA),
      strip.text.y = element_text(size = 8, angle = 0)
    ) +
    guides(fill = FALSE)
  
  return(list(map = map, basinplot = basinplot))
}

## bind resulting dataset to plot together and evaluate....
pcb_qflag_adjust_datemeans <- bind_rows(
  pcb_bio_qflag_adjust %>% 
    group_by(date, day, month, year, latitude, longitude, country, Subbasin, HELCOM_ID, BHI_ID) %>% 
    summarize(sum_congeners_datemean = mean(sum_congeners, na.rm = TRUE)) %>% 
    mutate(Matrix = "bio"),
  pcb_sed_qflag_adjust %>% 
    group_by(date, day, month, year, latitude, longitude, country, Subbasin, HELCOM_ID, BHI_ID) %>% 
    summarize(sum_congeners_datemean = mean(sum_congeners, na.rm = TRUE)) %>% 
    mutate(Matrix = "sed")
) %>% ungroup()

pcb_noqflag_datemeans <- bind_rows(
  pcb_bio_noqflag %>% 
    group_by(date, day, month, year, latitude, longitude, country, Subbasin, HELCOM_ID, BHI_ID) %>% 
    summarize(sum_congeners_datemean = mean(sum_congeners, na.rm = TRUE)) %>% 
    mutate(Matrix = "bio"),
  pcb_sed_noqflag %>% 
    group_by(date, day, month, year, latitude, longitude, country, Subbasin, HELCOM_ID, BHI_ID) %>% 
    summarize(sum_congeners_datemean = mean(sum_congeners, na.rm = TRUE)) %>% 
    mutate(Matrix = "sed")
) %>% ungroup()
```

``` r
pcb2014to2019 <- map_cwcon_initial(pcb_qflag_adjust_datemeans, yr = 2014:2018,  c(0, 15), matrix = "both")
gridExtra::grid.arrange(pcb2014to2019$map, pcb2014to2019$basinplot, nrow = 1, widths = c(1.2, 1))
```

<img src="con_prep_files/figure-markdown_github/map spatial dist of PCBs multiple years-1.png" width="120%" />

------------------------------------------------------------------------

<br>

#### 3.1.4 Status and Trend Options

**Most-recent Years in Data Layers for each Approach to Flagged Data**

``` r
htmlTable::htmlTable(
  full_join(
    pcb_qflag_adjust_datemeans %>%
      arrange(year) %>%
      group_by(Subbasin) %>%
      summarise(lastyr_Adjusted = last(year)) %>% 
      filter(!is.na(Subbasin)),
    pcb_noqflag_datemeans %>%
      arrange(year) %>%
      group_by(Subbasin) %>%
      summarise(lastyr_NoQflag = last(year)) %>% 
      filter(!is.na(Subbasin)),
    by = "Subbasin"
  ) %>% select(lastyr_Adjusted, lastyr_NoQflag, Subbasin),
  align = "rcl", 
  rnames = FALSE,
  css.cell = c(
    rep("padding-left: 5em; padding-right: 2em", 2),
    "padding-left: 14em; padding-right: 9em"
  ),
  col.rgroup = c("none", "#F7F7F7")
)
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: right;">
lastyr\_Adjusted
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
lastyr\_NoQflag
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;">
Subbasin
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2014
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2014
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Aland Sea
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2015
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2015
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Arkona Basin
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2016
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2014
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Bay of Mecklenburg
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Bornholm Basin
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Bothnian Bay
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Bothnian Sea
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Eastern Gotland Basin
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Gdansk Basin
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2016
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2014
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Great Belt
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Gulf of Finland
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Gulf of Riga
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2014
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2014
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Kattegat
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2016
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2014
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Kiel Bay
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2016
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2016
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Northern Baltic Proper
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2016
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2016
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
The Quark
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2008
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2008
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
The Sound
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; border-bottom: 2px solid grey; text-align: right;">
2014
</td>
<td style="padding-left: 5em; padding-right: 2em; border-bottom: 2px solid grey; text-align: center;">
2014
</td>
<td style="padding-left: 14em; padding-right: 9em; border-bottom: 2px solid grey; text-align: left;">
Western Gotland Basin
</td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

<br>

**A function to calculate the Indicators**

The function below takes as inputs: datalayer, years across which data is to be aggregated in status calculation, the biota reference point for the contaminant (health threshold in same units as datalayer values) and the sediment reference point, the trend-calculation approach, and trend lag i.e. the number of years we want to project into the future. Default approach for calculating the trend (scaledObs) is to scale the observations by the matrix-and-contaminant-specific reference point, capping at one, then group by spatial unit (BHI region or Subbasin) and matrix, and regress the scaled values against year; the year coefficient multiplied by `trendlag` becomes the trend score. Other approaches considered are: calculating trend with zscores of data, or using a mixed-effects model to try to account for different stations. See commented code below for more details.

``` r
cwcon_indicators <- function(dat, yrs, bio_thresh = 75, sed_thresh = 4.1, approach = "relativeChange", trendlag = 5){
  
  result <- list()
  
  joindf <- bhi_rgns_simple %>% 
    st_drop_geometry() %>% 
    select(-Area_km2) %>% 
    mutate(
      Subbasin = as.character(Subbasin), 
      HELCOM_ID = as.character(HELCOM_ID),
      rgn_nam = as.character(rgn_nam), 
      rgn_key = as.character(rgn_key)
    )
  
  ## STATUS CALCULATIONS ----
  ## calculate status by subbasins and by bhi regions
  
  result[["basin_status"]] <- dat %>%
    filter(year %in% yrs) %>% 
    ## NOTE: CHANGE METHOD FROM BHI1.0
    ## compare to reference point and cap at one before averaging concentrations,
    ## effectively giving more weight to the few higher-concentration values
    mutate(
      health_threshold = ifelse(str_detect(Matrix, "^bio"), bio_thresh, sed_thresh),
      ratio = sum_congeners_datemean/health_threshold,
      ## using pmin to cap the status at one
      dateloc_status = pmin(1, 1/ratio)
    ) %>%
    group_by(HELCOM_ID, Subbasin, Matrix) %>% 
    summarize(
      ## number of date-location specific (aggregated) data points contributing to status
      ## plus some other summary statisics of interest
      num_dateloc_pts = n(),
      status = mean(dateloc_status, na.rm = TRUE),
      max_dateloc_pts = max(sum_congeners_datemean, na.rm = TRUE),
      min_dateloc_pts = min(sum_congeners_datemean, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(HELCOM_ID, Subbasin) %>% 
    summarize(status = mean(status, na.rm = TRUE)) %>% # status from biota & sediment data combined w equal wgt
    right_join(joindf, by = c("Subbasin", "HELCOM_ID"))
  
  result[["bhirgn_status"]] <- dat %>%
    filter(year %in% yrs) %>% 
    mutate(
      health_threshold = ifelse(str_detect(Matrix, "^bio"), bio_thresh, sed_thresh),
      ratio = sum_congeners_datemean/health_threshold,
      ## using pmin to cap the status at one
      dateloc_status = pmin(1, 1/ratio)
    ) %>%
    group_by(BHI_ID, Matrix) %>% 
    summarize(
      ## number of date-location specific (aggregated) data points contributing to status
      ## plus some other summary statisics of interest
      num_dateloc_pts = n(),
      status = mean(dateloc_status, na.rm = TRUE),
      max_dateloc_pts = max(sum_congeners_datemean, na.rm = TRUE),
      min_dateloc_pts = min(sum_congeners_datemean, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(BHI_ID) %>% 
    summarize(status = mean(status, na.rm = TRUE)) %>%
    right_join(joindf, by = c("BHI_ID"))


  ## TREND CALCULATIONS ----
  ## calculate trend by subbasins and by bhi regions; use method specified in function args
  ## three approaches to calculating trend: mixed-effects, linear model
  for(zn in c("basin", "bhirgn")){
    
    ## ten years used in contaminants trend calculations
    trendyrs <- (max(yrs)-9):max(yrs)
    
    if(zn == "basin"){
      grpvars <- c("HELCOM_ID", "Subbasin")
      lstname <- "basin_trend"
    } else {
      grpvars <- c("BHI_ID")
      lstname <- "bhirgn_trend"
    }
    
    if(approach == "relativeChange"){
      ## trend approach 1: by observations scaled w.r.t. to reference point
      lm_dat <- dat %>% 
        filter(year %in% trendyrs) %>% 
        group_by(!!!syms(grpvars), Matrix) %>% 
        mutate(
          trendcalc_nyrs = n_distinct(year),
          num_obs = n()
        ) %>% 
        ## how much data is enough data to calculate short-term trends?
        filter(trendcalc_nyrs >= 6|(trendcalc_nyrs >= 4 & num_obs > 8))
      
      lm_estim <- lm_dat %>% 
        do(trend_mdl = lm(sum_congeners_datemean ~ year, data = .)) %>%
        mutate(
          health_threshold = ifelse(Matrix == "bio", bio_thresh, sed_thresh),
          ## change w.r.t. health thresholds...
          expectedchange5yrs = coef(trend_mdl)["year"]*trendlag,
          trendscore = -expectedchange5yrs/health_threshold,
          ## plotting variables, to visualize
          current_estimated_conc = predict(trend_mdl, data.frame(year = max(yrs))),
          projected_conc = predict(trend_mdl, data.frame(year = max(yrs)+trendlag))
        ) %>% 
        ungroup()
      ## plot to check...
      # ggplot(left_join(lm_dat, select(lm_estim, Subbasin, current_estimated_conc, projected_conc))) +
      #   geom_point(aes(x = year, y = sum_congeners_datemean, color = Matrix)) +
      #   geom_vline(xintercept = max(yrs)) +
      #   geom_vline(xintercept = max(yrs)+trendlag) +
      #   geom_hline(aes(yintercept = projected_conc, color = Matrix), alpha = 0.3) +
      #   geom_hline(aes(yintercept = current_estimated_conc, color = Matrix)) +
      #   facet_wrap(~Subbasin, scales = "free") +
      #   labs(x = "Year", y = "Congeners Concentration")
      
      result[[lstname]] <- lm_estim %>% 
        select(-trend_mdl) %>% 
        group_by(!!!syms(grpvars)) %>% 
        summarize(trend = mean(trendscore, na.rm = TRUE) %>% round(3)) %>%
        ## constrain trend values between one and negative one
        mutate(trend = ifelse(-1 <= trend & trend <= 1, trend, trend*abs(1/trend))) %>% 
        right_join(joindf, by = grpvars)
    }
    
    ## trend approach 2: calculate trend with zscores of data
    if(approach == "zscores"){
      dat_zscore <- dat %>% 
        filter(year %in% trendyrs) %>% 
        group_by(!!!syms(grpvars), Matrix) %>% 
        mutate(
          trendcalc_nyrs = n_distinct(year),
          mn = mean(sum_congeners_datemean),
          sd = sd(sum_congeners_datemean),
          zscore = (sum_congeners_datemean-mn)/sd
        )
      if(any(dat_zscore$trendcalc_nyrs < 5)){
        message("some trends calculated with fewer than five years")
      }
      if(any(is.na(dat_zscore$zscore))){
        message(sprintf(
          "zscore NAs (%s cases) caused trends calculation to fail", 
          nrow(filter(datalayer_zscore, is.na(zscore)))
        ))
      }
      result[[lstname]] <- dat_zscore %>% 
        filter(!is.na(zscore)) %>% 
        group_by(!!!syms(grpvars), Matrix) %>% 
        do(trend_mdl = lm(zscore ~ year, data = .)) %>% 
        mutate(trend = coef(trend_mdl)["year"]*trendlag) %>% 
        ungroup() %>% 
        select(-trend_mdl) %>% 
        group_by(!!!syms(grpvars)) %>% 
        summarize(trend = mean(trend, na.rm = TRUE)) %>% 
        right_join(joindf, by = grpvars)
    }
    
    ## trend approach 3: mixed effects
    ## current error: not enough observations per group...
    ## number of levels of each grouping factor must be < number of observations 
    ## would use the following piece of code to fit model.....
    # do({
    #   if(distinct(., latitude) %>% nrow() > 1){
    #     (trend_mdl = lme4::lmer(zscore ~ year + (1|latitude), data = .))
    #   } else {(trend_mdl = lm(zscore ~ year, data = .))}
    # }) %>% 
  }
  
  return(result)
}
```

**Compare two approaches to deal with flagged data, for BHI regions vs Basin**

1.  Start with mean ICES6 conc. by date and location (unique observations)
2.  Scale observations compared to the relevant contaminant-and-matrix-relevant reference point, capping at one
3.  Calculate status as mean of all scaled obs. for past 5 years in either BHI region or basin
4.  Compare by-basin and by-BHI region results for status and for trend, assess number of data points contributing

``` r
## using function defined above, investigate:
## (1) adjusting vs excluding flagged data, and 
## (2) aggregating data by subbasin versus BHI regions in calculating status and trend
pcb_qflag_adjust_indicators <- cwcon_indicators(pcb_qflag_adjust_datemeans, yrs = 2014:2018)
pcb_noqflag_indicators <- cwcon_indicators(pcb_noqflag_datemeans, yrs = 2014:2018)
## to try with biota data only...
# pcb_qflag_adjust_indicators <- cwcon_indicators(pcb_qflag_adjust_datemeans %>% filter(Matrix == "bio"), yrs = 2015:2019)
# pcb_noqflag_indicators <- cwcon_indicators(pcb_noqflag_datemeans %>% filter(Matrix == "bio"), yrs = 2015:2019)

pcb_status <- bind_rows(
  pcb_qflag_adjust_indicators$basin_status %>% 
    mutate(approach = "Adjusted", spatialunits = "Subbasins"),
  pcb_qflag_adjust_indicators$bhirgn_status %>% 
    mutate(approach = "Adjusted", spatialunits = "BHI Regions"),
  pcb_noqflag_indicators$basin_status %>% 
    mutate(approach = "NoQflag", spatialunits = "Subbasins"),
  pcb_noqflag_indicators$bhirgn_status %>% 
    mutate(approach = "NoQflag", spatialunits = "BHI Regions")
)
pcb_status <- pcb_status %>%
  mutate(Region = paste(Subbasin, rgn_nam, sep = ", ")) %>% 
  mutate(approach_sp_unit = paste(spatialunits, approach))
pcb_status$spatialunits <- factor(pcb_status$spatialunits, levels = c("Subbasins", "BHI Regions"))

statuspal <- colorRampPalette(fullpal)(42)[sample(1:42, size = 18)]

ggplot(pcb_status, aes(Region, status, fill = Subbasin)) +
  geom_col(position = position_dodge(), color = "grey", alpha = 0.7, show.legend = FALSE) + 
  scale_fill_manual(values = statuspal) +
  facet_grid(rows = vars(approach), cols = vars(spatialunits)) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 7))
```

![](con_prep_files/figure-markdown_github/investigate%20approaches%20to%20qflags%20and%20summarizing%20by%20region%20vs%20basin%20pcb%20indicator-1.png)

<br>

#### 3.1.5 Save PCB contaminants layer and intermediate datasets

``` r
write_csv(
  pcb_qflag_adjust_datemeans %>% 
    select(
      region_id = BHI_ID,
      latitude, longitude, 
      year,
      value = sum_congeners_datemean, 
      matrix = Matrix
    ), 
  file.path(dir_layers, sprintf("cw_con_pcb_bhi%s.csv", assess_year))
)
```

<br>

#### 3.1.6 Methods discussion

Only herring (no other species) used from biota dataset, because herring are most equally spatially spaced across the baltic sea, whereas others are mostly in the south.

**Status formula, ICES6 PCBs Biota and 7PCBs in Sediment**

*X*<sub>ICES6</sub> = min{bio\_reference\_point/mean\_ICES6\_region, 1}

*X*<sub>7sedPCB</sub> = min{sed\_reference\_point/mean\_7sedPCB\_region, 1}

*X*<sub>PCB</sub> = (*X*<sub>ICES6</sub> + *X*<sub>7sedPCB</sub>)/2

bio\_reference\_point = health\_threshold = 75*u**g*/*k**g* wet weight

sed\_reference\_point = health\_threshold = 4.1*u**g*/*k**g* dry weight

Score scaled between 0 and 1. If value is below 75, score = 1.

<br>

**Conclusions & Decisions regarding Status and Trend Options**

-   Including the qflag-adjusted values lowers the mean concentration by date and location
-   Including qflag-adjusted values also provides more observations in the Kattegat, The Quark, and W. Gotland Basin
-   When aggregating by BHI regions, more observations for Regions 1, 11, 26, 35, 36, 39, 41, 42 when including qflagged values
-   Using data including qflag-adjusted (this could lower values),

-   Use 5 year mean ICES6 concentration for status
-   Use the five status years plus another five prior for the regression that factors into trend calculation

<br>

**Trend Considerations**

-   Work on mixed effect model for trends?
-   Need to think about the interpretation of the data treatment. (a) If use raw observations, then normalize (zscore data), then fit trend, if get increase or decrease but all values are below the threshold, does it make sense to apply a change in the trend to the status? Would we really think the future status will be lower? (b) If take all raw observations, calculate "status" as done for the mean value, then fit trend, is this more true to the idea that variation below the human health threshold should not affect the trajectory of the future status?
-   Need to think if simple linear regression is okay, or if need to account for site?

<br>

**`BHI1.0` discussions with Anna Sobek **

-   Indicator choice: We agreed that *ICES6 is the best* option
-   Decision about use of qflagg-adjusted data: *use qflagged data with the adjustement* of (congener conc/2)
-   Decision about spatial scale of the data: decide best approach is to *calculate for each basin*
-   Trend decision: best approach is first convert individual observations to a "status" relative to the human health threshold, then fit linear model by basin for **10 year period**. (Tred Check: Does the trend value need to be rescaled to between -1 and 1? Does not exceed now but need to consider if method broadly works?)

<!-- **`BHI2.0` discussions with Anna Sobek ** -->

------------------------------------------------------------------------

<br>

### 3.2 PFOS Indicator

#### 3.2.1 Match BHI Regions

**Use Lat/Long to Match BHI Regions**

``` r
# use 'join_rgns_info' helper function defined in spatial.R
pfos <- join_rgns_info(
  read_clean_df("pfos_bio_cleaned.csv"),
  helcomID_col = "HELCOM_ID",
  latlon_vars = c("latitude", "longitude"),
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefiles"),
  return_spatial = FALSE,
  buffer_shp = buffer_sf
)
```

#### 3.2.2 Filter Organofluorines Data keeping only PFOS in Clupea harengus

Only herring (Clupea harengus) are used from the biota datasets, as they are fairly equally spatially distributed across the baltic sea, while many other species are predominantly in the south. Also, heerring are . In the raw organofluorine dataset, matrix analyzed for most measurements was liver but there were some also where muscle was analyzed. In the initial data cleaning (see `bhi-prep/data/CW/contaminants/v2019/con_data.rmd`), the concentrations from liver measurements were converted to muscle equivalent using the report: [Distribution of PFAS in liver and muscle of herring, perch, cod, eelpout, arctic char, and pike from limnic and marine environments in Sweden. Faxneld et al 2014](https://www.diva-portal.org/smash/get/diva2:767385/FULLTEXT01.pdf).

``` r
pfos <- pfos %>% 
  filter(!is.na(value_pfos_muscle_equiv_wet_wgt)) %>% 
  filter(str_detect(species, pattern = "Clupea harengus")) %>%
  filter(variable == "PFOS") %>% 
  select(
    species, num_indiv_subsample, monit_program, monit_purpose, monit_year,
    sub_samp_ref,
    qflagged, qflag, 
    country, Subbasin, HELCOM_ID, BHI_ID, latitude, longitude, station,
    date, year, month, day,
    variable, 
    ## rename variables so functions defined earlier in the script can be applied
    value_wet_wgt = value_pfos_muscle_equiv_wet_wgt, 
    detect_lim_wet_wgt = detect_lim_pfos_muscle_equiv_wet_wgt, 
    quant_lim_wet_wgt = quant_lim_pfos_muscle_equiv_wet_wgt
  )
```

#### 3.2.3 Evaluate Flagged Data & Sampling Patterns

##### Visualize PFOS flagged data in Timeseries

PFOS data plotted by subbasin. Colorscale in plot one corresponds to number of observations, with lighter blues/greens indicating more observations for the given date-congener-subbasin group, while darker colors indicate fewer observations in the dataset for the given date-congener-subbasin combination. Plot 2 shows the same information in boxplot format, with data grouped by year.

###### Concentrations Timeseries and Boxplots with Observation Counts

``` r
## pcb qflag adjustment congeners by basin and congener, 
## red x's indicate flagged data, colorscale indicates number of observations
plotdf <- pfos %>% 
  mutate(value_adj = ifelse(qflagged, value_wet_wgt/2, value_wet_wgt)) %>% 
  mutate(value_adj_original = ifelse(qflagged, value_wet_wgt, NA)) %>% 
  group_by(date, variable, Subbasin) %>% 
  mutate(countObs = n(), meanAdjww = mean(value_adj, na.rm = TRUE)) %>% 
  ungroup()

ggplot(plotdf) + 
  scale_color_distiller(palette = "GnBu", direction = -1, limits = c(0, 15)) +
  geom_point(aes(date, value_adj, color = countObs), show.legend = FALSE, size = 1.5, alpha = 0.8) +
  ## no flagged values that needed adjustment as of BHI2.0
  # geom_point(aes(date, value_adj_original), color = "red", shape = 4, size = 1, alpha = 0.6) +
  facet_wrap(~ Subbasin, scales = "free_y", ncol = 3) +
  labs(x = NULL, y = "Congener Concentration (ug/kg wet weight, muscle equivalent) measured in Clupea harengus") +
  theme_bw()
```

``` r
boxplotdf <- plotdf %>%
  mutate(year = as.factor(year)) %>% 
  group_by(year, variable, Subbasin) %>% 
  mutate(countObs = sum(countObs)) %>% 
  select(year, date, variable, Subbasin, meanAdjww, countObs) %>% 
  distinct()

ggplot(boxplotdf) + 
  scale_color_distiller(palette = "GnBu", direction = -1, limits = c(0, 155)) +
  scale_fill_distiller(palette = "GnBu", direction = -1, limits = c(0, 155)) +
  geom_boxplot(
    aes(year, meanAdjww, color = countObs, fill = countObs), 
    show.legend = FALSE, 
    alpha = 0.7, 
    outlier.size = 0.4
  ) + 
  facet_wrap(~ Subbasin, ncol = 3) +
  scale_x_discrete(breaks = c(2005, 2010, 2015)) +
  labs(
    x = NULL, 
    y = "Congener Daily Mean Concentrations (ug/kg wet weight muscle equivalent) measured in Clupea harengus"
  ) +
  theme_bw()
```

------------------------------------------------------------------------

<br>

##### Spatial distributions of PFOS sampling Locations, Years 2014-2018

``` r
pfos_qflag_adjust <- qflag_adjust(pfos, indicator = "pfos", sumcongeners = FALSE)
pfos_qflag_adjust_datemeans <- pfos_qflag_adjust %>% 
  group_by(date, day, month, year, latitude, longitude, country, Subbasin, HELCOM_ID, BHI_ID) %>% 
  summarize(sum_congeners_datemean = mean(PFOS, na.rm = TRUE)) %>%
  mutate(Matrix = "bio") %>% 
  ungroup()

pfos_noqflag <- qflag_adjust(pfos, indicator = "pfos", sumcongeners = FALSE, approach = "NoQflag")
pfos_noqflag_datemeans <- pfos_noqflag %>% 
  group_by(date, day, month, year, latitude, longitude, country, Subbasin, HELCOM_ID, BHI_ID) %>% 
  summarize(sum_congeners_datemean = mean(PFOS, na.rm = TRUE)) %>% 
  mutate(Matrix = "bio") %>% 
  ungroup()

pfos2008to2019 <- map_cwcon_initial(pfos_qflag_adjust_datemeans, yr = 2014:2018, c(0, 15))
gridExtra::grid.arrange(pfos2008to2019$map, pfos2008to2019$basinplot, nrow = 1, widths = c(1.2, 1))
```

<img src="con_prep_files/figure-markdown_github/pfos map of data records-1.png" width="120%" />

------------------------------------------------------------------------

<br>

#### 3.2.4 Status and Trend Options

**Most-recent Years in Data Layers for each Approach to Flagged Data**

``` r
htmlTable::htmlTable(
  full_join(
    pfos_qflag_adjust_datemeans %>%
      arrange(year) %>%
      group_by(Subbasin) %>%
      summarise(lastyr_Adjusted = last(year)) %>% 
      filter(!is.na(Subbasin)),
    pfos_noqflag_datemeans %>%
      arrange(year) %>%
      group_by(Subbasin) %>%
      summarise(lastyr_NoQflag = last(year)) %>% 
      filter(!is.na(Subbasin)),
    by = "Subbasin"
  ) %>% select(lastyr_Adjusted, lastyr_NoQflag, Subbasin),
  align = "rcl", 
  rnames = FALSE,
  css.cell = c(
    rep("padding-left: 5em; padding-right: 2em", 2),
    "padding-left: 14em; padding-right: 9em"
  ),
  col.rgroup = c("none", "#F7F7F7")
)
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: right;">
lastyr\_Adjusted
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
lastyr\_NoQflag
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;">
Subbasin
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2014
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2014
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Aland Sea
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2014
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2014
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Arkona Basin
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Bornholm Basin
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Bothnian Bay
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Bothnian Sea
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Eastern Gotland Basin
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2018
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2018
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Gulf of Finland
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2014
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2014
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
Kattegat
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; text-align: right;">
2017
</td>
<td style="padding-left: 5em; padding-right: 2em; text-align: center;">
2017
</td>
<td style="padding-left: 14em; padding-right: 9em; text-align: left;">
Northern Baltic Proper
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: right;">
2016
</td>
<td style="padding-left: 5em; padding-right: 2em; background-color: #f7f7f7; text-align: center;">
2016
</td>
<td style="padding-left: 14em; padding-right: 9em; background-color: #f7f7f7; text-align: left;">
The Quark
</td>
</tr>
<tr>
<td style="padding-left: 5em; padding-right: 2em; border-bottom: 2px solid grey; text-align: right;">
2014
</td>
<td style="padding-left: 5em; padding-right: 2em; border-bottom: 2px solid grey; text-align: center;">
2014
</td>
<td style="padding-left: 14em; padding-right: 9em; border-bottom: 2px solid grey; text-align: left;">
Western Gotland Basin
</td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

<br>

**Compare two approaches to deal with flagged data, for BHI regions vs Basin**

1.  Start with mean concentrations by date and location
2.  Scale observations compared to the relevant contaminant-and-matrix-relevant reference point, capping at one
3.  Calculate status as mean of all scaled obs. for past 5 years in either BHI region or basin
4.  Compare by-basin and by-BHI region results for status and for trend, assess number of data points contributing

``` r
## using function defined above, investigate:
## (1) adjusting vs excluding flagged data, and 
## (2) aggregating data by subbasin versus BHI regions in calculating status and trend
pfos_qflag_adjust_indicators <- cwcon_indicators(pfos_qflag_adjust_datemeans, yrs = 2014:2018, bio_thresh = 9.1)
pfos_noqflag_indicators <- cwcon_indicators(pfos_noqflag_datemeans, yrs = 2014:2018, bio_thresh = 9.1)

pfos_status <- bind_rows(
  pfos_qflag_adjust_indicators$basin_status %>% 
    mutate(approach = "Adjusted", spatialunits = "Subbasins"),
  pfos_qflag_adjust_indicators$bhirgn_status %>% 
    mutate(approach = "Adjusted", spatialunits = "BHI Regions"),
  pfos_noqflag_indicators$basin_status %>% 
    mutate(approach = "NoQflag", spatialunits = "Subbasins"),
  pfos_noqflag_indicators$bhirgn_status %>% 
    mutate(approach = "NoQflag", spatialunits = "BHI Regions")
)
pfos_status <- pfos_status %>%
  mutate(Region = paste(Subbasin, rgn_nam, sep = ", ")) %>% 
  mutate(approach_sp_unit = paste(spatialunits, approach))

pfos_status$spatialunits <- factor(pfos_status$spatialunits, levels = c("Subbasins", "BHI Regions"))

ggplot(pfos_status, aes(Region, status, fill = Subbasin)) +
  geom_col(position = position_dodge(), color = "grey", alpha = 0.7, show.legend = FALSE) + 
  scale_fill_manual(values = statuspal) +
  facet_grid(rows = vars(approach), cols = vars(spatialunits)) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 7))
```

![](con_prep_files/figure-markdown_github/investigate%20approaches%20to%20qflags%20and%20summarizing%20by%20region%20vs%20basin%20pfos%20indicator-1.png)

<br>

#### 3.1.5 Save PFOS contaminants layer and intermediate datasets

``` r
write_csv(
  pfos_qflag_adjust_datemeans %>% 
    select(
      region_id = BHI_ID,
      latitude, longitude, 
      year,
      value = sum_congeners_datemean, 
      matrix = Matrix
    ), 
  file.path(dir_layers, sprintf("cw_con_pfos_bhi%s.csv", assess_year))
)
```

<br>

------------------------------------------------------------------------

#### 3.1.6 Methods discussion

**Status formula, ICES6 PCBs Biota and 7PCBs in Sediment**

**Conclusions & Decisions regarding Status and Trend Options**

**Trend Considerations**

------------------------------------------------------------------------

<br>

### 3.3 Dioxin Indicator

#### 3.3.1 Match BHI Regions

Dioxins and dioxin-like PCBs datasets are manipulated as separate objects for the next steps, combined only in the last code chunk before section 3.3.3 'Status and Trend Options'. Sediment and biota datasets for each dioxins and dioxin-like PCBs are joined before spatial plotting.

``` r
## use 'join_rgns_info' helper function defined in spatial.R
dioxin_bio <- join_rgns_info(
  read_clean_df("dioxin_bio_cleaned.csv"),
  latlon_vars = c("latitude", "longitude"),
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefiles"),
  return_spatial = FALSE,
  buffer_shp = buffer_sf
)
dioxin_sed <- join_rgns_info(
  read_clean_df("dioxin_sed_cleaned.csv"),
  latlon_vars = c("latitude", "longitude"),
  rgn_shps_loc = file.path(dirname(dir_B), "Shapefiles"),
  return_spatial = FALSE,
  buffer_shp = buffer_sf
)
```

<br>

#### 3.3.2 Filter Dioxin-like PCBs to join with Dioxins dataset, and Convert all to Toxic Equivalents

To convert dioxin and dioxin-like PCB congener concentrations to toxic equivalents we use the [IPCS Reference Table](https://www.who.int/ipcs/assessment/tef_values.pdf), taken from The 2005 World Health Organization Re-evaluation of Human and Mammalian Toxic Equivalency Factors for Dioxins and Dioxin-like Compounds (Van den Berg et al, 2005).

``` r
tef_pdf <- pdftools::pdf_text("https://www.who.int/ipcs/assessment/tef_values.pdf")[1] %>%
  stringr::str_split(pattern = "\n")
tef_lookup <- as.data.frame(tef_pdf[[1]][2:34])
colnames(tef_lookup) <- "singlecolumn"
tidyr::separate(tef_lookup, singlecolumn, c("WHO_compound_name", "TEF_1998", "TEF_2005"), "\\s\\s+")

## manually matched with ICES congeners names...
## congeners in the tablee without a TEF value are not dioxin-like
lookup_tef <- read_csv(here::here("supplement", "lookup_tabs", "tef_conversion_lookup.csv"))
```

**Filter PCB to keep only dioxin-like PCBs, convert units, and convert to TEQ**

``` r
## get dioxin-like PCBs by joining TEF lookup table
## filter out congeners without a TEF conversion
## convert from micrograms/kilogram to picograms/gram and calculate TEQs
## ug/kg equivalent to 1000 pg/g
dioxinlike_pcbbio_teq <- all_pcb_bio %>%
  inner_join(lookup_tef, by = c("variable" = "congener")) %>%
  filter(!is.na(TEF_2005)) %>%
  mutate(value_teq = 1000*value_wet_wgt*TEF_2005) %>% 
  select(-congener_full, -WHO_compound_name, -congener_category, -TEF_2005) %>% 
  select(-ends_with("wet_wgt"))

dioxinlike_pcbsed_teq <- all_pcb_sed %>%
  inner_join(lookup_tef, by = c("variable" = "congener")) %>%
  filter(!is.na(TEF_2005)) %>%
  mutate(value_teq = 1000*value_dry_wgt*TEF_2005) %>% 
  select(-congener_full, -WHO_compound_name, -congener_category, -TEF_2005) %>% 
  select(-ends_with("dry_wgt"))
```

**Use TEF conversion factor to convert Dioxin concentrations to TEQ**

``` r
## subset dioxin datasets
dioxin_bio <- dioxin_bio %>%
  filter(!is.na(value_wet_wgt)) %>% 
  filter(str_detect(species, pattern = "Clupea harengus")) %>%
  ## check that rows are distinct: nrow(dioxin_bio) == nrow(distinct(dioxin_bio))...
  select(
    variable, value_wet_wgt, detect_lim_wet_wgt, quant_lim_wet_wgt,
    species, num_indiv_subsample, monit_program, monit_purpose, monit_year,
    sub_samp_ref,
    qflagged, qflag, 
    country, Subbasin, HELCOM_ID, BHI_ID, latitude, longitude, station,
    date, year, month, day
  ) 
dioxin_sed <- dioxin_sed %>%
  filter(!is.na(value_dry_wgt)) %>%
  ## check that rows are distinct: nrow(dioxin_sed) == nrow(distinct(dioxin_sed))...
  select(
    variable, value_dry_wgt, detect_lim_dry_wgt, quant_lim_dry_wgt,
    num_indiv_subsample, monit_program, monit_purpose, monit_year,
    samp_ref,
    qflagged, qflag, 
    country, Subbasin, HELCOM_ID, BHI_ID, latitude, longitude, station,
    date, year, month, day
  ) %>% 
  filter(!is.na(BHI_ID)|!is.na(Subbasin))

## filter out congeners without a TEF conversion
## convert from micrograms/kilogram to picograms/gram and calculate TEQs
dioxinbio_teq <- dioxin_bio %>%
  inner_join(lookup_tef, by = c("variable" = "congener")) %>%
  filter(!is.na(TEF_2005)) %>%
  mutate(value_teq = 1000*value_wet_wgt*TEF_2005) %>% 
  select(-congener_full, -WHO_compound_name, -congener_category, -TEF_2005) %>% 
  select(-ends_with("wet_wgt"))

dioxinsed_teq <- dioxin_sed %>%
  inner_join(lookup_tef, by = c("variable" = "congener")) %>%
  filter(!is.na(TEF_2005)) %>%
  mutate(value_teq = 1000*value_dry_wgt*TEF_2005) %>% 
  select(-congener_full, -WHO_compound_name, -congener_category, -TEF_2005) %>% 
  select(-ends_with("dry_wgt"))
```

##### Checking Outliers/Questionable Measurements {.tabset .tabset-fade .tabset-pills}

###### Dioxin-like PCBs in Biota

``` r
## something funky with some stations are these outliers or incorrectly recorded data??
dioxinlike_pcbbio_teq <- dioxinlike_pcbbio_teq %>% 
  mutate(
    ## points in 2014 in poland highlighted in BHI1.0 as questionable...
    ## for some reason it is the nonflagged data that are the problem?
    chk0 = (country == "Poland" & year == 2014 & !qflagged),
    ## additional points to investigate...
    chk1 = (station == "473" & year == 2015 & month == 7),
    chk2 = (station %in% c("SJB1454000", "SJB1455000", "SJB1453000") & year == 2018 & month == 10),
    chk3 = (station == "SJB1453000" & year == 2017 & month == 10),
    chk = !chk0&!chk1&!chk2&!chk3
  )
plotdf <- mutate(dioxinlike_pcbbio_teq, value_adj = ifelse(qflagged, value_teq/2, value_teq)) 

gridExtra::grid.arrange(
  makeplot(plotdf) + 
    geom_point(data = filter(plotdf, !chk), aes(date, value_adj), shape = 1) +
    ggtitle("All Data"),
  makeplot(filter(plotdf, !chk1)) + 
    geom_point(data = filter(plotdf, !chk1&!chk), aes(date, value_adj), shape = 1) +
    ggtitle("W/O station 473 July2015"),
  makeplot(filter(plotdf, !chk1&!chk2)) + 
    geom_point(data = filter(plotdf, !chk1&!chk2&!chk), aes(date, value_adj), shape = 1) +
    ggtitle("W/O SJB1454000/5000/3000, Oct2018"),
  makeplot(filter(plotdf, chk)) +
    ggtitle("All questionable points removed"),
  ncol = 4
)
```

![](con_prep_files/figure-markdown_github/checking%20dioxinlike%20pcb%20bio%20data%20and%20outliers-1.png)

``` r
dioxinlike_pcbbio_teq <- filter(dioxinlike_pcbbio_teq, chk) %>% select(-starts_with("chk"))
```

------------------------------------------------------------------------

<br>

###### Dioxin-like PCBs in Sediment

``` r
dioxinlike_pcbsed_teq <- dioxinlike_pcbsed_teq %>% 
  mutate(
    chk0 = (station == "FYN 3" & samp_ref == 293730),
    chk1 = (country == "Estonia" & qflag == "<" & variable %in% c("CB157", "CB167", "CB169")),
    chk = !chk0&!chk1
  )
plotdf <- dioxinlike_pcbsed_teq %>% 
  mutate(value_adj = ifelse(qflagged, value_teq/2, value_teq), country = as.factor(country))
plotdf$country <- factor(plotdf$country, levels = unique(plotdf$country)) # estonia last for matching palettes!

gridExtra::grid.arrange(
  makeplot(plotdf) +
    geom_point(aes(date, value_adj, color = country), alpha = 0.5, size = 0.5, show.legend = FALSE) +
    scale_color_manual(values = c(rep("grey", 7), "coral")) +
    geom_point(data = filter(plotdf, !chk), aes(date, value_adj), shape = 1) +
    ggtitle("All Data"),
  makeplot(filter(plotdf, !chk0)) + 
    geom_point(data = filter(plotdf, !chk0&!chk), aes(date, value_adj), shape = 1) +
    geom_point(aes(date, value_adj, color = country), alpha = 0.5, size = 0.5, show.legend = FALSE) +
    scale_color_manual(values = c(rep("grey", 7), "coral")) +
    ggtitle("W/O station FYN 3 sample #293730 Oct2000"),
  makeplot(filter(plotdf, chk)) + 
    ggtitle("All questionable points removed"),
  ncol = 3
)
```

![](con_prep_files/figure-markdown_github/checking%20dioxinlike%20pcb%20sed%20data%20and%20outliers-1.png)

``` r
dioxinlike_pcbsed_teq <- filter(dioxinlike_pcbsed_teq,chk) %>% select(-starts_with("chk"))
```

------------------------------------------------------------------------

<br>

###### Dioxins in Biota

``` r
dioxinbio_teq <- dioxinbio_teq %>% 
  mutate(
    chk0 = (station == "473" & year == 2015 & month == 7),
    chk1 = (station %in% c("SJB1454000", "SJB1455000", "SJB1453000") & year == 2018 & month == 10),
    chk = !chk0&!chk1
    # chk2 = (station == "Utlängan" & year == 2014 & month == 6),
    # chk = !chk0&!chk1&!chk2
  )
plotdf <- mutate(dioxinbio_teq, value_adj = ifelse(qflagged, value_teq/2, value_teq)) 

gridExtra::grid.arrange(
  makeplot(plotdf) + 
    geom_point(data = filter(plotdf, !chk), aes(date, value_adj), shape = 1) +
    ggtitle("All Data"),
  makeplot(filter(plotdf, !chk0)) + 
    geom_point(data = filter(plotdf, !chk0&!chk), aes(date, value_adj), shape = 1) +
    ggtitle("W/O station 473 July2015"),
  makeplot(filter(plotdf, !chk0&!chk1)) + 
    geom_point(data = filter(plotdf, !chk0&!chk1&!chk), aes(date, value_adj), shape = 1) +
    ggtitle("W/O SJB1454000/5000/3000, Oct2018"),
  makeplot(filter(plotdf, chk)) +
    ggtitle("All questionable points removed"),
  ncol = 4
)
```

![](con_prep_files/figure-markdown_github/checking%20dioxin%20biota%20data%20and%20outliers-1.png)

``` r
dioxinbio_teq <- filter(dioxinbio_teq, chk) %>% select(-starts_with("chk"))
```

<br>

#### 3.3.4 Status and Trend Options

``` r
htmlTable::htmlTable(
  full_join(
    dioxins_qflag_adjust_teqs_total %>%
      arrange(year) %>%
      group_by(Subbasin) %>%
      summarise(lastyr_Adjusted = last(year)),
    dioxins_noqflag_teqs_total %>%
      arrange(year) %>%
      group_by(Subbasin) %>%
      summarise(lastyr_NoQflag = last(year)),
    by = "Subbasin"
  ) %>% filter(!is.na(Subbasin)),
  align = "lcc", 
  rnames = FALSE,
  css.cell = c(
    rep("padding-left: 5.75em; padding-right: 1.5em", 2),
    "padding-left: 10em; padding-right: 1.5em"
  ),
  col.rgroup = c("none", "#F7F7F7")
)
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;">
Subbasin
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
lastyr\_Adjusted
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
lastyr\_NoQflag
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: left;">
Aland Sea
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: center;">
2017
</td>
<td style="padding-left: 10em; padding-right: 1.5em; text-align: center;">
2017
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: left;">
Arkona Basin
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2017
</td>
<td style="padding-left: 10em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2017
</td>
</tr>
<tr>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: left;">
Bay of Mecklenburg
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: center;">
2008
</td>
<td style="padding-left: 10em; padding-right: 1.5em; text-align: center;">
2008
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: left;">
Bornholm Basin
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2017
</td>
<td style="padding-left: 10em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2017
</td>
</tr>
<tr>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: left;">
Bothnian Bay
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: center;">
2018
</td>
<td style="padding-left: 10em; padding-right: 1.5em; text-align: center;">
2018
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: left;">
Bothnian Sea
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2018
</td>
<td style="padding-left: 10em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2018
</td>
</tr>
<tr>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: left;">
Eastern Gotland Basin
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: center;">
2016
</td>
<td style="padding-left: 10em; padding-right: 1.5em; text-align: center;">
2016
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: left;">
Great Belt
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2015
</td>
<td style="padding-left: 10em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2015
</td>
</tr>
<tr>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: left;">
Gulf of Finland
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: center;">
2018
</td>
<td style="padding-left: 10em; padding-right: 1.5em; text-align: center;">
2018
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: left;">
Gulf of Riga
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2018
</td>
<td style="padding-left: 10em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
</td>
</tr>
<tr>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: left;">
Kattegat
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: center;">
2017
</td>
<td style="padding-left: 10em; padding-right: 1.5em; text-align: center;">
2017
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: left;">
Kiel Bay
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2011
</td>
<td style="padding-left: 10em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2011
</td>
</tr>
<tr>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: left;">
Northern Baltic Proper
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: center;">
2017
</td>
<td style="padding-left: 10em; padding-right: 1.5em; text-align: center;">
2017
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: left;">
The Quark
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2017
</td>
<td style="padding-left: 10em; padding-right: 1.5em; background-color: #f7f7f7; text-align: center;">
2017
</td>
</tr>
<tr>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: left;">
The Sound
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; text-align: center;">
2015
</td>
<td style="padding-left: 10em; padding-right: 1.5em; text-align: center;">
2015
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; border-bottom: 2px solid grey; text-align: left;">
Western Gotland Basin
</td>
<td style="padding-left: 5.75em; padding-right: 1.5em; background-color: #f7f7f7; border-bottom: 2px solid grey; text-align: center;">
2017
</td>
<td style="padding-left: 10em; padding-right: 1.5em; background-color: #f7f7f7; border-bottom: 2px solid grey; text-align: center;">
2017
</td>
</tr>
</tbody>
</table>
<br>

**Compare two approaches to deal with flagged data, for BHI regions vs Basin**

1.  Start with total TEQs: congeners summed per sample, means by date and location, dioxin and dioxin-like PCB summes added together per unique date and location
2.  Take mean of all unique obs. for latest 5 years in either BHI region or basin
3.  Compare by-basin and by-BHI region results for status and for trend, assess number of data points contributing

``` r
## using function defined above, investigate:
## (1) adjusting vs excluding flagged data, and 
## (2) aggregating data by subbasin versus BHI regions in calculating status and trend
dioxin_qflag_adjust_indicators <- cwcon_indicators(
  dioxins_qflag_adjust_teqs_total %>% rename(sum_congeners_datemean = total_teq), 
  bio_thresh = 6.5, sed_thresh = 0.86, 
  yrs = 2014:2018
)
dioxin_noqflag_indicators <- cwcon_indicators(
  dioxins_noqflag_teqs_total %>% rename(sum_congeners_datemean = total_teq), 
  bio_thresh = 6.5, sed_thresh = 0.86, 
  yrs = 2014:2018
)
dioxin_status <- bind_rows(
  dioxin_qflag_adjust_indicators$basin_status %>% 
    mutate(approach = "Adjusted", spatialunits = "Subbasins"),
  dioxin_qflag_adjust_indicators$bhirgn_status %>% 
    mutate(approach = "Adjusted", spatialunits = "BHI Regions"),
  dioxin_noqflag_indicators$basin_status %>% 
    mutate(approach = "NoQflag", spatialunits = "Subbasins"),
  dioxin_noqflag_indicators$bhirgn_status %>% 
    mutate(approach = "NoQflag", spatialunits = "BHI Regions")
)
dioxin_status <- dioxin_status %>%
  mutate(Region = paste(Subbasin, rgn_nam, sep = ", ")) %>% 
  mutate(approach_sp_unit = paste(spatialunits, approach))

dioxin_status$spatialunits <- factor(dioxin_status$spatialunits, levels = c("Subbasins", "BHI Regions"))

ggplot(dioxin_status, aes(Region, status, fill = Subbasin)) +
  geom_col(position = position_dodge(), color = "grey", alpha = 0.7, show.legend = FALSE) + 
  scale_fill_manual(values = statuspal) +
  facet_grid(rows = vars(approach), cols = vars(spatialunits)) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 7))
```

![](con_prep_files/figure-markdown_github/investigate%20approaches%20to%20qflags%20and%20summarizing%20by%20region%20vs%20basin%20dioxin%20indicator-1.png)

<br>

#### 3.3.5 Save Dioxin contaminants layer and intermediate datasets

``` r
write_csv(
  dioxins_qflag_adjust_teqs_total %>% 
    select(
      region_id = BHI_ID,
      latitude, longitude, 
      year,
      value = total_teq, 
      matrix = Matrix
    ), 
  file.path(dir_layers, sprintf("cw_con_dioxin_bhi%s.csv", assess_year))
)
```

<br>

------------------------------------------------------------------------

#### 3.3.6 Methods discussion

**Status formula**

*X*<sub>*d**i**o**x**i**n**T**E**Q*</sub> = min{1,  teq threshold/mean dioxin-like pcb teq value}

**Conclusions & Decisions regarding Status and Trend Options**

**Trend Considerations**

------------------------------------------------------------------------

### 3.4 Concerning Substances Indicator

This indicator is a new addition since the `BHI1.0`, conceptualized as a more comprehensive way to account for the fact that many concerning, hazardous substances are not currently monitored in the Baltic Sea. In the earlier assessment, a penalty factor of 0.1 was applied to the entire Baltic to reduce the contaminant subgoal scores so they better reflected this concerning situation. This method uses [a list compiled by ECHA of concerning substances](https://echa.europa.eu/candidate-list-table?p_p_id=disslists_WAR_disslistsportlet&p_p_lifecycle=1&p_p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_pos=2&p_p_col_count=3&_disslists_WAR_disslistsportlet_javax.portlet.action=searchDissLists), and involves cross-referencing with those monitored substances in the [ICES contaminants databases for biota](http://dome.ices.dk/views/ContaminantsBiota.aspx) [and for sediments](http://dome.ices.dk/views/ContaminantsSediment.aspx). Those on the list which are monitored, are assessed for spatial coverage of monitoring; substances on the list may be monitored somewheree in the Baltic Sea, but only in some basins or for some countries. The final indicator score is the percentage of substances on the list that are monitored in the given region.

``` r
get_spatial_monitor_info <- function(substance_data_filepath, matrix = "bio", bhi_join_tab){
  
  if(matrix == "bio"){
    df <- readr::read_delim(substance_data_filepath, delim = ";")
  }  
  if(matrix == "sed"){
      df <- readr::read_delim(substance_data_filepath, delim = ";") %>% 
        filter(DEPHL <= 0.05)
  }
  countdf <- df %>%
    dplyr::select(
      country = Country, station = STATN, 
      year = MYEAR, ices_date = DATE, 
      lat = Latitude, lon = Longitude,
      param_group = PARGROUP, variable = PARAM, 
      measurement_ref = tblParamID,
      samp_ref = tblSampleID,
      sub_samp_id = SUBNO, bulk_id = BULKID,
      sub_samp_ref = matches("tblBioID"),
      press_depth = matches("DEPHU"), depth_low = matches("DEPHL")
    ) %>% 
    # filter(year %in% yrs) %>% 
    join_rgns_info(buffer_shp = buffer_sf) %>% 
    mutate(country = ifelse(
      !is.na(rgn_nam) & country != rgn_nam, 
      as.character(rgn_nam), 
      country
    )) %>% 
    filter(!is.na(BHI_ID)) %>% 
    group_by(year, param_group, variable, BHI_ID, HELCOM_ID, Subbasin, country) %>% 
    summarize(count_obs = n()) %>% 
    ungroup()
  
  countdfplot <- ggplot(countdf %>% mutate(BHI_ID = as.factor(BHI_ID))) + 
    geom_point(
        aes(year, BHI_ID, color = count_obs), 
        show.legend = FALSE,
        size = 3, 
        shape = 15, 
        alpha = 0.7
    ) + 
    facet_wrap(~country, nrow = 1, scales = "free_y") +
    labs(x = NULL, y = NULL) +
    scale_color_viridis_c()

  monitordf <- left_join(
    bhi_join_tab,
    countdf %>% 
      mutate(ICES_monitored = TRUE) %>% 
      ## should maybe filter, if fewer than a certain number of measurements in the region?
      # filter(count_obs > 2) %>%
      select(BHI_ID, HELCOM_ID, Subbasin, rgn_nam = country, year, ICES_monitored) %>%
      distinct()
    ) %>% mutate(ICES_monitored = ifelse(is.na(ICES_monitored), FALSE, ICES_monitored))
    
  return(list(monitored = monitordf, obscounts = countdfplot))
}
```

### 3.4.1 Regional monitoring of Concerning Substances

Of the concerning substances which are monitored, some are monitored only in some BHI regions and/or years. The code below is used in assessing this spatial coverage.

``` r
## read in master table of emerging contaminant
## this was compiled by Andrea and checked by Anna Sobeck for correctness/completeness
data_loc <- file.path(dir_B, "Goals", "CW", "CON")
subst_of_concern <- read_csv(file.path(data_loc, "concerning_substances_lookup.csv"))

## spatial regions table for joining
joindf <- BHI_rgns_shp %>% 
  st_drop_geometry() %>% 
  select(-Area_km2) %>% 
  mutate(
    Subbasin = as.character(Subbasin), 
    HELCOM_ID = as.character(HELCOM_ID),
    rgn_nam = as.character(rgn_nam), 
    rgn_key = as.character(rgn_key)
  ) %>% 
  mutate(year = list(1990:2020)) %>% 
  tidyr::unnest(year)

## loop through substances and determine which regions they are/not monitored in
rgn_monitor_bio_allsubst <- joindf
rgn_monitor_sed_allsubst <- joindf
plotlistbio <- list()
plotlistsed <- list()

for(nm in unique(filter(subst_of_concern, ICES_monitored)$filename)){
  
  bio_fp <- file.path(
    data_loc, 
    "ContaminantsBiota_ConcerningSubstances", 
    sprintf("ContaminantsBiota_%s.csv", nm)
  )
  sed_fp <- file.path(
    data_loc,
    "ContaminantsSediment_ConcerningSubstances", 
    sprintf("ContaminantsSediment_%s.csv", nm)
  )
  
  if(file.exists(bio_fp)){
    regionalmonitoringbio <- get_spatial_monitor_info(
      bio_fp,
      matrix = "bio",
      bhi_join_tab = joindf
    )
    colnames(regionalmonitoringbio$monitored) <- names(regionalmonitoringbio$monitored) %>%
      str_replace("ICES_monitored", paste(nm, "monitored", sep = "_"))

    rgn_monitor_bio_allsubst <- rgn_monitor_bio_allsubst %>%
      left_join(regionalmonitoringbio$monitored)
    
    plotlistbio[[nm]] <- regionalmonitoringbio$obscounts
  }
  if(file.exists(sed_fp)){
    regionalmonitoringsed <- get_spatial_monitor_info(
      sed_fp,
      matrix = "sed",
      bhi_join_tab = joindf
    )
    colnames(regionalmonitoringsed$monitored) <- names(regionalmonitoringsed$monitored) %>% 
      str_replace("ICES_monitored", paste(nm, "monitored", sep = "_"))
    
    rgn_monitor_sed_allsubst <- rgn_monitor_sed_allsubst %>% 
      left_join(regionalmonitoringsed$monitored)
    
    plotlistsed[[nm]] <- regionalmonitoringsed$obscounts
  }
}

## for the remaining substances of concern (not monitored at all) create columns
for(s in unique(filter(subst_of_concern, !ICES_monitored)$filename)){
  ## add to bio dataset
  dfbio <- mutate(rgn_monitor_bio_allsubst, addcolumn = FALSE)
  colnames(dfbio) <- c(names(rgn_monitor_bio_allsubst), paste0(s, "_monitored"))
  rgn_monitor_bio_allsubst <- dfbio
  ## add to sed dataset
  dfsed <- mutate(rgn_monitor_sed_allsubst, addcolumn = FALSE)
  colnames(dfsed) <- c(names(rgn_monitor_sed_allsubst), paste0(s, "_monitored"))
  rgn_monitor_sed_allsubst <- dfsed
}
## join and spread so can easily sum num. substances monitored
rgn_monitor_allsubst <- full_join(
  rgn_monitor_bio_allsubst %>%
    tidyr::pivot_longer(
      cols = ends_with("_monitored"), 
      names_to = "substance", 
      values_to = "bio_monitored"
    ),
  rgn_monitor_sed_allsubst %>%
    tidyr::pivot_longer(
      cols = ends_with("_monitored"), 
      names_to = "substance", 
      values_to = "sed_monitored"
    )
)
rgn_monitor_allsubst <- rgn_monitor_allsubst %>%
  mutate(monitored = bio_monitored|sed_monitored) %>%
  mutate(monitored = ifelse(is.na(monitored), FALSE, monitored)) %>%
  select(-bio_monitored, -sed_monitored) %>% 
  mutate(substance = str_remove(substance, "_monitored"))

# gridExtra::grid.arrange(grobs = plotlistbio, nrow = length(plotlistbio))
# gridExtra::grid.arrange(grobs = plotlistbio, nrow = length(plotlistsed))
```

``` r
## calculating emerging concerning substances indicator
concern_subst_indicator <- rgn_monitor_allsubst %>% 
  mutate(substance = paste0(substance, "_monitored")) %>% 
  tidyr::pivot_wider(names_from = "substance", values_from = "monitored")


num_substances <- length(grep("_monitored", colnames(concern_subst_indicator)))
concern_subst_ind_ts <- data.frame(
  Subbasin = character(), 
  BHI_ID = numeric(),
  proportion_monitored = numeric(),
  dimension = character(),
  year = numeric()
)

for(y in 2018:(min(concern_subst_indicator$year)+4)){
  yrs <- y:(y-4)
  
  concern_subst_ind_ts <- bind_rows(
    concern_subst_ind_ts,
    concern_subst_indicator %>%
      ## look at monitoring only within time period of interest
      filter(year %in% yrs) %>% 
      group_by(rgn_nam, rgn_key, Subbasin, HELCOM_ID, BHI_ID) %>% 
      summarise_at(vars(ends_with("_monitored")), sum) %>% 
      ungroup() %>% 
      mutate_at(vars(ends_with("_monitored")), funs(pmin(1, .))) %>% 
      ## add columns on number substances and number monitored
      mutate(
        num_substances = num_substances,
        num_monitored = rowSums(.[grep("_monitored", names(.))]),
        proportion_monitored = round(num_monitored/num_substances, 2)
      ) %>%
      select(Subbasin, BHI_ID, proportion_monitored) %>% 
      mutate(dimension = "status", year = y)
  )
}

concern_subst_indicator <- filter(concern_subst_ind_ts, year %in% 2014:2018)
```

### 3.4.2 Save concerning substances layer and intermediate datasets

The data layer for the concerning substances indicator contains true/false records per BHI region per year for each of the 35 distinct substances on the [European Chemical Agency Candidate List of substances of very high concern for Authorisation](https://echa.europa.eu/candidate-list-table?p_p_id=disslists_WAR_disslistsportlet&p_p_lifecycle=1&p_p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_pos=2&p_p_col_count=3&_disslists_WAR_disslistsportlet_javax.portlet.action=searchDissLists).

``` r
write_csv(
  rgn_monitor_allsubst %>% 
    select(
      region_id = BHI_ID,
      latitude, longitude, 
      year,
      monitored, 
      substance
    ), 
  file.path(dir_layers, sprintf("cw_con_penalty_bhi%s.csv", assess_year))
)
```

------------------------------------------------------------------------

4. Visualizing Contaminants Data Layers
---------------------------------------

``` r
## join PCB, Dioxin and PFOS indicators, and take average
cw_con <- do.call(rbind, lapply(
  list(
    list(df = pcb_qflag_adjust_indicators$basin_status, dim = "status", ind = "pcb"),
    list(df = pcb_qflag_adjust_indicators$basin_trend, dim = "trend", ind = "pcb"),
    list(df = pfos_qflag_adjust_indicators$basin_status, dim = "status", ind = "pfos"),
    list(df = pfos_qflag_adjust_indicators$basin_trend, dim = "trend", ind = "pfos"),
    list(df = dioxin_qflag_adjust_indicators$basin_status, dim = "status", ind = "dioxin"),
    list(df = dioxin_qflag_adjust_indicators$basin_trend, dim = "trend", ind = "dioxin")
  ), 
  function(x){
    joindf <- x$df %>% 
      ungroup() %>% 
      select(Subbasin, BHI_ID, rgn_nam, !!!syms(x$dim)) %>% 
      mutate(dimension = x$dim, indicator = x$ind)
    colnames(joindf) <- c("Subbasin", "BHI_ID", "rgn_nam", "score", "dimension", "indicator")
    
    return(joindf)
  }
)) 
cw_con_w_penalty <- cw_con %>%   
  dplyr::group_by(BHI_ID, dimension) %>%
  dplyr::mutate(num_indicators = n()) %>%
  ## in first assessment, excluded where only had pcb indicator
  ## have all 3 here though for each case
  # dplyr::filter(num_indicators > 1) %>%
  dplyr::summarise(score = ifelse(
    sum(is.na(score)) == n(), NA,
    mean(score, na.rm = TRUE)
  )) %>%
  dplyr::mutate(score = round(score, 3)) %>%
  ungroup() %>% 
  left_join(concern_subst_indicator, by = c("BHI_ID", "dimension")) %>% 
  mutate(score = ifelse(dimension == "trend", score, score*proportion_monitored))
```

### 4.1 Contaminants data layers Maps & Subbasin Trends

Maps on the left show each indicator's status individually, and the larger map on the right shows the combined status score with PCB, PFOS, and Dioxin indicators averaged and penalized by the Concerning Substances indicator (monitored proportion).

``` r
mapcols <- c("indianred", "coral", "goldenrod1", "khaki", "lightblue", "steelblue")

mapdf <- bind_rows(
  cw_con %>% 
    select(-rgn_nam) %>% 
    filter(dimension == "status"),
  
  cw_con_w_penalty %>% 
    filter(dimension == "status") %>% 
    tidyr::pivot_longer(
      cols = c("proportion_monitored", "score"),
      names_to = "indicator",
      values_to = "score"
    )
)
mapsf <- left_join(bhi_rgns_simple, mapdf, by = c("Subbasin", "BHI_ID"))

indicatormaps <- basemap + 
  geom_sf(
    data = filter(mapsf, indicator != "score"), aes(fill = score), 
    size = 0.1, alpha = 0.85, show.legend = FALSE
  ) +
  facet_wrap(~ indicator) +
  scale_fill_gradientn(colors = mapcols, limits = c(0, 1), na.value = "gainsboro")

statusmap <- basemap + 
  geom_sf(data = filter(mapsf, indicator == "score"), aes(fill = score), size = 0.1, alpha = 0.85) +
  facet_wrap(~ indicator) +
  scale_fill_gradientn(colors = mapcols, limits = c(0, 1), na.value = "gainsboro") +
  theme(
    legend.background = element_rect(color = "grey"),
    legend.position = c(0.15, 0.75)
  ) +
  labs(fill = "Score")

trendsdf <- left_join(
  cw_con %>% 
    select(-rgn_nam) %>% 
    filter(dimension == "trend") %>% 
    tidyr::pivot_wider(names_from = indicator, values_from = score),
  cw_con_w_penalty %>% 
    filter(dimension == "trend") %>% 
    select(-proportion_monitored, -Subbasin),
  by = c("BHI_ID", "dimension")
)
trendsdf <- trendsdf %>% 
  tidyr::pivot_longer(
    cols = c("pcb", "pfos", "dioxin", "score"), 
    names_to = "indicator",
    values_to = "score"
  ) %>% 
  left_join(
    read_csv(here::here("supplement", "lookup_tabs", "rgns_complete.csv")) %>% 
      select(Subbasin = subbasin, subbasin_order, BHI_ID = region_id, subbasin_order),
    by = c("Subbasin", "BHI_ID")
  ) %>% 
  mutate(
    score_na = ifelse(is.na(score), 1, score),
    indicator = ifelse(indicator == "score", "average_trend", indicator)
  ) %>% 
  arrange(desc(subbasin_order))

trendsdf$Subbasin <- factor(
  trendsdf$Subbasin,
  levels = unique(trendsdf$Subbasin)
)

trendplots <- ggplot(trendsdf, aes(Subbasin, score_na, fill = score)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "grey") +
  scale_fill_gradientn(colors = rev(mapcols), limits = c(-1, 1), na.value = "gainsboro") +
  facet_wrap(~ indicator, nrow = 1) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  guides() +
  theme_bw()
```

``` r
laymat <- rbind(
  c(1,1,2,2), c(1,1,2,2),  c(1,1,2,2), c(1,1,2,2),
  c(1,1,2,2), c(1,1,2,2), c(3,3,3,3), c(3,3,3,3), c(3,3,3,3)
)
gridExtra::grid.arrange(
  indicatormaps, statusmap, trendplots, 
  layout_matrix = laymat
)
```

![](con_prep_files/figure-markdown_github/visualize%20con%20data%20layers%20and%20trends-1.png)

<br>

5. Considerations for `BHI3.0`
------------------------------

From the ICES Organofluorines dataset, include PFAS in addition to PFOS. To include PFAS, a suitable reference point will need to be found.

<br>

6. References
-------------

[Faxneld et al. 2014a](http://www.diva-portal.org/smash/record.jsf?pid=diva2%3A728508&dswid=1554) Biological effects and environmental contaminants in herring and Baltic Sea top predators

[Faxneld et al. 2014b](https://www.diva-portal.org/smash/get/diva2:767385/FULLTEXT01.pdf) Distribution of PFAS in liver and muscle of herring, perch, cod, eelpout, arctic char, and pike from limnic and marine environments in Sweden.

[Bignert, A., Nyberg, E., Sundqvist, K.L., Wiberg, K., 2007.](http://pubs.rsc.org/en/Content/ArticleLanding/2007/EM/b700667e#!divAbstract) Spatial variation in concentrations and patterns of the PCDD/F and dioxin-like PCB content in herring from the northern Baltic Sea. J. Environ. Monit. 9, 550–556.

[Working Group on the State of the Environment and Nature Conservation](http://helcom.fi/helcom-at-work/groups/state-and-conservation)

[Commission Regulation (EU) No. 1259/2011 of 2 December 2011.](http://eur-lex.europa.eu/LexUriServ/LexUriServ.do?uri=OJ:L:2011:320:0018:0023:EN:PDF) Amending Regulation (EC) No 1881/2006 as regards maximum levels for dioxins, dioxin-like PCBs and non dioxin-like PCBs in foodstuffs.

[Van den Berg et al, World Health Organization 2005.](http://www.who.int/ipcs/assessment/tef_values.pdf) The 2005 World Health Organization Re-evaluation of Human and Mammalian Toxic Equivalency Factors for Dioxins and Dioxin-like Compounds.

[HELCOM PFOS core indicator document](http://www.helcom.fi/Core%20Indicators/PFOS_HELCOM%20core%20indicator%202016_web%20version.pdf)

[Norwegian environment agency, PCBs and Dioxins indicator in sediment](https://www.miljodirektoratet.no/globalassets/publikasjoner/m1132/m1132.pdf)

[Norwegian Environmental quality classification of water bodies](http://www.vannportalen.no/globalassets/nasjonalt/dokumenter/veiledere-direktoratsgruppa/Klassifisering-av-miljotilstand-i-vann-02-2018.pdf)

<br>
