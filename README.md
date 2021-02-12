### Data prep for the BHI assessment

<br>

This repository contains scripts and figures from the preliminary wrangling of data for the BHI assessments.


---

**Calculating the Index: [Methods and Code](https://github.com/OHI-Baltic/bhi)**   
**More about the Baltic Health Index: [Assessment Results and Discussion](https://baltic-ohi.shinyapps.io/dashboard/)**   
**About the Ocean Health Index, parent project of the BHI: [Ocean Health Index Science](http://ohi-science.org)**

---

**`prep`**  

Contains subfolders for each goal/subgoal, and for pressure and resilience dimensions. Within those subfolders is a folder for each BHI assessment year (assessment year is the current year, and uses most-recent data available which may be from a prior year). These subfolders of the `prep` folder contain documents with the entire procedure of converting, combining, rescaling, or otherwise deriving BHI input data layers and some intermediate datasets.

**`layers`**  

Contains data prepared, quality-checked, and ready to be used in calculating the BHI goal scores and aggregated index. All prepared layers are saved to this folder, potentially including multiple versions to be used in different analyses e.g. in evaluating the effect of using different reference points, or in testing scenarios. These layers are copied into the `bhi` repo where index calculations or scenarios analyses are conducted.

**`R`**  

A folder containing some functions used throughout the data prep process, and to keep the repo organized.

**`supplement`**  

Summary or background information on the BHI goals (the meaning or philosophy the goals, and their data sources) are documented in the `goal_summaries` subfolder.The docs in `layer_summaries` constitute metadata for the data layers saved to the repo's `layers` folder.  General reference/lookup tables are stored in `lookup_tabs`. 

<br>

---
