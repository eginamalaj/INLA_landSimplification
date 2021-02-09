# Using a Bayesian Framework to explain changes in insecticide use in Canada over 20 years

This file contains R code and data to run the analysis for the article:

**Malaj, E.**, and Morrissey CA. (2021). Increased reliance on insecticide in Canada linked to simplified agricultural landscapes *In Review in Ecological Applications* 

## Depends

To run this analysis files are require:  
1. Main raw data: `findatCD.RData` in folder data. **[CleanCensusDat](https://github.com/eginamalaj/CleanCensusDat)** repo explains the data clean used;
2. Spatial data #1: `camapF2.RData` in folder data used for the fortified Canada polygon file from code `Canada_map_plot.R` in folder supportCode;
3. Spatial data #2: `2016CD_ag.shp` in repo **[explore_agrochemicals](https://github.com/eginamalaj/explore_agrochemicals/tree/main/CDshapefile)**;
4. Support functions: three files in folder supportCode that help to run the main analysis in code `INLA_models.R`;
5. `INLA_models.R` the main R code to run the analysis and produce figures and tables.

R version 4.0.3

Packages: tidyverse, doBy, INLA, rgdal, spdep, egg, cowplot, plyr, cowplot, glmmTMB, DHARMa, emmeans, MCMCglmm, coda, ape