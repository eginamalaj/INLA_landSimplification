# Using a Bayesian Framework to explain changes in insecticide use in Canada over 20 years

This file contains R code and data to run the analysis for the article:

**Malaj, E.**, and Morrissey CA. (2021). Increased reliance on insecticide applications in Canada linked to simplified agricultural landscapes *in Review at Ecological Applications* 

## Depends

To run this analysis files are require:  
1. Main raw data: `findatCD.RData` in folder data. **[CleanCensusDat](https://github.com/eginamalaj/CleanCensusDat)** repo explains the data clean used;
2. Spatial Data #1: `camapF2.RData` is a fortified Canada polygon file produced by running the code `Canada_map_plot.R` in folder supportCode. Run that file, save it as `camapF2.RData` and then continue with the points below;
3. Spatial Data #2: download `2016CD_ag.shp` from repo **[explore_agrochemicals](https://github.com/eginamalaj/explore_agrochemicals/tree/main/CDshapefile)**;
4. Spatial Data #3: `pud_insecticides.RData` is a spatial distribution of insecticide use density as produced in Malaj et al. (2020): https://doi.org/10.1016/j.scitotenv.2019.134765. Data is produced in a similar way as the repo **[SpatialAnalysis_Mapping](https://github.com/eginamalaj/SpatialAnalysis_Mapping)**
5. Land Use Data: The csv files `aafc_crop_classifications.csv`, `landuseCDCensus2010.csv`, `landuseCDCensus2015.csv` and `SemiNatural.csv` all used in `correlationsIndex.R`.
6. Code #1: `INLA_models.R` the main R code to run the analysis and produce figures and tables for the Bayesian models
7. Code #2: `correlationsIndex.R` R code to run the formatting, extraction and plotting of the correlation between proportion of cropland and proportion of semi-natural habitats, and linear relationship between insecticide area (ha) and insecticide mass (kg).
8. Code #3: Java code to produce `SemiNatural.csv` data which was used in code `correlationsIndex.R`
9. Support functions: three R code files in folder supportCode to help run the main analysis in code `INLA_models.R`.

## Results

Folder `figures` contains all figures for this analyis.

R version 4.0.3

Packages: tidyverse, doBy, INLA, rgdal, raster, spdep, egg, cowplot, plyr, glmmTMB, DHARMa, emmeans, MCMCglmm, coda, ape
