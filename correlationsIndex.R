#
# ------------------------------------------------------------------------------------------------------
#
# Code for paper:
# Malaj & Morrissey (2021) Increased reliance on insecticide in Canada linked to simplified agricultural landscapes
# Ecological Applications
# Contact: eginamalaj@gmail.com
#
# ------------------------------------------------------------------------------------------------------
#
#
##### Index Correlations 
#
# Examine the index relationship between to verify if landscape simplification and insecticide applications 
# are good indicators.
#
options(scipen = 999) # not to have scientific numbers
#
rm(list = ls())
require(tidyverse)
require(raster)
require(rgdal)
#
prj<-"Your Working Directory"
setwd("Your Working Directory")
#
## Create Subfolders. Subfolder paths
datadir <- file.path(prj, "data")# data
figdir <- file.path(prj, 'figures')# figures
#
#
# ------------------------------------------------------------------------------------------------------
#
# Import files
#
# ------------------------------------------------------------------------------------------------------
#
# 1. Cropland classification - AAFC
crclass<-read.csv(file.path(datadir, "aafc_crop_classifications.csv"),sep=",", header=T)
crclass$Code<- as.character(crclass$Code)
#
# 2. Data from Google Earth Engine calculating area (km2) for each crop in 2015
# See Java file for reproducible code
ls2010<-read.csv(file.path(datadir, "landuseCDCensus2010.csv"),sep=",", header=T)
ls2015<-read.csv(file.path(datadir, "landuseCDCensus2015.csv"),sep=",", header=T)
#
# 3. Insecticide Mass Data
load(file="pud_insecticide.RData")# i_pest_c
#
# 4. Shapefile
cd<- readOGR("2016CD_ag.shp","2016CD_ag")# lat-long
#
# 5. Census of Ag Data
load("findatCD.RData") # call:findatCD - census of ag data
#
# ------------------------------------------------------------------------------------------------------
#
# Semi Natural
#
# ------------------------------------------------------------------------------------------------------
#
# proportion of cropland (landscape simplification) vs. semi-natural areas calculated from AAFC - Annual Crop Production 
#
perLU10<-ls2010 %>%
  dplyr::select (-c(X10,X20,X30,X34,X35,X130))%>% # remove all notRelevant (see crclass list for more) before taking sum to account only for ag areas
  rowwise() %>%
  dplyr::mutate(sum_area_km2 = sum(c_across(2:62), na.rm = TRUE))%>% # sum all agricultural crops
  gather(2:62, key=Code, value=area_km2)%>% 
  mutate(Code = gsub("X", "", Code),
         year = "2011", # year that will match the census survey reporting
         percLU = area_km2/sum_area_km2)%>% # calculate percent land use
  drop_na(area_km2)%>% # remove all NAs - no value so the crop is not there
  left_join(crclass[-3], by="Code")%>%
  arrange (cdID, Code)
#
#
as.factor(perLU10$Label_Habitat)->perLU10$Label_Habitat
semiNat10<- perLU10%>%
  group_by(cdID,year,Label_Habitat)%>%
  dplyr::summarize (percHab = sum(percLU))%>%
  spread(Label_Habitat,percHab)%>%
  dplyr::rename(CDUID=cdID, cropPerc=cropland, semiNatPerc=semiNatural)
#
#
# 2015
perLU15<-ls2015 %>%
    dplyr::select (-c(X10,X20,X30,X34,X35,X130))%>% # remove all notRelevant (see crclass list for more) before taking sum to account only for ag areas
    rowwise() %>%
    dplyr::mutate(sum_area_km2 = sum(c_across(2:62), na.rm = TRUE))%>% # sum all agricultural crops
    gather(2:62, key=Code, value=area_km2)%>% 
    mutate(Code = gsub("X", "", Code),
           year = "2016", # year that will match the census survey reporting
           percLU = area_km2/sum_area_km2)%>% # calculate percent land use
    drop_na(area_km2)%>% # remove all NAs - no value so the crop is not there
    left_join(crclass[-3], by="Code")%>%
    arrange (cdID, Code)
#
as.factor(perLU15$Label_Habitat)->perLU15$Label_Habitat
semiNat15<- perLU15%>%
  group_by(cdID,year, Label_Habitat)%>%
  dplyr::summarize (percHab = sum(percLU))%>%
  spread(Label_Habitat,percHab)%>%
  dplyr::rename(CDUID=cdID, cropPerc=cropland, semiNatPerc=semiNatural)
#
semiNat<-rbind(semiNat10,semiNat15)
#
# write.csv(semiNat, "SemiNatural.csv", row.names=FALSE)
#
#
# ------------------------------------------------------------------------------------------------------
#
# Pesticide Mass
#
# ------------------------------------------------------------------------------------------------------
#
#### 2. Calculate mass from Pesticide Use Density map
#
aea.proj <-"+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
cd$area_ha <- area(cd) / 10000 # for sqm to ha or 1000000 for sqkm 
#
cd<- spTransform(cd, CRS(aea.proj))
head(as.data.frame(cd[order(cd$area_ha),]))
tail(as.data.frame(cd[order(cd$area_ha),]))
#
# Plot min - max size CD
x11()
plot(cd);
# 
# biggest
plot(cd[cd$CDUID=="4817",], col="red", add=T)
plot(cd[cd$CDUID=="2473",], col="blue", add=T)# can't see it in map
#
# Smallest
plot(cd[cd$PRUID=="24",])
plot(cd[cd$CDUID=="2473" & cd$PRUID=="24",], col="blue", add=T)# can't see it in map
#
findatCD[findatCD$CDUID=="5955",]
#
#
## Range of total farm areas
#
head(as.data.frame(findatCD[order(findatCD$tfarea),]))
tail(as.data.frame(findatCD[order(findatCD$tfarea),]))
#
# Restrict Prairies
#
cd_ppr<-cd[cd$PRNAME=="Saskatchewan" | cd$PRNAME=="Alberta"| cd$PRNAME=="Manitoba",]
#
x11(); plot(i_pest_c); plot(cd_ppr, add=T)
#
# Extract, take mean per unit and turn into dataframe
#
mass1 <- raster::extract(x = i_pest_c, y = cd_ppr) # takes a minute to run
names(mass1) = cd_ppr$CDUID
mass2<-data.frame(sapply(mass1, mean))
mass2$CDUID<-rownames(mass2)
colnames(mass2)[1]<-"rate_kg_km2"
mass2$C_YEAR<-"2016"
#
#
# Compare with Census of Ag data
#
## Theme for plotting
#
theme2<-  theme(axis.line = element_line(size=1, colour = "black"),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.border = element_blank(), panel.background = element_blank(),
                axis.text.x=element_text(colour="black", size = 12),
                axis.text.y=element_text(colour="black", size = 12),
                axis.title.x=element_text(colour="black", size = 14),
                axis.title.y=element_text(colour="black", size = 14),
                legend.text = element_text(color="black", size=12),
                legend.title = element_text(color="black", face="bold", size=13))
###
#
as.character(semiNat$CDUID)->semiNat$CDUID
join_ag<- findatCD %>%
  dplyr::filter(C_YEAR=="2016")%>% # only for 2016 when the data is consistent
  right_join(semiNat, by=c("CDUID"="CDUID", "C_YEAR"="year"))%>%
  left_join(mass2, by=c("CDUID", "C_YEAR"))%>%
  mutate(rate_kg_ha=rate_kg_km2/100, # kg/km2 to kg/ha 
         rate_kg=rate_kg_ha*crplnd)
# 
# 1. Semi-Natural Areas
corSemiNat<- join_ag %>%
  dplyr::filter(!is.na(landSimp))# the NAs are for all areas >90% natural - therefore likely just forests and not accounted in census data

corSemiNat %>%
group_by(C_YEAR, REGION)%>%
  dplyr::summarise(spearmanCor=cor(landSimp,semiNatPerc, use = "pairwise.complete.obs", method="spearman")) # spearman - seminat not normally distributed
#
join_ag %>%
  dplyr::filter(!is.na(landSimp))%>%
  group_by(C_YEAR)%>%
  dplyr::summarise(spearmanCor=cor(landSimp,semiNatPerc, use = "pairwise.complete.obs", method="spearman"))
#
#
semiNAt_plot<- ggplot(data=corSemiNat, aes(landSimp, semiNatPerc)) +
  geom_point()+ 
  theme2+
  stat_cor(method="spearman", label.x = .25, label.y = .25, p.accuracy = 0.001)+
  #geom_smooth(method = "loess")+
  labs(y = 'Proportion of cropland', x = 'Proportion of seminatural areas')
#
#ggsave(filename =file.path(figdir, "SI_Semi-NaturalCor.tiff"), dpi=500, compression = "lzw", width =5, height=5, plot = semiNAt_plot)
#
#  
# 2. Insecticide Use
insdt<- join_ag %>%
  dplyr::filter(!is.na(INSECTI) & !is.na(rate_kg))
#
insdt%>%
  group_by(C_YEAR, REGION)%>%
  dplyr::summarise(spearmanCor=cor(INSECTI, rate_kg, use = "pairwise.complete.obs", method="spearman")) # spearman 
#
head(insdt[c("CDUID","crplnd","rate_kg_km2","rate_kg_ha","rate_kg")])
#
ggplotRegression <- function (fit) {
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    theme2+
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 2),
                       "| Intercept =",signif(fit$coef[[1]],3),
                       "| Slope =",signif(fit$coef[[2]], 2),
                       "| p < 0.001"))
}
#
insMass<-lm(data=insdt, rate_kg~INSECTI)
insMass_plot<- ggplotRegression(insMass)+
  labs(y = 'Insecticide Mass (kg)', x = 'Area treated with insecticides (ha)')
#
#ggsave(filename =file.path(figdir, "SI_Insecticide-MassCor.tiff"), dpi=500, compression = "lzw", width =6, height=6, plot = insMass_plot)

