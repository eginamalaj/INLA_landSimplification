# ------------------------------------------------------------------------------------------------------
#
# Code for paper:
# Malaj & Morrissey (2021) Increased reliance on insecticide applications in Canada linked to simplified 
# agricultural landscapes. Ecological Applications
# Contact: eginamalaj@gmail.com
#
# ------------------------------------------------------------------------------------------------------
#
# Set your directory
setwd()
#
options(scipen=999)
require(tidyverse)
require(doBy)
require(INLA) 
require(rgdal)
require(spdep)# for neighbours - poly2nb
require(egg)# ggarrange
require(cowplot) #plot_grid
require(plyr)
require(cowplot)
#
# Difference between years
require(glmmTMB)
require(DHARMa)
require(emmeans)
require(car)
#
# For plotting INLA
require(MCMCglmm)
require(coda)
require(ape)
#
#
# ------------------------------------------------------------------------------------------------------
#### Functions
# ------------------------------------------------------------------------------------------------------
#
source("supportCode/efxplotFunc.R") # call: Efxplot
source("supportCode/dformat.R")# call: myspread, join.sp.df
#
# ------------------------------------------------------------------------------------------------------
#### Data
# ------------------------------------------------------------------------------------------------------
#
load("data/findatCD.RData") # call:findatCD - all data
load("data/camapF2.RData") # call:camapF2 - Fortified Canada polygon file from code Canada_map_plot
#
# ------------------------------------------------------------------------------------------------------
#
# Summary of parameters for Table 1
#
# ------------------------------------------------------------------------------------------------------
#
findatCD<-findatCD%>% dplyr::select(-INSECTI)
parsum<- as.data.frame(
  findatCD%>%
    group_by(REGION, C_YEAR)%>%
    dplyr::summarise_at(vars(grainPrc,oilsdPrc,plsesPrc,frtvegPrc,tilminPrc,netincm,frmsiz,gdd,pcp),
                        list(min=min, mean=mean,max=max))%>% # Q1=~quantile(., probs = 0.25), Q3=~quantile(., probs = 0.75)
    mutate(
      Cereal=paste0(round(grainPrc_mean,2), " (", round(grainPrc_min,2),",",round(grainPrc_max,2), ")"),
      Oilseeds=paste0(round(oilsdPrc_mean,2), " (", round(oilsdPrc_min,2),",",round(oilsdPrc_max,2), ")"),
      Pulses=paste0(round(plsesPrc_mean,3), " (", round(plsesPrc_min,3),",",round(plsesPrc_max,3), ")"),
      FruitsVeg=paste0(round(frtvegPrc_mean,3), " (", round(frtvegPrc_min,3),",",round(frtvegPrc_max,3), ")"),
      Tillage=paste0(round(tilminPrc_mean,2), " (", round(tilminPrc_min,2),",",round(tilminPrc_max,2), ")"),
      NetIncome=paste0(round(netincm_mean,0), " (", round(netincm_min,0),",",round(netincm_max,0), ")"),
      frmsiz=paste0(round(frmsiz_mean,0), " (", round(frmsiz_min,0),",",round(frmsiz_max,0), ")"),
      GDD=paste0(round(gdd_mean,0), " (", round(gdd_min,0),",",round(gdd_max,0), ")"),
      PCP=paste0(round(pcp_mean,0), " (", round(pcp_min,0),",",round(pcp_max,0), ")"),
    )
  %>%
    dplyr :: select(REGION,C_YEAR,Cereal,Oilseeds,Pulses,FruitsVeg,Tillage,frmsiz,NetIncome,GDD,PCP)
)
#
# ------------------------------------------------------------------------------------------------------
#
# Changes in Land Simpl and Insecticide Use
#
# ------------------------------------------------------------------------------------------------------
#
# Change order of regions
findatCD$REGION <- factor(findatCD$REGION, levels = c("PACIFIC","PRAIRIE", "CENTRAL", "ATLANTIC"))
#
hist(findatCD$landSimp); range(findatCD$landSimp) # proportion
#
# beta_family - dealing with proportional data (0-1)
ls_mod<-glmmTMB(data=findatCD, landSimp~C_YEAR*REGION +(1|CDUID),family="beta_family") 
#
# check assumptions - OK - beta distribution fits relatively well
ls_sim<- simulateResiduals(fittedModel = ls_mod, n = 250)
x11();plot(ls_sim)
testDispersion(ls_sim) # n.s. p-value indicates no under/overdispersion
#
aa = plot(ls_sim)
ggsave(filename ="Fig1SI-LS.tiff", dpi=500, compression = "lzw", width =10, height=10, plot = aa)


ls_anv<-Anova(ls_mod,type="II",test="Chisq")
#
ls_m<-emmeans(ls_mod, pairwise~C_YEAR|REGION, type = "response")
#
# Least square means
ls_m$emmeans
#
# For plotting
zero_margin <- theme(panel.spacing=grid::unit(0,"lines")) 
#
#
# Insecticides
#
hist(findatCD$insPrc); range(findatCD$insPrc) # nonnormal & proportion
i_mod<-glmmTMB(data=findatCD, insPrc~C_YEAR*REGION +(1|CDUID),dispformula = ~C_YEAR, family="beta_family") 
#
# check assumptions - OK - beta distribution fits relatively well
i_sim<- simulateResiduals(fittedModel = i_mod, n = 250)
x11();plot(i_sim)
testDispersion(i_sim) # n.s. p-value indicates no under/overdispersion
#
#test for overall interaction effect
ins_anv<-Anova(i_mod,type="II",test="Chisq")
#
ins_m<-emmeans(i_mod, pairwise~C_YEAR|REGION, type = "response")
#
# Least square means
ins_m$emmeans
#
### Plot means, CI and comparisons
#
# For interpretation of bars see references:
# https://stats.stackexchange.com/questions/173479/difference-between-confidence-intervals-and-comparison-arrows
# https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
# 
#
#X11(width=10, height = 10)
ls_plot<-plot(ls_m, comparisons =
                TRUE, horizontal = FALSE, colors = c("black", "blue", "blue", "red"))+
  facet_wrap(~ REGION, nrow=1, strip.position="top")+
  theme_bw(base_size = 16)+
  theme(strip.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(colour="black", size=12),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm"))+ 
  #scale_y_discrete(breaks = c(0.15,0.25,0.35,0.45,0.55,0.65,0.75))+
  labs(y = "", x = "Landscape simplification")+
  zero_margin 
#
#
#
ins_plot<-
  plot(ins_m, comparisons = TRUE, horizontal = FALSE, colors = c("black", "green", "green","red"))+
  facet_wrap(~ REGION, nrow=1, strip.position="top")+
  theme_bw(base_size = 16)+
  theme(strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, colour="black", size=14),
        axis.text.y = element_text(colour="black", size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(-1,0.3,1,0), "cm"))+ # ("top", "right", "bottom", "left") # remove the white space of around the graph
  #scale_y_discrete(breaks = c(0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16))+
  labs(y = "Year", x = "Insecticide applications")+
  zero_margin 
#
x11(width =10, height=10)
ls_ins<-plot_grid(ls_plot,ins_plot, nrow = 2);ls_ins #, rel_heights = c(3/5, 2/5)
#
# ------------------------------------------------------------------------------------------------------
#
# Census Division Shapefile
#
# ------------------------------------------------------------------------------------------------------
#
# Re-Project
aea.proj <-"+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
cd<- readOGR("data/shapefileCD/2016CD_ag.shp","2016CD_ag")
cd<- spTransform(cd, CRS(aea.proj))
#
# Polygons for each region. Remove last column in ToC
#
# Prairie
pprshp<-cd[cd$PRUID=="46" | cd$PRUID=="47" | cd$PRUID=="48",-6]
#
# BC
#
bcshp<-cd[cd$PRUID=="59",-6]
#
# Central
#
ccshp<-cd[cd$PRUID=="24" | cd$PRUID=="35",-6]
#
# Atlantic
#
acshp<-cd[cd$PRUID=="11" | cd$PRUID=="12" |cd$PRUID=="13",-6]
#
#
# Fortify for mapping in ggplot
cdF<-fortify(cd, region="CDUID")
#
#
# ------------------------------------------------------------------------------------------------------
#
# Mapping change in Land simplification
#
# ------------------------------------------------------------------------------------------------------
#
# Change calculation: (final_value-initial_value)/initial_value*100
#
chng<- findatCD[findatCD$C_YEAR=="1996" | findatCD$C_YEAR=="2016",c("C_YEAR","CDUID","REGION","insPrc","landSimp")]
#
chng2<- chng %>% myspread (C_YEAR, c("insPrc","landSimp"))
#
colnames(chng2)[3:6]<-c("insPrc96","landSimp96","insPrc16","landSimp16")
#
chng2$lschng <- ((chng2$landSimp16 - chng2$landSimp96) / chng2$landSimp96)*100
chng2$inschng <- ((chng2$insPrc16 - chng2$insPrc96) / chng2$insPrc96)*100
#
# All NA - not analysed in 1996 or 2016 - Remove for map
chng3<- chng2[!is.na(chng2$insPrc96) & !is.na(chng2$insPrc16),]
range(chng3$lschng)
range(chng3$inschng)
#
as.factor(chng3$CDUID)->chng3$CDUID
as.factor(cdF$id)->cdF$id
#
# Merge with spatial dataset (fortified)
pdt_chng<-merge(cdF,chng3, by.x="id", by.y="CDUID", all.y=T)
#
nrow(data.frame(unique(pdt_chng$id)))==nrow(chng3)
#
#
#################### Plotting
#
# Landscape Simplification
#
require(classInt)
#
x11();hist((chng3$lschng[chng3$lschng>0])) # left skewed - quantile
#
classIntervals((pdt_chng$lschng), n=6, style="equal")
#
classIntervals((chng3$lschng[chng3$lschng>0]), n=3, style="quantile")
classIntervals((chng3$lschng[chng3$lschng<0]), n=2, style="equal")

# Always order - makes the polygons not split
pdt_chng<-pdt_chng[order(pdt_chng$order), ] # makes the graph not split
#
#
### Plot landscape simplification
#
# Manually picked breaks - note no zeros - either increased or decreased
pdt_chng$brkc_ls <- cut(pdt_chng$lschng, include.lowest=FALSE,
                        breaks=c(-51,-25,0,10,20,170), # 7 levels
                        labels=c("[-50 to -25)","[-25 to -1.5]","[0.1 to 10)", "[10 to 20)",
                                 ">20")) # 6 levels
#
# Quantile scales - each class ~20-25%
nrow(chng3[chng3$lschng<10 & chng3$lschng> 0.1,])/nrow(chng3) # 31%
nrow(chng3[chng3$lschng<20 & chng3$lschng> 10,])/nrow(chng3) # 30%
nrow(chng3[chng3$lschng>20,])/nrow(chng3) # 28%
#
# Units increasing 
nrow(chng3[chng3$lschng>0,])/nrow(chng3) # 89% 
nrow(chng3[chng3$lschng<0,])/nrow(chng3) # 11%   
#
#X11(width = 11, height = 6)
#
ls_p<- ggplot() +
  geom_polygon(data = pdt_chng, aes(fill = brkc_ls, 
                                    x = long, 
                                    y = lat, 
                                    group = group),
               color="white") +
  geom_polygon(data = camapF2, aes(x=long, y=lat, group = group), fill=NA, color = 'black', size = 0.1)+
  scale_x_continuous(limits = c(-1500000, 4300000))+ # everything visible
  scale_y_continuous(limits = c(4800000, 7500000))+
  coord_equal() +
  theme_map() +
  theme(legend.position=c(0.05, 0.2),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        plot.margin = unit(c(-0.7,0.1,-0.6,0.1), "cm"))+ # ("top", "right", "bottom", "left")
  scale_fill_manual(name="Change in \nLandscape Simplification (%)", #(white = no data) \n1996-2016
                    values=c("#01665e", "#5ab4ac",
                             "#dfc27d",
                             "#bf812d", "#8c510a"))+ 
  guides(fill = guide_legend(nrow = 1))+
  #guide = guide_legend(reverse = F,
  #                     fill = guide_legend(nrow = 2,byrow=TRUE)))+
  labs(x = NULL, 
       y = NULL)
#ls_p
#
### Plot insecticide use
#
x11();hist(pdt_chng$inschng)
#
hist(chng3$inschng[chng3$inschng>0]) # left skewed - quantile
#
classIntervals((chng3$inschng[chng3$inschng>0]), n=3, style="quantile")
classIntervals((chng3$inschng[chng3$inschng<0]), n=2, style="equal")
#
# Manually picked breaks - note no zeros - either increased or decreased
pdt_chng$brkc_ins <- cut(pdt_chng$inschng, include.lowest=FALSE,
                         breaks=c(-91,-50, 0, 70, 200, 1210), # 
                         labels=c("[-90 to -50)","[-50 to -0.7)", "[0.1 to 70)","[70 to 200)",
                                  ">200")) 
# Quantile scales - each class ~20-25%
nrow(chng3[chng3$inschng<78 & chng3$inschng> 0.1,])/nrow(chng3) # 23%
nrow(chng3[chng3$inschng<200 & chng3$inschng> 78,])/nrow(chng3) # 25%
nrow(chng3[chng3$inschng>200,])/nrow(chng3) # 22%
#
# Units increasing 
nrow(chng3[chng3$inschng>0,])/nrow(chng3) # 70% 
nrow(chng3[chng3$inschng<0,])/nrow(chng3) # 30%   
#
#
ins_p<-
  ggplot() +
  geom_polygon(data = pdt_chng, aes(fill = brkc_ins, 
                                    x = long, 
                                    y = lat, 
                                    group = group),
               color="white") +
  geom_polygon(data = camapF2, aes(x=long, y=lat, group = group), fill=NA, color = 'black', size = 0.1)+
  scale_x_continuous(limits = c(-1500000, 4300000))+ # everything visible
  scale_y_continuous(limits = c(4800000, 7500000))+
  coord_equal() +
  theme_map() +
  theme(legend.position=c(0.05, 0.2),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        plot.margin = unit(c(-0.7,0.1,-0.6,0.1), "cm"))+
  scale_fill_manual(name="Change in \nInsecticide applications (%)", #(white = no data) \n1996-2016
                    values= c("#2166AC","#D1E5F0", # cols generated from brewer.pal(10,"RdBu")
                              "#FDBB84", 
                              "#EF6548","#D7301F",
                              "#B30000"))+ 
  guides(fill = guide_legend(nrow = 1))+
  #guide = guide_legend(reverse = F,
  #                     fill = guide_legend(nrow = 2,byrow=TRUE)))+
  labs(x = NULL, 
       y = NULL)
#
#X11(width = 11, height = 9)
chng<-plot_grid(ls_p,ins_p, ncol=1, labels = c('A', 'B'), label_x = 0.05, vjust=3)
#
#
# ------------------------------------------------------------------------------------------------------
#
# More Formatting before INLA 
#
# ------------------------------------------------------------------------------------------------------
#
as.factor(findatCD$GEO_PROV)->findatCD$GEO_PROV
#
ca<- as.data.frame(cd); as.character(ca$CDUID)->ca$CDUID; nrow(unique(ca[1])) # 225 individual polygons
#
ca2 <- data.frame(expand.grid(id=as.character(unique(ca$CDUID)),
                              yr=as.factor(c(1996,2001,2006,2011,2016)))); nrow(ca2) # 225 x5 years = 1125
ca2$id<- as.character(ca2$id)
#
ca3<-left_join(ca2, findatCD, by=c('id' ='CDUID' , 'yr'='C_YEAR')); nrow(ca2)==nrow(ca3)
#
ca3$yr<- as.numeric(as.character(ca3$yr)); ca3[is.na(ca3$yr),]
#
# Remove NAs - created by the merge above
ca4<-ca3[!is.na(ca3$landSimp),]
#
# Remove entries that have only 1 & 2 years reported
camn<-ca4 %>% 
  group_by(id) %>% 
  dplyr::summarise(yr.n = length(yr))
camn2<-camn[camn$yr.n<3,]; nrow(camn2) # 8 polygons that have less than 3 years
#
# Keep only polygons that have 3 years in common
camn3<-ca4[!ca4$id %in% camn2$id,]; nrow(unique(camn3[1]))- nrow(unique(ca4[1])) # -8 polygon removed
#
# Final Dataset 
#
# Division by a factor to have variables at the same range. Note that below we'll centre too before INLA
#
ca5<-camn3%>%
  mutate(gdd=gdd/1000,
         pcp=pcp/1000,
         netincm=netincm/1000,
         frmsiz=frmsiz/100
  )
#
# Check amount of zeros for each variable to decide what variable to keep for each region
#
camn3 %>% 
  na.omit() %>% 
  group_by(REGION, yr) %>% 
  dplyr::summarise(ptg = n(),
                   grainPrc_n= (sum(grainPrc==0)/ptg)*100,
                   oilsdPrc_n= (sum(oilsdPrc==0)/ptg)*100,
                   frtvegPrc_n= (sum(frtvegPrc==0)/ptg)*100,
                   plsesPrc_n= (sum(plsesPrc==0)/ptg)*100
  )
#
#
# Percentage
camn3 %>% 
  na.omit() %>% 
  group_by(REGION) %>% 
  dplyr::summarise(ptg = n(),
                   grain_mean= (mean(grainPrc*100)),
                   oilsd_mean= (mean(oilsdPrc*100)),
                   frtveg_mean= (mean(frtvegPrc*100)),
                   plses_mean= (mean(plsesPrc*100)),
                   tilg_mean= (mean(tilminPrc*100))
  )
#
# Removed per province everything that the average is less than 2 percent
# BC - oilseeds, pulses 
# AC - oilseeds and pulses
# CC - pulses
# Prairie - fruits and veg
#
# ------------------------------------------------------------------------------------------------------
#
# INLA Regional Models
#
# ------------------------------------------------------------------------------------------------------
#
#####################
#
# Prairie
#
#####################
#
#### Data
#
inla_ppr<-ca5[ca5$REGION=="PRAIRIE",]; range(inla_ppr$insPrc)
#
# Create a unique dataset with all polygons that are found at least once
####
#
idppr<-as.data.frame(unique(inla_ppr$id)); nrow(idppr) # 53 polys
colnames(idppr)<-"id"
idppr2<-rbind(idppr,idppr,idppr,idppr,idppr)
idppr2$yr<-rep(c(1996,2001,2006,2011,2016), each=53)
#
####
#
inla_ppr2<-merge(idppr2, inla_ppr, by.x=c("id","yr"),by.y=c("id","yr"), all.x=T)
inla_ppr2[is.na(inla_ppr2$insPrc),] # no polys with the missing data. Ok.
#
inla_ppr3<-arrange(inla_ppr2,as.numeric(inla_ppr2$yr),as.numeric(inla_ppr2$id))
#
inla_ppr3$id_area_1 <- rep(1:53, 5)
inla_ppr3$id_year_1 <- rep(1:5, each=53)
inla_ppr3$id_area_2 <- rep(1:53, 5)
inla_ppr3$id_year_2 <- rep(1:5, each=53)
#
inla_ppr3[inla_ppr3$id_area_1==2,] # same number for each site - same as id
inla_ppr3[inla_ppr3$id_year_1==2,] # same number for each year - same as yr
#
# Scale all parameters - mean 0 and SD of 1
for (i in 9:18) {
  inla_ppr3[,i] <- as.numeric(scale(inla_ppr3[,i], scale=F))
}
inla_ppr3$year <- as.numeric(scale(inla_ppr3$yr, scale=F))
#
# Shapefile
ppr<- as.data.frame(pprshp); as.character(ppr$CDUID)->ppr$CDUID
#
# Merge
ppr2<-merge(inla_ppr3, ppr, by.x='id', by.y='CDUID', all.x=T); nrow(unique(ppr2[1])) # 53 polygons out of 54 - poly CDUID=4619 removed as has only two years
#
ppr3<-ppr2
#
## Data for spatial autocorrelation
#
# ids of polygon
dtppr<-data.frame(cdid=unique(ppr3$id))
#as.character(dtppr$cdid)->dtppr$cdid
#
cdppr <-join.sp.df(pprshp, dtppr, xcol="CDUID", ycol="cdid")
#
nghbppr <- poly2nb(cdppr)
nb2INLA("ppr.graph", nghbppr) # only once to create the .graph
ppr.adj <- paste(getwd(),"/ppr.graph",sep="")
g.ppr <- INLA::inla.read.graph("ppr.graph"); x11(); plot(g.ppr)
summary(g.ppr)
plot(nghbppr, coordinates(cdppr), col="gray", cex=0.2) # remove one CDUID to plot (from 54 to 53)
#
#
# model6 main fixed effects and year trend plus random intercepts
# for county and year and CAR intercept
#
# iid - independent and identically distributed random variables
# space-time models: 
# besag - for the spatial domain
# ar1 - first order conditional autoregressive model (CAR)
# besag - nearby things are similar. choose neighbours based on graph (distance between polygons)
#
#
# Check for collinearity
#
cor(ppr3[!is.na(ppr3$insPrc),9:18]) ##collinear: oilseeds r>0.7  remove
#
fppr<- insPrc ~ 
  # fixed effects. 1 is for the intercept. -1 if we don't need an intercept
  1+ landSimp +frmsiz + netincm + tilminPrc+ grainPrc+ plsesPrc+
  gdd + pcp + 
  f(id_area_1, model="iid") + # random effect area
  f(id_year_1, model="iid") + # random effect year
  f(id_area_2, model="besag", graph=ppr.adj, # space
    group=id_year_2, control.group=list(model="ar1")) # time
#
pprinla<- inla(fppr,family="beta",data=ppr3,
               control.predictor=list(link = 1, compute=T),
               control.compute=list(dic=T,cpo=T, waic=T),
               num.threads=6,
               verbose=TRUE)
summary(pprinla)
#
#
#####################
#
# PACIFIC
#
#####################
#
#
#### Data
#
inla_bc<-ca5[ca5$REGION=="PACIFIC",]
#
# Create a unique dataset with all polygons that are found at least once
####
#
idbc<-as.data.frame(unique(inla_bc$id)); nrow(idbc) #19 polys
colnames(idbc)<-"id"
idbc2<-rbind(idbc,idbc,idbc,idbc,idbc)
idbc2$yr<-rep(c(1996,2001,2006,2011,2016), each=19)
#
####
#
inla_bc2<-merge(idbc2, inla_bc, by.x=c("id","yr"),by.y=c("id","yr"), all.x=T)
inla_bc2[is.na(inla_bc2$insPrc),] # polys with the missing data
#
inla_bc3<-arrange(inla_bc2,as.numeric(inla_bc2$yr),as.numeric(inla_bc2$id))
#
inla_bc3$id_area_1 <- rep(1:19, 5)
inla_bc3$id_year_1 <- rep(1:5, each=19)
inla_bc3$id_area_2 <- rep(1:19, 5)
inla_bc3$id_year_2 <- rep(1:5, each=19)
#
inla_bc3[inla_bc3$id_area_1==2,] # same number for each site - same as id
inla_bc3[inla_bc3$id_year_1==2,] # same number for each year - same as yr
#
# Scale all parameters - mean 0 and SD of 1
for (i in 9:18) {
  inla_bc3[,i] <- as.numeric(scale(inla_bc3[,i], scale=F))
}
inla_bc3$year <- as.numeric(scale(inla_bc3$yr, scale=F))
#
# Shapefile
bc<- as.data.frame(bcshp); as.character(bc$CDUID)->bc$CDUID
#
# Merge
bc2<-merge(inla_bc3, bc, by.x='id', by.y='CDUID', all.x=T); nrow(unique(bc2[1])) # 19 polygons
#
bc3<-bc2
#
##
#
# INLA Modeling
#
## Data for spatial autocorrelation
#
# ids of polygon
dtbc<-data.frame(cdid=unique(bc3$id))
as.character(dtbc$cdid)->dtbc$cdid
#
cdbc <-join.sp.df(bcshp, dtbc, xcol="CDUID", ycol="cdid")
#
nghbbc <- poly2nb(cdbc)
nb2INLA("bc.graph", nghbbc) # only once to create the .graph
bc.adj <- paste(getwd(),"/bc.graph",sep="")
g.bc <- inla.read.graph("bc.graph"); x11(); plot(g.bc)
summary(g.bc)
plot(nghbbc, coordinates(bcshp), col="gray", cex=0.2) # 19 in total - nothing to remove here
#
cor(bc3[!is.na(bc3$crplnd),c(9:13,15,17:19)]) #collinear:  r>0.7
#
fbc<- insPrc ~ 
  1+ landSimp + frmsiz + tilminPrc +grainPrc+ frtvegPrc + netincm+
  gdd + pcp + 
  f(id_area_1, model="iid") + # random effect area
  f(id_year_1, model="iid") + # random effect year
  f(id_area_2, model="besag", graph=bc.adj, # space
    group=id_year_2, control.group=list(model="ar1")) # time
#
bcinla<- inla(fbc,family="beta",data=bc3,
              control.predictor=list(link = 1, compute=T),
              control.compute=list(dic=T,cpo=T, waic=T),
              num.threads=6,
              verbose=T)
summary(bcinla)
#
as.data.frame(bcinla$summary.fixed)
plogis(-3.03728604 + (0.958128520*0.5))
#
#
#
#####################
#
# Central Canada
#
#####################
#
#
#### Data
#
inla_cc<-ca5[ca5$REGION=="CENTRAL",]
#
# Create a unique dataset with all polygons that are found at least once
####
#
idcc<-as.data.frame(unique(inla_cc$id)); nrow(idcc) # 115 polys
colnames(idcc)<-"id"
idcc2<-rbind(idcc,idcc,idcc,idcc,idcc)
idcc2$yr<-rep(c(1996,2001,2006,2011,2016), each=115)
#
####
#
inla_cc2<-merge(idcc2, inla_cc, by.x=c("id","yr"),by.y=c("id","yr"), all.x=T)
inla_cc2[is.na(inla_cc2$insPrc),] # polys with the missing data. OK
#
inla_cc3<-arrange(inla_cc2,as.numeric(inla_cc2$yr),as.numeric(inla_cc2$id))
#
inla_cc3$id_area_1 <- rep(1:115, 5)
inla_cc3$id_year_1 <- rep(1:5, each=115)
inla_cc3$id_area_2 <- rep(1:115, 5)
inla_cc3$id_year_2 <- rep(1:5, each=115)
#
inla_cc3[inla_cc3$id_area_1==2,] # same number for each site - same as id
inla_cc3[inla_cc3$id_year_1==2,] # same number for each year - same as yr
#
# Scale all parameters - mean 0 and SD of 1
for (i in 9:18) {
  inla_cc3[,i] <- as.numeric(scale(inla_cc3[,i], scale=F))
}
inla_cc3$year <- as.numeric(scale(inla_cc3$yr, scale=F))
#
# Shapefile
cc<- as.data.frame(ccshp); as.character(cc$CDUID)->cc$CDUID
#
# Merge
cc2<-merge(inla_cc3, cc, by.x='id', by.y='CDUID', all.x=T); nrow(unique(cc2[1])) # 115 polygons
#
cc3<-cc2
#
########
#
#
##### INLA 
#
## Data for spatial autocorrelation
#
# ids of polygon
dtcc<-data.frame(cdid=unique(cc3$id))
as.character(dtcc$cdid)->dtcc$cdid
#
cdcc <-join.sp.df(ccshp, dtcc, xcol="CDUID", ycol="cdid")
#
# Plot number Ids inside polygons
#plot(cdcc); pointLabel(coordinates(cdcc),labels=cdcc$CDUID)
#
nghbcc <- poly2nb(cdcc)
nb2INLA("cc.graph", nghbcc) # only once to create the .graph
cc.adj <- paste(getwd(),"/cc.graph",sep="")
g.cc <- inla.read.graph("cc.graph"); plot(g.cc)
summary(g.cc)
plot(nghbcc, coordinates(cdcc), col="gray", cex=0.2)# cdcc - coordinates of matching polygons only
#
cor(cc3[!is.na(cc3$insPrc),c(9:18)])# collinear: grain, oilseeds and tilminPrc r>0.7
#
fcc<- insPrc ~ 
  1+ landSimp+ frmsiz + netincm + frtvegPrc + gdd +pcp  + tilminPrc+
  f(id_area_1, model="iid") + # random effect area
  f(id_year_1, model="iid") + # random effect year
  f(id_area_2, model="besag", graph=cc.adj, # space
    group=id_year_2, control.group=list(model="ar1")) # time
#
ccinla<- inla(fcc,family="beta",data=cc3,
              control.predictor=list(link = 1, compute=T),
              control.compute=list(dic=T,cpo=T, waic=T),
              num.threads=6,
              verbose=T)
summary(ccinla)
#
as.data.frame(ccinla$summary.fixed)
plogis(-2.93868453 + (1.04679489*0.6))
#
####
#
#####################
#
# ATLANTIC
#
#####################
#
#
#### Data
#
inla_ac<-ca5[ca5$REGION=="ATLANTIC",]
#
# Create a unique dataset with all polygons that are found at least once
####
#
idac<-as.data.frame(unique(inla_ac$id)); nrow(idac) # 26 polys
colnames(idac)<-"id"
idac2<-rbind(idac,idac,idac,idac,idac)
idac2$yr<-rep(c(1996,2001,2006,2011,2016), each=26)
#
####
#
inla_ac2<-merge(idac2, inla_ac, by.x=c("id","yr"),by.y=c("id","yr"), all.x=T)
inla_ac2[is.na(inla_ac2$insPrc),] # polys with the missing data. OK
#
inla_ac3<-arrange(inla_ac2,as.numeric(inla_ac2$yr),as.numeric(inla_ac2$id))
#
inla_ac3$id_area_1 <- rep(1:26, 5)
inla_ac3$id_year_1 <- rep(1:5, each=26)
inla_ac3$id_area_2 <- rep(1:26, 5)
inla_ac3$id_year_2 <- rep(1:5, each=26)
#
inla_ac3[inla_ac3$id_area_1==2,] # same number for each site - same as id
inla_ac3[inla_ac3$id_year_1==2,] # same number for each year - same as yr
#
# Scale all parameters - mean 0 and SD of 1
for (i in 9:18) {
  inla_ac3[,i] <- as.numeric(scale(inla_ac3[,i], scale=F))
}
inla_ac3$year <- as.numeric(scale(inla_ac3$yr, scale=F))
#
# Shapefile
ac<- as.data.frame(acshp); as.character(ac$CDUID)->ac$CDUID
#
# Merge
ac2<-merge(inla_ac3, ac, by.x='id', by.y='CDUID', all.x=T); nrow(unique(ac2[1])) # 26 polygons
#
ac3<-ac2
#
#
#
##### INLA 
#
## Data for spatial autocorrelation
#
# ids of polygon
dtac<-data.frame(cdid=unique(ac3$id))
as.character(dtac$cdid)->dtac$cdid
#
cdac <-join.sp.df(acshp, dtac, xcol="CDUID", ycol="cdid")
#
# Plot number IDs in the polygons
# plot(cdac); pointLabel(coordinates(cdac),labels=cdac$CDUID)
#
nghbac <- poly2nb(cdac)
nb2INLA("ac.graph", nghbac) # only once to create the .graph
ac.adj <- paste(getwd(),"/ac.graph",sep="")
g.ac <- inla.read.graph("ac.graph"); plot(g.ac)
summary(g.ac)
plot(nghbac, coordinates(cdac), col="gray", cex=0.2)
#
cor(ac3[!is.na(ac3$insPrc),c(9:18)])# collinear: grain r>0.7
#
fac<- insPrc ~ 
  1+ landSimp + frmsiz + netincm + tilminPrc + frtvegPrc + gdd +pcp  +
  f(id_area_1, model="iid") + # random effect area
  f(id_year_1, model="iid") + # random effect year
  f(id_area_2, model="besag", graph=ac.adj, # space
    group=id_year_2, control.group=list(model="ar1")) # time
#
acinla<- inla(fac,family="beta",data=ac3,
              control.predictor=list(link = 1, compute=T),
              control.compute=list(dic=T,cpo=T, waic=T),
              num.threads=6,
              verbose=T)
summary(acinla)
#
as.data.frame(acinla$summary.fixed)
plogis(-2.100737585 + (0.619839331*0.5))
#
#
# ------------------------------------------------------------------------------------------------------
#
# Moran Index Test to test there is spatial autocorrelation
#
# ------------------------------------------------------------------------------------------------------
# 
# 1. BC
wwbc <-  nb2listw(nghbbc)
moran.test(bc3$insPrc[bc3$yr=="2016"], wwbc,na.action=na.omit)
#
# 2. Prairie
wwppr <-  nb2listw(nghbppr)
moran.test(ppr3$insPrc[ppr3$yr=="2016"], wwppr)
#
# 3. Central
x11(); plot(cdcc); pointLabel(coordinates(cdcc),labels=cdcc$CDUID)
#
# remove one polygon that it's not connected
cdcc2 = cdcc[!cdcc$CDUID==3553,]
nghbcc2 <- poly2nb(cdcc2)
wwcc <-  nb2listw(nghbcc2)
moran.test(cc3$insPrc[cc3$yr=="2016" & !cc3$id==3553], wwcc,na.action=na.omit)
#
# 4. Atlantic
x11(); plot(cdac); pointLabel(coordinates(cdac),labels=cdac$CDUID)
#
# remove one polygon that it's not connected
cdac2 = cdac[!cdac$CDUID==1315,]
nghbac2 <- poly2nb(cdac2)
wwac <-  nb2listw(nghbac2)
moran.test(ac3$insPrc[ac3$yr=="2016"& !ac3$id==1315], wwac,na.action=na.omit)
#
#
# ------------------------------------------------------------------------------------------------------
#
# Model predictions
#
# ------------------------------------------------------------------------------------------------------
#
# For cross validation reference: 
# https://github.com/thopitz/landslides-point-process/blob/master/Chapter.R 
#
pprfix<-as.data.frame(pprinla$summary.fixed); pprfix$parm<-rownames(pprfix);pprfix$parm[1]<-"intercept";pprfix$Region<-"PRAIRIE"
bcfix<-as.data.frame(bcinla$summary.fixed);bcfix$parm<-rownames(bcfix);bcfix$parm[1]<-"intercept";bcfix$Region<-"PACIFIC"
ccfix<-as.data.frame(ccinla$summary.fixed); ccfix$parm<-rownames(ccfix);ccfix$parm[1]<-"intercept";ccfix$Region<-"CENTRAL"
acfix<-as.data.frame(acinla$summary.fixed); acfix$parm<-rownames(acfix);acfix$parm[1]<-"intercept";acfix$Region<-"ATLANTIC"
#
#### All 
#
intbind<-
  rbind(
    pprfix[pprfix$parm=="intercept" | pprfix$parm=="landSimp", c(1,8:9)], 
    bcfix[bcfix$parm=="intercept" | bcfix$parm=="landSimp", c(1,8:9)],
    ccfix[ccfix$parm=="intercept" | ccfix$parm=="landSimp", c(1,8:9)],
    acfix[acfix$parm=="intercept" | acfix$parm=="landSimp", c(1,8:9)]
  )
#
sppar<-spread(data=intbind, parm, mean)
#
sppar2<- sppar %>%
  mutate(`0.1`=plogis(intercept+ (landSimp*0.1)),
         `0.2`=plogis(intercept+ (landSimp*0.2)),
         `0.3`=plogis(intercept+ (landSimp*0.3)),
         `0.4`=plogis(intercept+ (landSimp*0.4)),
         `0.5`=plogis(intercept+ (landSimp*0.5)),
         `0.51`=plogis(intercept+ (landSimp*0.51)),
         `0.55`=plogis(intercept+ (landSimp*0.55)),
         `0.6`=plogis(intercept+ (landSimp*0.6)),
         `0.63`=plogis(intercept+ (landSimp*0.63)),
         `0.7`=plogis(intercept+ (landSimp*0.7)),
         `0.8`=plogis(intercept+ (landSimp*0.8)),
         `0.9`=plogis(intercept+ (landSimp*0.9))
  )%>%
  dplyr :: select (-intercept,-landSimp)%>%
  gather(
    plandsimp ,predict, -Region
  )

sppar2$plandsimp<-as.numeric(sppar2$plandsimp)
sppar2$Region<-as.factor(sppar2$Region)
#
sppar3<-sppar2[sppar2$Region=="PRAIRIE" | sppar2$Region=="CENTRAL",]
#
#
x11(width=8, height=5)
pred_plot<-
  ggplot(data=sppar3, aes(x=plandsimp, y=predict, color=Region)) +
  geom_line(size = 2, alpha = .8) +
  #geom_point(data=sppar3, aes(x=plandsimp, y=predict), size = 3, color="black")+
  geom_vline(xintercept = c(0.5,0.6, 0.7), linetype="dashed", size=1)+
  annotate("rect",xmin=0.5,xmax=0.7,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black")+
  geom_text(aes(x=0.5, y=0.17, label="1996"), size=5, angle=90, vjust=-0.4, hjust=0, color="black")+
  geom_text(aes(x=0.6, y=0.17, label="2016"), size=5, angle=90, vjust=-0.4, hjust=0, color="black")+
  geom_text(aes(x=0.7, y=0.17, label="2036"), size=5, angle=90, vjust=-0.4, hjust=0, color="black")+
  theme_classic(base_size = 18) +
  scale_colour_manual(values=c("#FC4E07","#E69F00")) +
  scale_x_continuous(breaks = seq(min(sppar2$plandsimp), max(sppar2$plandsimp), by = 0.1))+
  scale_y_continuous(breaks = seq(round(min(sppar2$predict),2), round(max(sppar2$predict),2), by = 0.02))+
  theme(legend.position = c(0.15, 0.9),
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"))+
  #scale_x_continuous(expand = c(0, 0)) +
  #scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends))+
  labs(x = "Landscape Simplification", y = "Predicted Insecticide Applications")
#
#
#ggsave(filename =file.path(figdir, "Fig4_Predictions.tiff"), dpi=500, compression = "lzw", width =8, height=5, plot = pred_plot)
#
#
# Plot INLA Estimates
#
bc_plotm<-Efxplot(bcinla)+ 
  labs(title="PACIFIC")+
  theme(plot.title = element_text(color = "black", size = 18, 
                                  face = "bold"))
ppr_plotm<- Efxplot(pprinla)+ 
  labs(title="PRAIRIE")+
  theme(plot.title = element_text(color = "black", size = 18, 
                                  face = "bold"))

cc_plotm<-Efxplot(ccinla)+ 
  labs(title="CENTRAL")+
  theme(plot.title = element_text(color = "black", size = 18, 
                                  face = "bold"))

ac_plotm<-Efxplot(acinla)+ 
  labs(title="ATLANTIC")+
  theme(plot.title = element_text(color = "black", size = 18, 
                                  face = "bold"))

plotm<-plot_grid(bc_plotm, ppr_plotm, cc_plotm, ac_plotm, nrow = 2)
#
X11(width =9, height=12);plotm
#
# For text
#
inppr<-as.data.frame(pprinla$summary.fixed)
inppr$Var <- rownames(inppr); inppr$region<-"ppr"
#
inbc<-as.data.frame(bcinla$summary.fixed)
inbc$Var <- rownames(inbc); inbc$region<-"bc"
#
incc<-as.data.frame(ccinla$summary.fixed)
incc$Var <- rownames(incc); incc$region<-"cc"
#
inac<-as.data.frame(acinla$summary.fixed)
inac$Var <- rownames(inac); inac$region<-"ac"
#
inla_prov<-rbind(inppr,inbc,incc,inac)
#
inla_prov[inla_prov$region=="cc", ]
#
#
#########################################################################################################
#
######### Checking for multicolinearity 
#
# lm to check VIFs
#
glm_bc <- lm(insPrc ~ landSimp+frmsiz+tilminPrc+netincm+grainPrc+frtvegPrc+gdd+pcp,data = bc3)
vif(glm_bc)

glm_ppr <- lm(insPrc ~ landSimp+frmsiz+tilminPrc+netincm+grainPrc+plsesPrc+gdd+pcp,data = ppr3)
vif(glm_ppr)

glm_cc <- lm(insPrc ~ landSimp+frmsiz+tilminPrc+netincm+frtvegPrc+gdd+pcp,data = cc3)
vif(glm_cc)

glm_ac <- lm(insPrc ~ landSimp+frmsiz+tilminPrc+netincm+frtvegPrc+gdd+pcp,data = ac3)
vif(glm_ac)
#
#
# Correlations
#
ls_col = c("REGION","landSimp","frmsiz","tilminPrc","netincm","grainPrc","oilsdPrc","frtvegPrc","plsesPrc","gdd","pcp")
#
cor_df<-rbind(
  inla_bc2[,ls_col],
  inla_ppr2[,ls_col],
  inla_cc2[,ls_col],
  inla_ac2[,ls_col])
#
#
cor_bc=data.frame(cor(cor_df[cor_df$REGION=="PACIFIC",-1], use="pairwise.complete.obs"))
cor_bc$var= row.names(cor_bc); cor_bc$Region="Pacific"
#
cor_ppr=data.frame(cor(cor_df[cor_df$REGION=="PRAIRIE",-1], use="pairwise.complete.obs"))
cor_ppr$var= row.names(cor_ppr); cor_ppr$Region="Prairie"
#
cor_cc=data.frame(cor(cor_df[cor_df$REGION=="CENTRAL",-1], use="pairwise.complete.obs"))
cor_cc$var= row.names(cor_cc); cor_cc$Region="Central"
#
cor_ac=data.frame(cor(cor_df[cor_df$REGION=="ATLANTIC",-1], use="pairwise.complete.obs"))
cor_ac$var= row.names(cor_ac); cor_ac$Region="Atlantic"
#
cor_all=rbind(cor_bc,cor_ppr,cor_cc,cor_ac)
#

