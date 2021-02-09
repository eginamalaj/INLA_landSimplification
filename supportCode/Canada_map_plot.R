#
#
#### Plotting Canada's inset maps
#
require(raster)
#
# Projection
aea.proj <-"+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#
# Map of Canada for the plot grob
camap <- getData("GADM", country = "canada", level = 1)
camap <- spTransform(camap, CRS(aea.proj))
camapF <- fortify(camap)
#
#
### Canada Borders
#
# To highlight the provinces
#
camapF$positive <- ifelse(camapF$id==1|camapF$id==4|camapF$id==7| # Prairie
                            camapF$id==3|camapF$id==13| #ON/QB
                            camapF$id==6| # BC
                            camapF$id==8|camapF$id==9|camapF$id==11|camapF$id==2, # Maritime
                          "Yes", "No") 
#
# Polygon only with the province's data
#
camapF2<-camapF[camapF$positive=="Yes",]
#
# Save polygon
# save(camapF2, "data/camapF2.RData")
#




