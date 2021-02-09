#
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
# save(camapF2, file="C:/Users/Egina/Dropbox/_Work/R-Pest/CensusAg2020/RData/camapF2.RData")
#
# Create group variable
camapF2_gr<- camapF2 %>%
  mutate(grp = if_else(id == 1 | id == 4 | id == 7 , "Prairie",
                       ifelse(id == 3 | id == 13, "Central",
                              ifelse(id == 8 | id == 9| id == 11 | id == 2, "Atlantic","Pacific"))))
#
# Polygons in gray - for map of Canada
camapF3 <- #ggplotGrob( )#for using in a figure directly
  ggplot() +
    geom_blank(data =camapF, aes(x = long, y = lat)) +
    geom_map(data = camapF, map = camapF,
             aes(group = group, map_id = id),
             fill = "white", color = "black", size = 0.3)+
    geom_polygon(data = camapF2, aes(x=long, y=lat, group = group),fill="#b2b2b2", color = "black", size = 0.3)+
    theme_map()+
    theme(panel.background = element_rect(fill = NULL), panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=2))
#
#
# Polygons in color
camapF4 <- #ggplotGrob() # to add map directly into the figure
  ggplot() +
    geom_blank(data =camapF, aes(x = long, y = lat)) +
    geom_map(data = camapF, map = camapF,
             aes(group = group, map_id = id),
             fill = "white", color = "black", size = 0.3)+
    geom_polygon(data = camapF2_gr, aes(x=long, y=lat, group = group ,fill= factor(grp)),color = "black", size = 0.3)+
    scale_fill_manual(values=c("#41B6C4","#225EA8","#FC4E07","#E69F00"))+ # colors not right
    theme_map()+
    theme(panel.background = element_rect(fill = NULL), panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          legend.position = "none")
# 
#
# Save it so it can be added to the boxplots manually
#ggsave(filename =file.path(figdir, "Inset_Map.tiff"), width = 3.5, height = 3, plot = camapF3)   
#
#



