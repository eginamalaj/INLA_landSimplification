###
###
### Function to plot INLA estimates
###
###
#
# Code adapted from: https://github.com/gfalbery/ggregplot/blob/master/R/Model%20Effects%20Plot.R
# For tutorials see: https://ourcodingclub.github.io/2018/12/04/inla.html
#
#
Efxplot <- function(ModelList,
                    Sig = TRUE, StarLoc = NULL,
                    Alpha1 = 1, Alpha2 = 1,
                    PointOutline = F,
                    ModelNames = NULL,
                    VarNames = NULL, VarOrder = NULL,
                    Intercept = TRUE, Size = 1,
                    tips = 0.2){
  
  require(dplyr); require(ggplot2); require(INLA); require(MCMCglmm)
  
  Graphlist<-list()
  
  if(!class(ModelList) == "list"){
    ModelList <- list(ModelList)
  }
  
  for(i in 1:length(ModelList)){
    model<-ModelList[[i]]
    
    if(class(model) == "inla"){
      Graph<-as.data.frame(summary(model)$fixed)
      colnames(Graph)[which(colnames(Graph)%in%c("0.025quant","0.975quant"))]<-c("Lower","Upper")
      colnames(Graph)[which(colnames(Graph)%in%c("0.05quant","0.95quant"))]<-c("Lower","Upper")
      colnames(Graph)[which(colnames(Graph)%in%c("mean"))]<-c("Estimate")
    }
    
    if(class(model) == "MCMCglmm"){
      Graph<-as.data.frame(summary(model)$solutions)
      colnames(Graph)[1:3]<-c("Estimate","Lower","Upper")
    }
    
    Graph$Model<-i
    Graph$Factor<-rownames(Graph)
    
    Graphlist[[i]]<-Graph
    
  }
  
  Graph <- bind_rows(Graphlist)
  
  Graph$Sig <- with(Graph, ifelse(Lower*Upper>0, "*", ""))
  
  Graph$Model <- as.factor(Graph$Model)
  
  if(!is.null(ModelNames)){
    levels(Graph$Model)<-ModelNames
  }
  
  position <- ifelse(length(unique(Graph$Model))  ==  1, "none", "right")
  
  if(is.null(VarOrder)) VarOrder <- rev(unique(Graph$Factor))
  if(is.null(VarNames)) VarNames <- VarOrder
  
  Graph$Factor <- factor(Graph$Factor, levels = VarOrder)
  levels(Graph$Factor) <- VarNames
  
  Graph %<>% as.data.frame %>% filter(!is.na(Factor))
  
  if(!Intercept){
    
    VarNames <- VarNames[!str_detect(VarNames, "ntercept")]
    
    Graph <- Graph %>% filter(Factor %in% VarNames)
    
  }
  
  Graph$starloc <- NA
  
  min<-min(Graph$Lower,na.rm = T)
  max<-max(Graph$Upper,na.rm = T)
  
  if(Sig == TRUE){
    
    Graph$starloc <- max + (max - min)/10
    
  }
  
  if(!is.null(StarLoc)){
    
    Graph$starloc <- StarLoc
    
  }
  
  Graph$Alpha <- with(Graph, ifelse(Lower*Upper>0, Alpha1, Alpha2))
  
  Graph %>%
    mutate(SigAlpha = factor(as.numeric(Lower*Upper > 0),
                             levels = c(1, 0))) ->
    
    Graph
  
  if(PointOutline){
    
    PointOutlineAlpha <- Alpha1
    
  }else{
    
    PointOutlineAlpha <- 0
    
  }
  
  ggplot(as.data.frame(Graph[!Graph$Factor=='(Intercept)',]), # remove intercept
         aes(x = reorder(as.factor(Factor),Estimate), # reorder 
             y = Estimate,
             group = Model,
             #colour = Model, #remove color
             alpha = SigAlpha)) +
    geom_point(position=position_dodge(w=0.5), size=2, stat = "identity")+
    geom_errorbar(position=position_dodge(w=0.5), aes(ymin = Lower, ymax = Upper), size=1.3, width=tips)+
    geom_hline(aes(yintercept=0),size=1, lty=2) + labs(x=NULL) + coord_flip() +
    theme_bw(base_size = 18)+
    theme(legend.position = position,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(colour="black",size=14),
          axis.text.y = element_text(colour="black"),
          axis.title.x = element_text(size=14)) +
    geom_text(aes(label = Sig, y = starloc),
              position = position_dodge(w = 0.5), size = 6,
              show.legend = F) +
    scale_alpha_manual(values = c(Alpha1, Alpha2)) +
    scale_y_continuous(labels=function(x) sprintf("%.0f", x))+ # change decimal points - 0 none, 1,2 etc
    guides(alpha = "none") +
    geom_point(colour = "black", aes(group = Model),
               position = position_dodge(w = 0.5), size = 4,
               alpha = PointOutlineAlpha) +
    scale_x_discrete(labels = c('(Intercept)'='Model\nIntercept','frtvegPrc'='Prop.\nFruits &\nVegetable','landSimp'='Landscape\nSimplification', 'frmsiz'='Farm\nSize',
                                'netincm'='Net Income\nper ha','tilminPrc'='Prop.\nConservation\nTillage',
                                'gdd'='Growing\nDegree\nDays', 'pcp'="Total\nPrecipitation", 'year'="Year", 'grainPrc'='Prop.\nCereals', 
                                'plsesPrc'='Prop.\nPulses'))+
    geom_errorbar(aes(ymin = Lower, ymax = Upper, group = Model),
                  width = 0.1,
                  position = position_dodge(w = 0.5),
                  colour = "black",
                  alpha = PointOutlineAlpha) +
    geom_point(position = position_dodge(w = 0.5), size = 6,
               alpha = PointOutlineAlpha)
  
}
#

