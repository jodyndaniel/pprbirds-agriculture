#######################################################################################################
###################################### THEME FOR PLOTTING #############################################
#######################################################################################################
theme_special <- function(){
  font <- "Tahoma"
  theme_classic() %+replace%
    
    theme(panel.background = element_rect(color = "black", fill = NA, size = 1),
          axis.title.y = element_text(angle = 90, size = 20, margin = margin(t = 0, r = 25, b = 0, l = 0)),
          axis.title.x = element_text( size = 20,margin = margin(t = 25, r = 0, b = 0, l = 0)),
          plot.title = element_text(face="bold",hjust = 0,  vjust = 0,size = 25),          
          plot.subtitle = element_text(face="bold", hjust = 1,  vjust = 1,size = 20),
          text=element_text(size = 16, family= font),
          axis.text=element_text(size = 16),
          legend.title.align = 0.5)
}


#######################################################################################################
############################################ LEGEND ###################################################
#######################################################################################################

# We use this function to extract the legend from figures
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#######################################################################################################
######################################### AXES FOR NMDS ###############################################
#######################################################################################################

NMDS.scree<-function(x) { #where x is the name of the data frame variable
  plot(rep(1,50),replicate(50,
                           metaMDS(x,autotransform=T,trymax=100,k=1)$stress),
       xlim=c(1,10),ylim=c(0,0.5),xlab="# of Dimensions",ylab="Stress",
       main="MDS stress plot")
  
  for (i in 1:10) {
    points(rep(i+1,50),replicate(50,metaMDS(x,autotransform=T,trymax=100,k=i+1)$stress))
  }
}
# 
NMDS.screePoints<-function(x,y) { #where x is the name of the data frame variable
  runr = matrix(NA, nrow = 10, ncol = y)
  for (i in 1:10) {
    runr[i,] = replicate(y, metaMDS(x,autotransform=T,trymax=100,k=i)$stress)
    
  }
  return(runr)
}
#######################################################################################################
########################## PROPORTION DATA TRANSFORMATION #############################################
#######################################################################################################

trans.arcsine <- function(x){
  abs(asin(sign(x)) * sqrt(abs(x)))
}
#######################################################################################################
###################################### SCALE FOR PLOTTING #############################################
#######################################################################################################

scaleFUN <- function(x) sprintf("%.1f", x)


#######################################################################################################
########################################## PRUNE CART #################################################
#######################################################################################################

PruneTreeDeviance <- function(x,y,z){
  trep <- replicate(x,{
    Devi <- matrix(cv.tree(y,FUN = prune.misclass)$dev/z,byrow = TRUE)
  })
  rd <- apply(trep,1:2, mean)*100
  rder <- apply(trep,1:2, std.error)*100
  rderf <- cbind(rd,rder)
  rownames(rderf) <- cv.tree(y,FUN = prune.misclass)$size
  colnames(rderf) <- c("Mean", "Standard_Error")
  return(rderf)
}