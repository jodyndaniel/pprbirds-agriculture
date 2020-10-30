################################
################################
library(ggplot2)
library(plyr)
library(reshape2)
require(ggrepel)
require(extrafont)
require(grid)
require(gridExtra)
require(gtable)
library(scales)
require(viridis)
require(ggthemes)
library(RColorBrewer)
######################################################################################################
######################################################################################################

texta = paste("Species")
textb = paste("Functional Traits &\n Wetland Characteristics")
textaplot <- 
  ggplot() + 
  annotate("text", x = 0, y = 1, size = 12, label = texta, family="Tahoma", angle = 90) + 
  theme_void() 
textbplot <- 
  ggplot() + 
  annotate("text", x = 0, y = 1, size = 12, label = textb, family="Tahoma", angle = 90) + 
  theme_void() 

testlay <- rbind(c(1, 2, 3),
                 c(1, 2, 3),
                 c(1, 2, 3),
                 c(4, 5, 6),
                 c(4, 5, 6),
                 c(4, 5, 6),
                 c(NA,7,7))

testlay2 <- rbind(c(1, 2),
                 c(1, 2),
                 c(1, 2),
                 c(3))


################################################################################################
################################### WETLAND BIRDS #############################################
################################################################################################
Wet.BirdsPolanPP.1 <- 
  ggplot()+
  geom_point(data=Wet.Birds.NMDS.Scores,aes(x = NMDS1, y = NMDS2,shape= as.factor(Group)),size = 6)+
  geom_text_repel(data= Wet.Birds.Species.AX1.Vectors.1,
                  aes(x = NMDS1, y = NMDS2,label= Common.Name),family="Tahoma",size = 6,
                  box.padding = unit(0.50, "cm"),
                  point.padding = unit(0.45, "lines"))+                                      
  geom_segment(data=Wet.Birds.Species.AX1.Vectors.1,
               aes(x=0,xend = NMDS1, y=0, yend = NMDS2),
               arrow = arrow(length = unit(0.55, "cm")),size=0.6,color = "grey10",inherit.aes=TRUE)+
  scale_shape_manual("Assemblage", values = c(1,8,10,4,12), label = c("Shrub Associates", 
                                                                         "Wetland Edge Nesters",
                                                                         " Hummock Nesters",
                                                                         " Shoreline Birds", 
                                                                         "Pond & Reed Associates"))+
  scale_size_area() +
  xlab("NMDS1") +
  ylab("NMDS2")+
  labs(title = " ",
       subtitle  = "A")+ #title
  theme_special()+
  theme(legend.position="none")

Wet.BirdsPolanPP.2 <- 
  ggplot()+
  geom_point(data=Wet.Birds.NMDS.Scores,aes(x = NMDS1, y = NMDS3,shape= as.factor(Group)),size = 6)+
  geom_text_repel(data= Wet.Birds.Species.AX3.Vectors.1,
                  aes(x = NMDS1, y = NMDS3,label= Common.Name),family="Tahoma",size = 6,
                  box.padding = unit(0.50, "cm"),
                  point.padding = unit(0.45, "lines"))+                                      
  geom_segment(data=Wet.Birds.Species.AX3.Vectors.1,aes(x=0,xend = NMDS1, y=0, yend = NMDS3),
               arrow = arrow(length = unit(0.55, "cm")),size=0.6,color = "grey10",inherit.aes=TRUE)+
  scale_shape_manual("Assemblage", values = c(1,8,10,4,12), label = c("Shrub Associates", 
                                                                      "Wetland Edge Nesters",
                                                                      " Hummock Nesters",
                                                                      " Shoreline Birds", 
                                                                      "Pond & Reed Associates"))+
  scale_size_area() +
  xlab("NMDS1") +
  ylab("NMDS3")+
  labs(title = " ",
       subtitle  = "B")+ #title
  theme_special()+
  theme(legend.position="none")

Wet.BirdsPolanPP.3 <- 
  ggplot()+
  geom_point(data=Wet.Birds.NMDS.Scores,aes(x = NMDS1, y = NMDS2,shape= as.factor(Group)),
             size = 6, color = "grey5")+
  geom_text_repel(data= Wet.Birds.Functional.Abundances.Covariates.AX1.Vectors.1,
                  aes(x = NMDS1, y = NMDS2,label= stringr::str_wrap(Label,23), 
                      color = as.factor(Class_Combinded)),
                  size = 6,family="Tahoma",
                  box.padding = unit(0.50, "cm"),
                  point.padding = unit(0.45, "lines"))+                                      
  geom_segment(data=Wet.Birds.Functional.Abundances.Covariates.AX1.Vectors.1,
               aes(x=0,xend = NMDS1, y=0, yend = NMDS2, color = as.factor(Class_Combinded)),
               arrow = arrow(length = unit(0.55, "cm")),size=1.25,inherit.aes=TRUE)+
  scale_shape_manual("Assemblage", values = c(1,8,10,4,12), label = c("Shrub Associates", 
                                                                      "Wetland Edge Nesters",
                                                                      "Hummock Nesters",
                                                                      "Shoreline Birds", 
                                                                      "Pond & Reed Associates"))+
  scale_color_manual("Functional Group", values = c("grey25", "coral4"), label = c("Functional Traits", 
                                                                      "Local & Landscape Variables"))+
  scale_size_area() +
  xlab("NMDS1") +
  ylab("NMDS2")+
  labs(title = " ",
       subtitle  = "C")+ #title
  theme_special()+
  theme(legend.position="none")


Wet.BirdsPolanPP.4 <- 
  ggplot()+
  geom_point(data=Wet.Birds.NMDS.Scores,aes(x = NMDS1, y = NMDS3,shape= as.factor(Group)),size = 6)+
  geom_text_repel(data= Wet.Birds.Functional.Abundances.Covariates.AX3.Vectors.1,
                  aes(x = NMDS1, y = NMDS3,label= stringr::str_wrap(Label,23), 
                      color = as.factor(Class_Combinded)),
                  size = 6,family="Tahoma",
                  box.padding = unit(0.50, "cm"),
                  point.padding = unit(0.45, "lines"))+                                     
  geom_segment(data=Wet.Birds.Functional.Abundances.Covariates.AX3.Vectors.1,
               aes(x=0,xend = NMDS1, y=0, yend = NMDS3, color = as.factor(Class_Combinded)),
               arrow = arrow(length = unit(0.55, "cm")),size=1.25,inherit.aes=TRUE)+
  scale_shape_manual("Assemblage", values = c(1,8,10,4,12), label = c("Shrub Associates", 
                                                                      "Wetland Edge Nesters",
                                                                      " Hummock Nesters",
                                                                      " Shoreline Birds", 
                                                                      "Pond & Reed Associates"))+
  scale_color_manual("Functional Group", values = c("grey25", "coral4"), label = c("Functional Traits", 
                                                                                     "Local & Landscape Variables"))+
  scale_size_area() +
  xlab("NMDS1") +
  ylab("NMDS3")+
  labs(title = " ",
       subtitle  = "D")+ #title
  theme_special()+
  theme(legend.position="none")

WLegend <- 
  ggplot()+
  geom_point(data=Wet.Birds.NMDS.Scores,aes(x = NMDS1, y = NMDS3,shape= as.factor(Group)),size = 8)+
  geom_text_repel(data= Wet.Birds.Functional.Abundances.Covariates.AX3.Vectors.1,
                  aes(x = NMDS1, y = NMDS3,label= stringr::str_wrap(Label,23), 
                      color = as.factor(Class_Combinded)),
                  size = 4,family="Tahoma",
                  box.padding = unit(0.50, "cm"),
                  point.padding = unit(0.45, "lines"))+                                     
  geom_segment(data=Wet.Birds.Functional.Abundances.Covariates.AX3.Vectors.1,
               aes(x=0,xend = NMDS1, y=0, yend = NMDS3, color = as.factor(Class_Combinded)),
               arrow = arrow(length = unit(0.55, "cm")),size=1.25,inherit.aes=TRUE)+
  scale_shape_manual("Assemblage", values = c(1,8,10,4,12), label = c("Shrub Associates", 
                                                                      "Wetland Edge Nesters",
                                                                      " Hummock Nesters",
                                                                      " Shoreline Birds", 
                                                                      "Pond & Reed Associates"))+
  scale_color_manual("Vectors", values = c("grey25", "coral4"), label = c("Functional Traits", 
                                                                                   "Wetland Characteristics"))+
  scale_size_area() +
  xlab("NMDS1") +
  ylab("NMDS3")+
  labs(title = " ",
       subtitle  = "D")+ #title
  theme_special()+
  theme(legend.position="bottom")


mylegend2 <- g_legend(WLegend)

png("Output/NMDS_Species_Functional-Traits.png", width = 24.5, height = 19.05, units = 'in', res = 300)
grid.arrange(grobs = list(textaplot, Wet.BirdsPolanPP.1, Wet.BirdsPolanPP.2,
                          textbplot, Wet.BirdsPolanPP.3,Wet.BirdsPolanPP.4,
                          mylegend2),   
             widths = c(1,5,5),
             layout_matrix = testlay)
dev.off()


##############################################################################################

Site.Histogram.Plot <- ggplot(data = Covariets.CART, aes(x = Size))+
  geom_density(aes(y = ..scaled..), alpha = 0.15, colour = "grey50", fill = "grey50")+
  geom_histogram(aes(y = ..count.. / max(..count..)), alpha = 0.5, colour = "grey50")+
  labs(x = " Wetland Size (m)",
       y = "Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin=unit(rep(0.5, 4), "cm"),
        panel.background = element_blank(), 
        plot.title = element_text(hjust = 0,  vjust = 0,size = 22),          
        plot.subtitle = element_text(hjust = 1,  vjust = 1,size = 20),
        text=element_text(family="Tahoma", face="bold", size = 16),axis.text=element_text(size = 16),
        legend.title.align = 0.5, legend.position = "bottom")
png("Output/WetlandSize-Hist.png", width = 12, height = 10, units = 'in', res = 600)
plot(Site.Histogram.Plot)
dev.off()

############################################################################################
############################################################################################
Wet.Birds.Year.1 <- 
  ggplot(data = Wet.Birds.NMDS.Scores,aes(x = NMDS1, y = NMDS2, shape= as.factor(Year), color = as.factor(Year)))+
  geom_point(size = 6)+
  scale_shape_manual("Year", values = c(1, 10))+
  stat_ellipse(level = 0.90,geom = "polygon", fill = NA)+
  scale_color_manual("Year", values = c("grey15", "grey75"))+
  scale_size_area() +
  xlab("NMDS1") +
  ylab("NMDS2")+
  labs(title = " ",
       subtitle  = "A")+ #title
  theme_special()+
  theme(legend.position="none")

Wet.Birds.Year.2 <- 
  ggplot(data = Wet.Birds.NMDS.Scores,aes(x = NMDS1, y = NMDS3, shape= as.factor(Year), color = as.factor(Year)))+
  geom_point(size = 6)+
  scale_shape_manual("Year", values = c(1, 10))+
  stat_ellipse(level = 0.90,geom = "polygon", fill = NA)+
  scale_color_manual("Year", values = c("grey15", "grey75"))+
  scale_size_area() +
  xlab("NMDS1") +
  ylab("NMDS3")+
  labs(title = " ",
       subtitle  = "B")+ #title
  theme_special()+
  theme(legend.position="none")


WYearLegend <- 
  ggplot(data = Wet.Birds.NMDS.Scores,aes(x = NMDS1, y = NMDS3, shape= as.factor(Year), color = as.factor(Year)))+
  geom_point(size = 8)+
  scale_shape_manual("Year", values = c(1, 18))+
  stat_ellipse(level = 0.90,geom = "polygon", fill = NA)+
  scale_color_manual("Year", values = c("grey15", "grey75"))+
  scale_size_area() +
  xlab("NMDS1") +
  ylab("NMDS3")+
  labs(title = " ",
       subtitle  = "B")+ #title
  theme_special()+
  theme(legend.position="bottom")


mylegend3 <- g_legend(WYearLegend)

png("Output/NMDS_Year.png", width = 20.5, height = 12.05, units = 'in', res = 300)
grid.arrange(grobs = list(Wet.Birds.Year.1, Wet.Birds.Year.2, mylegend3),   
             widths = c(5,5),
             layout_matrix = testlay2)
dev.off()
