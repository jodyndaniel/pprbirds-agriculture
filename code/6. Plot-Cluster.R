library(ggdendro)
library(dendextend)
library(lemon)

# need to chage the numbering of the clusters from what the program gave to our new numbering
# with two vectors, I can intercange as needed
epe <- cutree(as.dendrogram(wetbirds.agnes.flex),
             order_clusters_as_data = TRUE ,k = 5)

eped <- cutree(as.dendrogram(wetbirds.agnes.flex),
             order_clusters_as_data = TRUE ,k = 5)
# change the order/group name
epe[epe==1] = 10
epe[epe==2] = 6
epe[epe==3] = 8
epe[epe==4] = 9
epe[epe==5] = 7

# sort by the new order
epedr <- sort(epe)
NGroup <- cbind.data.frame(label = names(epedr), GNN = epedr)
epedrd <- sort(eped)
RNGroup <- cbind.data.frame(label = names(epedrd), GNR = epedrd)
ALLGP <- merge(NGroup, RNGroup, by = "label" )

ALLGP <- ALLGP[order(ALLGP$GNR),]
# make it a denogram
wetlandendo <- as.dendrogram(wetbirds.agnes.flex)
# order by groups (ones named by us)
dft = rotate(wetlandendo, ALLGP$label)
# extract data
wetbirdsplotclst = dendro_data(dft,  type="rectangle")
# add group #s
wetbirdsplotclst$labels = merge(wetbirdsplotclst$labels,ALLGP, by = "label" )
wetbirdsplotclst$labels = wetbirdsplotclst$labels[order(wetbirdsplotclst$labels$GNR),]

ClusterWBirds <- 
  ggplot(wetbirdsplotclst$segments)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_point(data =  wetbirdsplotclst$labels, aes(x,y-0.015,shape = as.factor(GNN)),size = 2)+
  scale_shape_manual("Assemblage", values = c(1,8,10,4,12), label = c("Shrub Associates", 
                                                                         "Wetland Edge Nesters",
                                                                         " Hummock Nesters",
                                                                         " Shoreline Birds", 
                                                                         "Pond & Reed Associates"))+
  ylab("Bray-Curtis Dissimilarity ")+
  coord_flip() +
  theme(text=element_text(family="Tahoma", face="bold", size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(lineend = "round"),
        legend.key = element_rect(colour = NA, fill = "white"),
        legend.title.align = 0.5)+
 coord_capped_cart(left = "both")
  




png("Output/Dendogram_Wetland-Birds.png",width = 9, height = 6, units = 'in', res = 600)
plot(ClusterWBirds)
dev.off()


