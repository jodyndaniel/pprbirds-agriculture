#####################################################
#####################################################
########## Extracting Functional Traits #############
#####################################################
#####################################################
library(readxl)
Bird.Traits <- data.frame(read_xlsx("Input/Traits.xlsx", sheet = 1))
Local.Landscape.Names <- data.frame(read_xlsx("Input/Community-Sites-Landscape.xlsx", sheet = 6, col_names = TRUE))
Bird.Trait.Names <- data.frame(read_xlsx("Input/Traits.xlsx", sheet = 4))

# find the sum abundnaces of birds belong to each trait group
trait.list <- colnames(Bird.Traits)[-1]
WBirds.Functional.Abundances <- matrix(NA, nrow = nrow(Wet.Birds.MatrixNMSF), 
                                       ncol = length(trait.list))
for (i in 1:ncol(Bird.Traits)){
  trait.members <- which(colnames(Wet.Birds.MatrixNMSF)%in%Bird.Traits$Code[Bird.Traits[, trait.list[i]]==1])
  if (length(trait.members)==1){
    WBirds.Functional.Abundances[,i] <- Wet.Birds.MatrixNMSF[, trait.members]
  }
  else{
    WBirds.Functional.Abundances[,i] <- rowSums(Wet.Birds.MatrixNMSF[, trait.members])}
  }
colnames(WBirds.Functional.Abundances) <- trait.list

#####################################################
#####################################################
######## Nonmetric MultiDimensional Scaling #########
#####################################################
#####################################################
library(vegan)
library(goeveg)
library(tidyverse)
#####################################################
#####################################################

# function for optimal number of axes
WETBScreePlot <- NMDS.screePoints(Wet.Birds.MatrixNMS,50) #
plot(1:10, WETBScreePlot[,1])

# save the mean and error
WETScreeP <- data.frame(X = 1:10,
                      Mean = apply(WETBScreePlot, 1, function(x) mean(x)),
                      STDER = apply(WETBScreePlot, 1, function(x) std.error(x)))

########################
#### WETLAND BIRDS ####
########################

# Now, let's try the NMDS
(Wet.Birds.NMDS <- metaMDS(Wet.Birds.MatrixNMS,distance = "bray",
                           k = 3, autotransform=TRUE,trymax=100))

# Distance: bray 

# Dimensions: 3 
# Stress:    0.1857649    
# Stress type 1, weak ties
# Two convergent solutions found after 20 tries
# Scaling: centring, PC rotation, halfchange scaling 
# Species: expanded scores based on 'Wet.Birds.MatrixNMS
###### ################ Species Vectors  ###############################

################################################################################################################
##############################################Species Vectors ####################################################
################################################################################################################

Bird.Names <- data.frame(read_xlsx("Input/CommunityNames.xlsx", sheet = 1, col_names = TRUE))

### Axis 1 #####
################################################################################################################
################################################################################################################

# Extract vector scores based above a correlation above 0.2
Wet.Birds.Species.AX1.Vectors <- data.frame(scores(envfit(Wet.Birds.NMDS,# extract species scores and fit data
                                                   Wet.Birds.MatrixISA, # fit data based on the abundances of species
                                                   permutations = 999), # no more than these permutations
                                                       "vectors")) # extract vectors and not scores
# need to make a column that will these vectors to the orignal data
Wet.Birds.Species.AX1.Vectors$Code <- row.names(Wet.Birds.Species.AX1.Vectors)
# save the vector correlations
Wet.Birds.Species.AX1.Correlation <- data.frame(envfit(Wet.Birds.NMDS,# extract species scores and fit data
                                                       Wet.Birds.MatrixISA, # fit data based on the abundances of species
                                                           permutations = 999)$vectors[2])
# need a column to base the merge so that we could only pull out vectors of the value we want
Wet.Birds.Species.AX1.Correlation$Code <- row.names(Wet.Birds.Species.AX1.Correlation)
# which vectors have correlation greater  than 0.1?
Wet.Birds.Species.AX1.Correlation.1 <- Wet.Birds.Species.AX1.Correlation[which(Wet.Birds.Species.AX1.Correlation$r>0.1),]
# Let's select thw vectors we want plus get their labels
Wet.Birds.Species.AX1.Vectors.1 <- 
  list(Wet.Birds.Species.AX1.Correlation.1, Wet.Birds.Species.AX1.Vectors, Bird.Names) %>% 
  reduce(left_join, by = "Code")

###################################################################################################################
# Repeat for Axis 3
###################################################################################################################

# Extract vector scores based above a correlation above 0.1
Wet.Birds.Species.AX3.Vectors <- data.frame(scores(envfit(Wet.Birds.NMDS,# extract species scores and fit data
                                                            Wet.Birds.MatrixISA, # fit data based on the abundnaces of species
                                                              permutations = 999, # no more than these permutations
                                                              choices = c(1,3)),# select axis 1 and 3
                                                       "vectors")) # exract vectors and not scores
# need to make a column that will these vectors to the orignal data
Wet.Birds.Species.AX3.Vectors$Code <- row.names(Wet.Birds.Species.AX3.Vectors)
Wet.Birds.Species.AX3.Correlation <- data.frame(envfit(Wet.Birds.NMDS,# extract species scores and fit data
                                                        Wet.Birds.MatrixISA, # fit data based on the abundnaces of species
                                                           choices = c(1,3),# select axis 1 and 3,
                                                           permutations = 999)$vectors[2])
# need a column to base the merge so that we could only pull out vectors of the value we want
Wet.Birds.Species.AX3.Correlation$Code <- row.names(Wet.Birds.Species.AX3.Correlation)
# which vectors have correlation greater  than 0.1?
Wet.Birds.Species.AX3.Correlation.1 <- Wet.Birds.Species.AX3.Correlation[which(Wet.Birds.Species.AX3.Correlation$r>0.1),]
# Let's select thw vectors we want plus get their labels
Wet.Birds.Species.AX3.Vectors.1 <- 
  list(Wet.Birds.Species.AX3.Correlation.1, 
       Wet.Birds.Species.AX3.Vectors, 
       Bird.Names) %>%  
  reduce(left_join, by = "Code")



############################################################################################
############################################################################################
############################################################################################

# Extract vector scores based above a correlation above 0.2
Wet.Birds.Functional.Abundances.AX1.Vectors <- data.frame(scores(envfit(Wet.Birds.NMDS,# extract Functional.Abundances scores and fit data
                                                                        WBirds.Functional.Abundances, # fit data based on the abundances of Functional.Abundances
                                                                        permutations = 999), # no more than these permutations
                                                                 "vectors")) # extract vectors and not scores
# need to make a column that will these vectors to the orignal data
Wet.Birds.Functional.Abundances.AX1.Vectors$Code <- row.names(Wet.Birds.Functional.Abundances.AX1.Vectors)
# save the vector correlations
Wet.Birds.Functional.Abundances.AX1.Correlation <- data.frame(envfit(Wet.Birds.NMDS,# extract Functional.Abundances scores and fit data
                                                                     WBirds.Functional.Abundances, # fit data based on the abundances of Functional.Abundances
                                                                     permutations = 999)$vectors[2])
# need a column to base the merge so that we could only pull out vectors of the value we want
Wet.Birds.Functional.Abundances.AX1.Correlation$Code <- row.names(Wet.Birds.Functional.Abundances.AX1.Correlation)
# which vectors have correlation greater  than 0.1?
Wet.Birds.Functional.Abundances.AX1.Correlation.1 <- Wet.Birds.Functional.Abundances.AX1.Correlation[which(Wet.Birds.Functional.Abundances.AX1.Correlation$r>0.1),]
# Let's select thw vectors we want plus get their labels
Wet.Birds.Functional.Abundances.AX1.Vectors.1 <- 
  list(Wet.Birds.Functional.Abundances.AX1.Correlation.1, 
       Wet.Birds.Functional.Abundances.AX1.Vectors, 
       Bird.Trait.Names) %>% 
  reduce(left_join, by = "Code")

###################################################################################################################
# Repeat for Axis 3
###################################################################################################################

# Extract vector scores based above a correlation above 0.1
Wet.Birds.Functional.Abundances.AX3.Vectors <- data.frame(scores(envfit(Wet.Birds.NMDS,# extract Functional.Abundances scores and fit data
                                                                        WBirds.Functional.Abundances, # fit data based on the abundnaces of Functional.Abundances
                                                                        permutations = 999, # no more than these permutations
                                                                        choices = c(1,3)),# select axis 1 and 3
                                                                 "vectors")) # exract vectors and not scores
# need to make a column that will these vectors to the orignal data
Wet.Birds.Functional.Abundances.AX3.Vectors$Code <- row.names(Wet.Birds.Functional.Abundances.AX3.Vectors)
Wet.Birds.Functional.Abundances.AX3.Correlation <- data.frame(envfit(Wet.Birds.NMDS,# extract Functional.Abundances scores and fit data
                                                                     WBirds.Functional.Abundances, # fit data based on the abundnaces of Functional.Abundances
                                                                     choices = c(1,3),# select axis 1 and 3,
                                                                     permutations = 999)$vectors[2])
# need a column to base the merge so that we could only pull out vectors of the value we want
Wet.Birds.Functional.Abundances.AX3.Correlation$Code <- row.names(Wet.Birds.Functional.Abundances.AX3.Correlation)
# which vectors have correlation greater  than 0.1?
Wet.Birds.Functional.Abundances.AX3.Correlation.1 <- Wet.Birds.Functional.Abundances.AX3.Correlation[which(Wet.Birds.Functional.Abundances.AX3.Correlation$r>0.1),]
# Let's select thw vectors we want plus get their labels
Wet.Birds.Functional.Abundances.AX3.Vectors.1 <- 
  list(Wet.Birds.Functional.Abundances.AX3.Correlation.1, 
       Wet.Birds.Functional.Abundances.AX3.Vectors, 
       Bird.Trait.Names) %>%  
  reduce(left_join, by = "Code")

############################################################################################
############################################################################################
############################################################################################

# Extract vector scores based above a correlation above 0.2
Wet.Birds.Covariets.AX1.Vectors <- data.frame(scores(envfit(Wet.Birds.NMDS,# extract Covariets scores and fit data
                                                            Wet.Covariets.NMS, # fit data based on the abundances of Covariets
                                                            permutations = 999), # no more than these permutations
                                                     "vectors")) # extract vectors and not scores
# need to make a column that will these vectors to the orignal data
Wet.Birds.Covariets.AX1.Vectors$Code <- row.names(Wet.Birds.Covariets.AX1.Vectors)
# save the vector correlations
Wet.Birds.Covariets.AX1.Correlation <- data.frame(envfit(Wet.Birds.NMDS,# extract Covariets scores and fit data
                                                         Wet.Covariets.NMS, # fit data based on the abundances of Covariets
                                                         permutations = 999)$vectors[2])
# need a column to base the merge so that we could only pull out vectors of the value we want
Wet.Birds.Covariets.AX1.Correlation$Code <- row.names(Wet.Birds.Covariets.AX1.Correlation)
# which vectors have correlation greater  than 0.1?
Wet.Birds.Covariets.AX1.Correlation.1 <- Wet.Birds.Covariets.AX1.Correlation[which(Wet.Birds.Covariets.AX1.Correlation$r>0.1),]
# Let's select thw vectors we want plus get their labels
Wet.Birds.Covariets.AX1.Vectors.1 <- 
  list(Wet.Birds.Covariets.AX1.Correlation.1, 
       Wet.Birds.Covariets.AX1.Vectors, 
       Local.Landscape.Names) %>% 
  reduce(left_join, by = "Code")

###################################################################################################################
# Repeat for Axis 3
###################################################################################################################

# Extract vector scores based above a correlation above 0.1
Wet.Birds.Covariets.AX3.Vectors <- data.frame(scores(envfit(Wet.Birds.NMDS,# extract Covariets scores and fit data
                                                            Wet.Covariets.NMS, # fit data based on the abundnaces of Covariets
                                                            permutations = 999, # no more than these permutations
                                                            choices = c(1,3)),# select axis 1 and 3
                                                     "vectors")) # exract vectors and not scores
# need to make a column that will these vectors to the orignal data
Wet.Birds.Covariets.AX3.Vectors$Code <- row.names(Wet.Birds.Covariets.AX3.Vectors)
Wet.Birds.Covariets.AX3.Correlation <- data.frame(envfit(Wet.Birds.NMDS,# extract Covariets scores and fit data
                                                         Wet.Covariets.NMS, # fit data based on the abundnaces of Covariets
                                                         choices = c(1,3),# select axis 1 and 3,
                                                         permutations = 999)$vectors[2])
# need a column to base the merge so that we could only pull out vectors of the value we want
Wet.Birds.Covariets.AX3.Correlation$Code <- row.names(Wet.Birds.Covariets.AX3.Correlation)
# which vectors have correlation greater  than 0.1?
Wet.Birds.Covariets.AX3.Correlation.1 <- Wet.Birds.Covariets.AX3.Correlation[which(Wet.Birds.Covariets.AX3.Correlation$r>0.1),]
# Let's select the vectors we want plus get their labels
Wet.Birds.Covariets.AX3.Vectors.1 <- 
  list(Wet.Birds.Covariets.AX3.Correlation.1, 
       Wet.Birds.Covariets.AX3.Vectors, 
       Local.Landscape.Names) %>%  
  reduce(left_join, by = "Code")
############################################################################################
############################################################################################

Wet.Birds.Functional.Abundances.Covariates.AX1.Vectors.1 <-
  cbind(rbind(Wet.Birds.Functional.Abundances.AX1.Vectors.1,
              Wet.Birds.Covariets.AX1.Vectors.1), 
        Class_Combinded = c(rep(1,nrow(Wet.Birds.Functional.Abundances.AX1.Vectors.1)),
                            rep(2,nrow(Wet.Birds.Covariets.AX1.Vectors.1))))


Wet.Birds.Functional.Abundances.Covariates.AX3.Vectors.1 <-
  cbind(rbind(Wet.Birds.Functional.Abundances.AX3.Vectors.1,
              Wet.Birds.Covariets.AX3.Vectors.1), 
        Class_Combinded = c(rep(1,nrow(Wet.Birds.Functional.Abundances.AX3.Vectors.1)),
                            rep(2,nrow(Wet.Birds.Covariets.AX3.Vectors.1))))
############################################################################################
Wet.Birds.NMDS.Scores <- data.frame(scores(Wet.Birds.NMDS))
Wet.Birds.NMDS.Scores$SiteNo <- row.names(Wet.Birds.NMDS.Scores)
Wet.Birds.NMDS.Scores$GN <- wetncluster.bd.isa[[5]]

Wet.Birds.NMDS.Scores$Group[Wet.Birds.NMDS.Scores$GN==1] <- 5
Wet.Birds.NMDS.Scores$Group[Wet.Birds.NMDS.Scores$GN==2] <- 1
Wet.Birds.NMDS.Scores$Group[Wet.Birds.NMDS.Scores$GN==3] <- 3
Wet.Birds.NMDS.Scores$Group[Wet.Birds.NMDS.Scores$GN==4] <- 4
Wet.Birds.NMDS.Scores$Group[Wet.Birds.NMDS.Scores$GN==5] <- 2


Birds.Polan$SiteNo <- as.character(Birds.Polan$SiteNo)
Wet.Birds.NMDS.Scores$SiteNo <- as.character(Wet.Birds.NMDS.Scores$SiteNo)
SiteInfo.Raw$SiteNo <- as.character(SiteInfo.Raw$SiteNo)
Wet.Birds.NMDS.Scores <-
  list(Wet.Birds.NMDS.Scores, 
       Birds.Polan[,1:2], 
       SiteInfo.Raw[,1:8]) %>%
  reduce(left_join, by = "SiteNo")












