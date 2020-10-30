#####################################################
################# Data Preparation ##################
#####################################################

# First, I will need to import the data.
# Then, I will attempt to create new dataframes,
# based on traits - obligate, faculative wet, and faculative dry.
####### Importing Data ######

### Next, I will import the data ###

# Importing file raw bird data #
library(readxl)
library(vegan)
library(vegetarian)
library(plotrix)
library(here)

source(here("Code/1. Functions.R"))
Birds.Raw <- data.frame(read_xlsx("Input/Community-Sites-Landscape.xlsx", sheet = 1, col_names = TRUE))
Birds.Raw <- Birds.Raw[order(Birds.Raw$SiteNo), ]
Plants.Raw <- read_xlsx("Input/Community-Sites-Landscape.xlsx", sheet = 2, col_names = TRUE)
Plants.Raw <- Plants.Raw[order(Plants.Raw$SiteNo), ]
LandCover.Raw <-  read_xlsx("Input/Community-Sites-Landscape.xlsx", sheet = 3, col_names = TRUE)
LandCover.Raw <- LandCover.Raw[order(LandCover.Raw$SiteNo), ]
WaterChem.Raw <- read_xlsx("Input/Community-Sites-Landscape.xlsx", sheet = 4, col_names = TRUE)
WaterChem.Raw <- WaterChem.Raw[order(WaterChem.Raw$SiteNo), ]
SiteInfo.Raw <- as.data.frame(read_xlsx("Input/Community-Sites-Landscape.xlsx", sheet = 5, col_names = TRUE))

Bird.Trait <- as.data.frame(read_xlsx("Input/Traits.xlsx", sheet = 1, col_names = TRUE))
Plant.Trait <- as.data.frame(read_xlsx("Input/Traits.xlsx", sheet = 2, col_names = TRUE))

################################################################################################################
################################################## Prep Covariates #############################################
################################################################################################################

# apply an arc sine transformation on plant data for NMS
Plants.Tran <- cbind.data.frame(Plants.Raw[,1:2],
                                     trans.arcsine(Plants.Raw[,3:ncol(Plants.Raw)]))

LandCover.CART <- cbind.data.frame(SiteNo = LandCover.Raw$SiteNo,Region = LandCover.Raw$Region,
                                        Permanence = WPermenance$Permanence,
                                        NonNatural= LandCover.Raw$NonNatural,
                                        Cropland= LandCover.Raw$Cropland,
                                        UrbanExposed = rowSums(cbind(LandCover.Raw$UrbanDeveloped,
                                                                     LandCover.Raw$`Exposed Land/Barren`)),
                                        PastureForage = LandCover.Raw$PastureForages,
                                        WaterWet = rowSums(cbind(LandCover.Raw$Water,
                                                                 LandCover.Raw$Wetland)),
                                        ForestShrub = rowSums(cbind(LandCover.Raw$`Coniferous Forest`,
                                                                    LandCover.Raw$`Deciduous Forest`,
                                                                    LandCover.Raw$`Mixed Forest`,
                                                                    LandCover.Raw$Shrubland)))


LandCover.NMS <- data.frame(SiteNo = LandCover.Raw$SiteNo,Region = LandCover.Raw$Region,
                                  Permanence = WPermenance$Permanence,
                                  trans.arcsine(LandCover.CART[,4:ncol(LandCover.CART)]))

WaterChem.CART <- cbind.data.frame(SiteNo = WaterChem.Raw$SiteNo, 
                                   MaxDepth = WaterChem.Raw$`Max Depth`)

WaterChem.CART <- merge(WaterChem.CART,SiteInfo.Raw[,1:8], by = "SiteNo" )[,-4][,-4][,-4]
WaterChem.CART <- WaterChem.CART[,1:2]
################################################################################################################
########################################### Prep Bird Data #######################################
################################################################################################################

Birds.Matrix <- Birds.Raw[, 3:ncol(Birds.Raw)]
####################################################################
################# Extract Wetland Birds ############################
####################################################################

### Which birds are obligates? ###
# column number of obligates
ob <- which(Bird.Trait$BirdCode=="Oblig")

# names of obligates
Obli <- c(colnames(Bird.Trait)[which(Bird.Trait[ob,]==1,arr.ind=FALSE)])

#find the column numbers for the obligates in the dataframe for the CART
ObligCART <- which(colnames(Birds.Matrix)%in% Obli)

################ Which Birds are Facultative? ###############
# column number of facultives
fa <- which(Bird.Trait$BirdCode=="Facul")

# names of obligates
Facu <- c(colnames(Bird.Trait)[which(Bird.Trait[fa,]==1,arr.ind=FALSE)])

#find the column numbers for the obligates in the dataframe for the CART
FaculCART <- which(colnames(Birds.Matrix)%in% Facu)


################ Which Birds are Facultative Wet? ###############
# column number of facultives
fawe <- which(Bird.Trait$BirdCode=="FaculWet")

# names of obligates
FacuWe <- c(colnames(Bird.Trait)[which(Bird.Trait[fawe,]==1,arr.ind=FALSE)])

#find the column numbers for the obligates in the dataframe for the CART
FaculWeCART <- which(colnames(Birds.Matrix)%in% FacuWe)


########## Now, we have grouped birds based on their traits ########
# Since this will be binary dataset, lets further codify the data
# We will create a new variable for each assemblage

WetBirds.FacWe <- Birds.Matrix[,FaculWeCART]
WetBirds.Facul <- Birds.Matrix[,FaculCART]
WetBirds.Obi <- Birds.Matrix[,ObligCART]

Wet.Birds.Matrix <- cbind.data.frame(WetBirds.FacWe,
                                     WetBirds.Facul,
                                     WetBirds.Obi)
# remove empty rows
WETBirds.Polan.PA <- Wet.Birds.Matrix
WETBirds.Polan.PA[WETBirds.Polan.PA>1] <- 1
WetEmptyISA <- which(rowSums(WETBirds.Polan.PA)>1)

# transform data and add site numbers as rownames for ISA and NMS
Wet.Birds.MatrixISA <- decostand(sqrt(Wet.Birds.Matrix[WetEmptyISA,]),method = "max")
rownames(Wet.Birds.MatrixISA) <- LandCover.Polan[WetEmptyISA,1]
Wet.Birds.MatrixNMS <- decostand(sqrt(Wet.Birds.Matrix[WetEmptyISA,]), method = "max")
rownames(Wet.Birds.MatrixNMS) <- LandCover.Polan[WetEmptyISA,1]
Wet.Birds.MatrixNMSF <- Wet.Birds.Matrix[WetEmptyISA,]
################################################################################################################
########################################### Plant Cover @ Sites ################################################
################################################################################################################



# Which plants are in the robust emergent group?
r <- which(Plant.Trait$Code=="Robust_Emergent")
RoEm <- c(colnames(Plant.Trait)[which(Plant.Trait[r,]==1,arr.ind=FALSE)])

#find the column numbers for the robust emergent group
#in the dataframe for the CART
RobEme <- which(colnames(Plants.Raw)%in% RoEm)

# Which plants are in the woody group?
w <- which(Plant.Trait$Code=="Hardwood")
Hard <- c(colnames(Plant.Trait)[which(Plant.Trait[w,]==1,arr.ind=FALSE)])

#find the column numbers for the woody group
#in the dataframe for the CART
Wood <- which(colnames(Plants.Raw)%in% Hard)

# Which plants are in the BroadLeaf Emergent?
b <- which(Plant.Trait$Code=="BroadLeaf_Emergent")
Broad <- c(colnames(Plant.Trait)[which(Plant.Trait[b,]==1,arr.ind=FALSE)])

#find the column numbers for the BroadLeaf Emergent group
#in the dataframe for the CART
BroLea <- which(colnames(Plants.Raw)%in% Broad)

# First, let's create a new variable, one for each plant trait
# This vairble is created by using the column number of the plants within the group
PlantTraitCover <- data.frame(SiteNo = Plants.Raw$SiteNo,
                                 BroadLeaf = rowSums(Plants.Raw[,BroLea]),
                                 Woody = rowSums(Plants.Raw[,Wood]),
                                 RobEmerge = rowSums(Plants.Raw[,RobEme]))


PlantTraitCover.NMS <- data.frame(SiteNo = Plants.Raw$SiteNo,
                                 trans.arcsine(PlantTraitCover[,2:ncol(PlantTraitCover)]))
################################################################################################################
################################################ Covariates NMS + CART #######################################
################################################################################################################

Wet.Covariets.CART <- merge(merge(merge(merge(LandCover.CART,WaterChem.CART, by = "SiteNo"),
                        PlantTraitCover, by = "SiteNo"),SiteInfo.Raw[,c("SiteNo", "Size")],
                        by = "SiteNo" ), Birds.Raw[,c(1:2)], by = "SiteNo")[WetEmptyISA,]

rownames(Wet.Covariets.CART) = Wet.Covariets.CART$SiteNo
Wet.Covariets.CART <- Wet.Covariets.CART[,-1]


Wet.Covariets.NMS <- merge(merge(merge(LandCover.CART,WaterChem.CART, by = "SiteNo"),
                              PlantTraitCover.NMS, by = "SiteNo"),SiteInfo.Raw[, c("SiteNo", "Size")], 
                        by = "SiteNo" )[WetEmptyISA,-c(2,3)]

rownames(Wet.Covariets.NMS) = Wet.Covariets.NMS$SiteNo
Wet.Covariets.NMS <- Wet.Covariets.NMS[,-1]







