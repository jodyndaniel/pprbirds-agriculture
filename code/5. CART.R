##################################################################
##################################################################
library(tree) 
library(caret)
library(maptree)
require(rattle)
library(RColorBrewer)
library(diversity)
library(vegan)
library(vegetarian)
library(plotrix)
library(RVAideMemoire)
library(DescTools)

###################################################################
Wet.Covariets.CART.GP <- Wet.Covariets.CART

# give a new clustering number based on wetland affinity
Wet.Covariets.CART.GP$Group[wetncluster.bd.isa[[5]] == 1] <- 5 # 8
Wet.Covariets.CART.GP$Group[wetncluster.bd.isa[[5]] == 2] <- 1 # 7
Wet.Covariets.CART.GP$Group[wetncluster.bd.isa[[5]]== 3] <- 3 # 10
Wet.Covariets.CART.GP$Group[wetncluster.bd.isa[[5]] == 4] <- 4 # 11
Wet.Covariets.CART.GP$Group[wetncluster.bd.isa[[5]] == 5] <- 2 # 12

# after running the CART, we will need to prune the tree (using a function we made)
# selecting where we see a drop in error as the number of nodes we should prune at
WETBirds.CART.Raw <- tree(as.factor(Group) ~ .,
                       data = Wet.Covariets.CART.GP, split = "deviance")

# examine the error based on the number of nods
Prune.WEB <- PruneTreeDeviance(x=100,y=WETBirds.CART.Raw,z = 64)
plot(rownames(Prune.WEB), Prune.WEB[,1], type="b") # 8 seems optimal
# now we can prune
WETBirds.CART.PR <- prune.tree(WETBirds.CART.Raw, best = 8)
# pull out tree staistics 
sum(sapply(resid(WETBirds.CART.PR),function(x)(x-mean(resid(WETBirds.CART.PR)))^2))
sum(sapply(resid(WETBirds.CART.Raw),function(x)(x-mean(resid(WETBirds.CART.Raw)))^2))
# visualize the final tree and save
draw.tree(WETBirds.CART.Raw,size = 3, digits = 3,nodeinfo = TRUE,print.levels=FALSE)
draw.tree(WETBirds.CART.PR,size = 3, digits = 3,nodeinfo = TRUE,print.levels=FALSE)
png("Output/WETBCART.png",width = 7, height = 7,units = 'in', res = 600)
draw.tree(WETBirds.CART.PR,size = 3, digits = 3,nodeinfo = TRUE,print.levels=FALSE)
dev.off()


# Now we need to examine the classification error rates for the tree
# is their a large difference in what the tree predicts and what the data suggests?
WetBirds.CART.CM <- data.frame(Group = as.factor(predict(WETBirds.CART.PR,Wet.Covariets.CART.GP,type="class")))
# a confusion matrix could be helpful 
confusionMatrix(WetBirds.CART.CM$Group,as.factor(Wet.Covariets.CART.GP$Group))

WCMatrix <- cbind(Prediction = predict(WETBirds.CART.PR,Wet.Covariets.CART.GP,type="class"), 
                Group = Wet.Covariets.CART.GP$Group) # kappa is fair, but not great
# a g-test could help in figuring out if the group membership frequency 
# differences between the predictions and observations
GTest(WCMatrix, correct = "williams")
# no differences

WETNODE <- data.frame(Node = WETBirds.CART.PR$where,# node membership of each site
                      Group_Predicted = Wet.Covariets.CART.GP$Group, # assemblage they were predicted to belong to  
                      Group_Observed = WetBirds.CART.CM$Group)
# based on an examination of WETBirds.CART.PR$frame, and WETBirds.CART.PR$where
# we are able to tell which node each site was predicted to 
# this, we could use to work back the error rates in each node
# the actual assemblage they belong to 
# and site covariates (e.g., Region, Disturbance Class)
# if an assemblage has more than one terminal node, the node on the left is listed as
# A and the one to the right as B - this is based on the visualized tree
WETNODE$Class[WETNODE$Node == 5] = "3A"
WETNODE$Class[WETNODE$Node == 6] = 2
WETNODE$Class[WETNODE$Node == 7] = "4A"
WETNODE$Class[WETNODE$Node == 8] = "4B"
WETNODE$Class[WETNODE$Node == 12] = 1
WETNODE$Class[WETNODE$Node == 13] = "5A"
WETNODE$Class[WETNODE$Node == 14] = "3B"
WETNODE$Class[WETNODE$Node == 15] = "5B"


WETNODEF <- data.frame(WETNODE,
                       Disturb = LandCover.Polan[WetEmptyISA,"Disturb"],
                       Region = Wet.Covariets.CART.GP$Region, 
                       Permanence = Wet.Covariets.CART.GP$Permanence )
######################################################################################################################
######################################################################################################################
######################################################################################################################
# Now, we can create a table of site covariates - mean and standard error for each predicted group
CART.Table.WetMean <- matrix(data=NA,nrow=15,ncol=5)
CART.Table.WetError <- matrix(data=NA,nrow=15,ncol=5)
# covariates that are numeric  
for (j in 4:13){ 
    red <- aggregate(Wet.Covariets.CART.GP[,j]~ Group, Wet.Covariets.CART.GP, mean)
    redb <- aggregate(Wet.Covariets.CART.GP[,j]~ Group, Wet.Covariets.CART.GP, std.error)
    CART.Table.WetMean[j,] <- red[,2]
    CART.Table.WetError[j,] <- redb[,2]
   
}
# add rownames
rownames(CART.Table.WetMean) <- colnames(Wet.Covariets.CART.GP)
rownames(CART.Table.WetError) <- colnames(Wet.Covariets.CART.GP)
# categorical covariates
table(WETNODEF$Class,WETNODEF$Region)
table(WETNODEF$Class,WETNODEF$Permanence)
table(WETNODEF$Class,WETNODEF$Disturb)
table(WETNODEF$Class, WETNODEF$Group_Observed)


