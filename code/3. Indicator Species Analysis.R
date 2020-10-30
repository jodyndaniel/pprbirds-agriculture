#####################################################
#####################################################
##### Indiator Species Analysis ####
#####################################################
#####################################################

library(indicspecies)
library(stats)
library(vegan)
library(cluster)
library(labdsv)
library(NbClust)
library(factoextra)
library(bindr)
library(clusterSim)
library(xtable)
# the count data can be influenced by sample size, where some sites have fewer species
# and it can also be influenced by instances when some species are rare
# so, we will transform the data

#################################################################################################################
#################################################################################################################
# first, let's is build the clusters
# All birds
# the function "hclust" can use any didtance matrix type, but we must create it externally
# all birds#

WetBD.Bray.Flex <- vegdist(Wet.Birds.MatrixISA)# data is sqrt transformed 

# Next, we run the cluster anlysis, we will use the flexible method
set.seed(123)
(wetbirds.agnes.flex <- agnes(WetBD.Bray.Flex, diss=TRUE, method = "flexible",
                              par.method=c(0.625, 0.625, -0.25))) # 0.7191624   

###################################################
###### Find optimal number of clusters ############
###################################################
# there are two ways
# we can use "pam" and increase the number of clusters from 2 to 20
# then, I can find the average silohete value across all attempts
# the one that's the highest is the best one

wetncluster.bd.isa <-vector("list",1)
for (i in 2:20){
  wetncluster.bd.isa[[i]] <- cutree(as.hclust(wetbirds.agnes.flex),k=i)
}
#################################################################################################################
#################################################################################################################
### R was giving strange results when I ran the cluster analsysis with flexible beta method
# the results of Ward with a wiscon transformation did match what I got in PCORD
# for safety reasons, I ran the cluster analsysis in PCORD
# and then I will export the group memberships and run the ISA
# I will use multipatt vs indval because it is based on the updated method
# one that compares site group combinations versus just the orignal site groups


# what are the correct number of clusters?

# first, we will check for cluster numbers from 2 the when there are atleast 2 sites per group
# which groupings have only one site per group?
wetsitecluster = matrix(NA, nrow = nrow(Wet.Birds.MatrixISA), ncol=20)
wetsiteclusterlist = vector("list",1)
for (i in 2:20){
  wetsitecluster[,i] = as.factor(wetncluster.bd.isa[[i]])
  wetsiteclusterlist[[i]] = table(wetsitecluster[,i])
}

WETBSigISA = ISA.Sig(x=Wet.Birds.MatrixISA,y=wetncluster.bd.isa, z=2,r=8) # 5

# let's try the same thing, but instead, we will find the number of sigificant indictaors
# then we will count them up
ISA.SigIndicators = function(x,y,z,r){ # x is the matrix, y is the cluster matrix, y is the lower limit, z is the higher limit
  ncluster.bd.isa.sig <- vector("list",1)
  for (i in z:r){
    ncluster.bd.isa.sig[[i]] <-multipatt(x,y[[i]], control = how(nperm=999),
                                         duleg=FALSE, print.perm = FALSE)$sign[,i+3]# run model and pull out p values
    ncluster.bd.isa.sig[[i]][ncluster.bd.isa.sig[[i]]>=0.05] = NA # non sigificant pvalues should be made NA
    ncluster.bd.isa.sig[[i]][ncluster.bd.isa.sig[[i]]<0.05] = 1 # siginifint ones should be made 1
    ncluster.bd.isa.sig[[i]] = sum(ncluster.bd.isa.sig[[i]],na.rm = TRUE) # find the number of indicators
  }
  return(ncluster.bd.isa.sig)} # return list

WETBNogroups = ISA.SigIndicators(x=Wet.Birds.MatrixISA,y=wetncluster.bd.isa, z=2,r=8) #5 or 7



######## Try with indval
# its is aa bit faster

ISA.SigIndicatorsIDVAL = function(x,y,z,r){ # x is the matrix, y is the cluster matrix, y is the lower limit, z is the higher limit
  ncluster.bd.isa.sig <- vector("list",1)
  for (i in z:r){
    ncluster.bd.isa.sig[[i]] <-indval(x,y[[i]], control = how(nperm=4999))$pval # run model and pull out p values
    ncluster.bd.isa.sig[[i]][ncluster.bd.isa.sig[[i]]>=0.05] = NA # non sigificant pvalues should be made NA
    ncluster.bd.isa.sig[[i]][ncluster.bd.isa.sig[[i]]<0.05] = 1 # siginifint ones should be made 1
    ncluster.bd.isa.sig[[i]] = sum(ncluster.bd.isa.sig[[i]],na.rm = TRUE) # find the number of indicators
  }
  return(ncluster.bd.isa.sig)}

WETBNogroupsIval = ISA.SigIndicatorsIDVAL(x=Wet.Birds.MatrixISA,y=wetncluster.bd.isa, z=2,r=10)#  5

ISA.SigIDVAL = function(x,y,z,r){ # x is the matrix, y is the cluster matrix, y is the lower limit, z is the higher limit
  ncluster.bd.isa.sig <- vector("list",1)
  for (i in z:r){
    ncluster.bd.isa.sig[[i]] <-indval(x,y[[i]], control = how(nperm=4999))$pval # run model and pull out p values
    ncluster.bd.isa.sig[[i]] = mean(ncluster.bd.isa.sig[[i]])# non sigificant pvalues should be made NA
  }
  return(ncluster.bd.isa.sig)}

WETSigIval = ISA.SigIDVAL(x=Wet.Birds.MatrixISA,y=wetncluster.bd.isa, z=2,r=11) # 6
 
##### Final ISA #####

# now that we know what the optimal number of clusters are, we can proceed in building the final ISA
# using 5 clusters
OWETBISA.Polan <- indval(Wet.Birds.MatrixISA,wetncluster.bd.isa[[5]], control = how(nperm=4999))

# extract the p-values, indicator values and which cluster is species is most strongly associated with
OWETBISAPvalue <- matrix(OWETBISA.Polan$pval)
rownames(OWETBISAPvalue) <- names(OWETBISA.Polan$pval)
OWETBISAMaxcls = matrix(OWETBISA.Polan$maxcls)
rownames(OWETBISAMaxcls) <- names(OWETBISA.Polan$maxcls)
OWETBISAIndcls = matrix(OWETBISA.Polan$indcls)
rownames(OWETBISAIndcls) <- names(OWETBISA.Polan$indcls)
OWETBISAIndval= OWETBISA.Polan$indval
# now, making into one data frame
OWETBISAIndvalF = data.frame(Maxcls = OWETBISAMaxcls,
                             Inscls = OWETBISAIndcls,
                             Pvalue= OWETBISAPvalue,
                             OWETBISAIndval)
# sort data by p-value and group
OWETBISAIndvalF = OWETBISAIndvalF[order(OWETBISAIndvalF$Maxcls,OWETBISAIndvalF$Pvalue),]
OWETBISAIndvalF$Group[OWETBISAIndvalF$Maxcls==1] <- " Pond & Reed Associates "
OWETBISAIndvalF$Group[OWETBISAIndvalF$Maxcls==2] <- " Shrub Associates "
OWETBISAIndvalF$Group[OWETBISAIndvalF$Maxcls==3] <- " Hummock Nesters"
OWETBISAIndvalF$Group[OWETBISAIndvalF$Maxcls==4] <- " Shoreline"
OWETBISAIndvalF$Group[OWETBISAIndvalF$Maxcls==5] <- "Wetland Edge Nesters"

# give a new clustering number based on wetland affinity
OWETBISAIndvalF$NGroup[OWETBISAIndvalF$Maxcls==1] <- 5 # 8
OWETBISAIndvalF$NGroup[OWETBISAIndvalF$Maxcls==2] <- 1 # 7
OWETBISAIndvalF$NGroup[OWETBISAIndvalF$Maxcls==3] <- 3 # 10
OWETBISAIndvalF$NGroup[OWETBISAIndvalF$Maxcls==4] <- 4 # 11
OWETBISAIndvalF$NGroup[OWETBISAIndvalF$Maxcls==5] <- 2 # 12


OWETBISAIndvalFO <- OWETBISAIndvalF[order(OWETBISAIndvalF$NGroup,OWETBISAIndvalF$Pvalue, decreasing = c(FALSE,FALSE)),]
write.table(OWETBISAIndvalFO, "Output/WETBIRDINVAL.txt",sep="\t")




