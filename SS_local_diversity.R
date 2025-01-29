#This was having trouble knitting- I was switching between the console and r-chunk due to issues setting the working directory. I transformed
tree <-read.csv("TREE.csv")
print(tree)
tree.df <-as.data.frame(tree)
print(tree.df)

tree.species.df <-data.frame(Plot_ID = c(tree$PLOT),
                             Species_ID =c(tree$SPCD))
print(tree.species.df)

print(tree.species.df$Plot_ID)

#necessary functions 
  #observed richenss
S.obs <- function(x = ""){
  rowSums(x > 0) *1 
} 
  #Good's coverage
GC <- function (x = ""){1- (rowSums(x ==1)/rowSums(x))}
  #chao1
chao1 <- function(x ="") {
  S.obs(x) + (sum(x==1)^2)/(2*sum(x ==2))
}
  #chao2
chao2 <- function(site = "", SbyS = "") {
  SbyS = as.data.frame(SbyS)
  x = SbyS[site,]
  SbyS.pa <- (SbyS >0) *1
  Q1 = sum(colSums(SbyS.pa) ==1)
  Q2 = sum(colSums(SbyS.pa) ==2)
  chao2 = S.obs(x) +(Q1^2)/(2*Q2)
  return(chao2)
}
  #Shannon's H
shanH <-function (x = ""){
  H =0
  for (n_i in x){
    if (n_i > 0) {
      p = n_i/sum(x)
      H = H-p*log(p)
    }
  }
  return(H)
}

  #Evar
Evar <- function(x){
  x <-as.vector(x[x >0])
  1-(2/pi)*atan(var(log(x)))
}
  #ACE
ace <- function (x = "", thresh =10){
  x <- x[x >0]
  S.abund <-length(which (x > thresh))
  S.rare <- length(which (x <= thresh))
  singlt <- length(which (x ==1))
  N.rare <-sum(x[which (x <= thresh)])
  C.ace <-1-(singlt/N.rare)
  i <- c(1:thresh)
  count <- function (i, y){
    length(y[y == i])
  }
  a.1 <- sapply(i, count, x)
  f.1 <-(i * (i-1))*a.1
  G.ace <- (S.rare/C.ace)*(sum(f.1)/(N.rare*(N.rare-1)))
  S.ace <- S.abund +(S.rare/C.ace) +(singlt/C.ace) *max(G.ace,0)
  return(S.ace)
}

  #RAC
RAC <-function (x = "") {
  x.ab = x[x>0]
  x.ab.ranked = x.ab[order(x.ab, decreasing = TRUE)]
  as.data.frame(lapply(x.ab.ranked, unlist))
  return(x.ab.ranked)
}

  #SimpD
SimpD = function(x = ""){
  D=0
  N=sum(x)
  for (n_i in x){
    D =D +(n_i^2)/(N^2)
  }
  return(D)
}

  #simpE
simpE <- function (x = ""){
  S <-S.obs(x)
  x = as.data.frame(x)
  D <- diversity(x,"inv")
  E = (D)/S
}

#Site by Species df 
library(dplyr)
library(tidyr)
library(tibble)

tree.species.df <- tree.species.df %>%
  mutate(Plot_ID = as.character(Plot_ID), Species_ID = as.character(Species_ID))
tree.ss.df <- as.data.frame.matrix(table(tree.species.df$Plot_ID, tree.species.df$Species_ID))
tree.ss.df <- rownames_to_column(tree.ss.df, var = "Plot_ID")
print(tree.ss.df)
tree.species.df <- tree.ss.df[, 2:ncol(tree.ss.df)]
#Overview
length(tree.species.df$Species_ID) #43528 species in the Tree dataset-species delimited by species ID ranging from 110-999 translation of the codes can be found in the repository
range(tree.species.df$Species_ID)
length(tree.species.df$Plot_ID) $43528 plots in the Tree dataset ranging from 1-999 translation of the codes can be found in the repository
range(tree.species.df$Plot_ID)
str(tree.species.df)
#This dataset is so large much of the diversity metrics will only printed a limited number of outputs. I will have the code for all the sites commented out with comparison at only one site (Site 1)
tree.site1.df <- as.data.frame(tree.ss.df[1,])
tree.site1.df$Plot_ID <- NULL
print(tree.site1.df)

#Observed Richness
S.obs(tree.site1.df) #Observed richenss = 2
obsRich.all <- S.obs(tree.ss.df) #Observed richness ranges from [2,12] for the enture dataset

#Good's Coverage
GC(tree.site1.df) #.889 GC
GC.tree.all <-GC(tree.species.df) #mean = .6391


chao1.tree.all <- chao1(tree.species.df) #range [17048.72, 17058.72] mean 17051.15

#as of 10:41 on 1/29/2025 I am pushing but will keep working- there is so much data I need some time to think on how I want to filter it so I can do appropriate analysis 