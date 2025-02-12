setwd("/cloud/project/QB_biodiversity_project_EH")
# Load necessary libraries
library(vegan)
library(tibble)
library(lattice)
library(readxl)
library(dplyr)
library(tidyr)
#load in data from separate R script file
tree <-read.csv("TREE.csv")

tree.species.df <-data.frame(Plot_ID = c(tree$PLOT),
                             SPCD =c(tree$SPCD))
myco <-as.data.frame(read_xlsx("MycoType_ref2.xlsx"))
myco<- myco[,-c(3:7)]
invasive <-read_xlsx("INVASIVE.xlsx")
untree.species <- unique(tree.species.df$SPCD)
unmyco.species <-unique(myco$SPCD)
tree.species.df <- tree.species.df %>%
  filter(!SPCD %in% c(999, 998)) 

#add myco type
tree.species.df$PlotStatus <- NA

# Loop through rows of tree.species.df
for (i in seq_len(nrow(tree.species.df))) {  
  if (tree.species.df$Plot_ID[i] %in% invasive$PLOT) {  
    tree.species.df$PlotStatus[i] <- "invasive"  
  } else {  
    tree.species.df$PlotStatus[i] <- "non-invasive"  
  }
}
#Site by Speccies
tree.species.df.1 <-data.frame(Plot_ID = c(tree$PLOT),
                               Species_ID =c(tree$SPCD))
#print(tree.species.df)


tree.ss.df <- as.data.frame.matrix(table(tree.species.df.1$Plot_ID, tree.species.df.1$Species_ID))
tree.ss.df <- rownames_to_column(tree.ss.df, var = "Plot_ID")
#print(tree.ss.df)

tree.species.only.df <- tree.ss.df[, 2:ncol(tree.ss.df)]


#Create Factors vector

factor <- factor(tree.species.df$PlotStatus)

length(factor)
nrow(tree.species.only.df)

factor_trimmed <- factor[1:nrow(tree.species.only.df)]

adonis2(tree.species.only.df ~ factor_trimmed, method = "bray", permutations = 999)

