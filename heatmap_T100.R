# Load necessary libraries
library(vegan)
library(tibble)
library(lattice)

# Read the CSV file
tree <- read.csv("TREE.csv")
#print(tree)

tree.species.df <- data.frame(Plot_ID = tree$PLOT, Species_ID = tree$SPCD)
#print(tree.species.df)

tree.ss.df <- as.data.frame.matrix(table(tree.species.df$Plot_ID, tree.species.df$Species_ID))
tree.ss.df <- rownames_to_column(tree.ss.df, var = "Plot_ID")
tree.ss.df <-tree.ss.df[1:100,1:100]
#print(tree.ss.df)

tree.species.only.df <- tree.ss.df[, -1]
print(tree.species.only.df)

tree.species.only.df.L <-tree.species.df

#Bray-Curtis 

tree.db <-vegdist(tree.species.only.df, method = "bray")

order <-rev(attr(tree.db, "Labels"))
levelplot(as.matrix(tree.db)[,order], aspect = "iso", col.regions =inferno,
         xlab = "Plot", ylab = "Plot", scales =list(cex =.5),
         main = "Bray-Curtis Distance")
