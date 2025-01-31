library(ggplot2)
tree <-read.csv("TREE.csv")
print(tree)

tree.species.df <-data.frame(Plot_ID = c(tree$PLOT),
                             Species_ID =c(tree$SPCD))
print(tree.species.df)

print(tree.species.df$Plot_ID)

#Site by Species df 
tree.ss.df <- as.data.frame.matrix(table(tree.species.df$Plot_ID, tree.species.df$Species_ID))
tree.ss.df <- rownames_to_column(tree.ss.df, var = "Plot_ID")
print(tree.ss.df)

tree.species.only.df <- tree.ss.df[, 2:ncol(tree.ss.df)]
##########################################################
#Could make bar graph for each plot 
#pca?
site1 <- tree.species.only.df[1,]
site1

