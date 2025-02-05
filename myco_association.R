library(readxl)
library(tibble)
#load tree
tree <-read.csv("TREE.csv")

tree.species.df <-data.frame(Plot_ID = c(tree$PLOT),
                             SPCD =c(tree$SPCD))
str(tree.species.df)
as.numeric()
print(tree.species.df)

myco <-as.data.frame(read_xlsx("MycoType_ref.xlsx"))
myco<- myco[,-c(3:7)]
str(myco)


#Merging
tree.myco.species <- merge(tree.species.df, myco, by = "SPCD")
print(tree.myco.species)
#right now 11:41 am 2/5/2025 the tree.myco.species is going through each time a SPCD is listed 
#and then giving its myco association - try filtering it so all the 1, 2, and 3 are together
#then make indvidual site by species???
#should you do invasive layer first? 
#Need to clean out the species that we were not able to idnitify 

