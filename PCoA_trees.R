library("vegan")
library("tibble")

tree <-read.csv("TREE.csv")
print(tree)

tree.species.df <-data.frame(Plot_ID = c(tree$PLOT),
                             Species_ID =c(tree$SPCD))
print(tree.species.df)

print(tree.species.df)

#Site by Species df 
tree.ss.df <- as.data.frame.matrix(table(tree.species.df$Plot_ID, tree.species.df$Species_ID))
tree.ss.df <- rownames_to_column(tree.ss.df, var = "Plot_ID")
print(tree.ss.df)
tree.species.only.df <- tree.ss.df[, 2:ncol(tree.ss.df)]

treeREL <- tree.species.only.df 
for(i in 1:nrow(tree.species.only.df)){
  treeREL[i,] =tree.species.only.df[i,]/sum(tree.species.only.df[i,])
}
# add.spec.scores <- function(ordi,comm,method="cor.scores",multi=1,Rscale=F,scaling="1") {
#   ordiscores <- scores(ordi,display="sites")
#   n <- ncol(comm)
#   p <- ncol(ordiscores)
#   specscores <- array(NA,dim=c(n,p))
#   rownames(specscores) <- colnames(comm)
#   colnames(specscores) <- colnames(ordiscores)
#   if (method == "cor.scores") {
#     for (i in 1:n) {
#       for (j in 1:p) {specscores[i,j] <- cor(comm[,i],ordiscores[,j],method="pearson")}
#     }
#   }
#   if (method == "wa.scores") {specscores <- wascores(ordiscores,comm)}
#   if (method == "pcoa.scores") {
#     rownames(ordiscores) <- rownames(comm)
#     eigenv <- ordi$eig
#     accounted <- sum(eigenv)
#     tot <- 2*(accounted/ordi$GOF[2])-(accounted/ordi$GOF[1])
#     eigen.var <- eigenv/(nrow(comm)-1)
#     neg <- length(eigenv[eigenv<0])
#     pos <- length(eigenv[eigenv>0])
#     tot <- tot/(nrow(comm)-1)
#     eigen.percen <- 100*eigen.var/tot
#     eigen.cumpercen <- cumsum(eigen.percen)
#     constant <- ((nrow(comm)-1)*tot)^0.25
#     ordiscores <- ordiscores * (nrow(comm)-1)^-0.5 * tot^-0.5 * constant
#     p1 <- min(p, pos)
#     for (i in 1:n) {
#       for (j in 1:p1) {
#         specscores[i,j] <- cor(comm[,i],ordiscores[,j])*sd(comm[,i])/sd(ordiscores[,j])
#         if(is.na(specscores[i,j])) {specscores[i,j]<-0}
#       }
#     }
#     if (Rscale==T && scaling=="2") {
#       percen <- eigen.var/tot
#       percen <- percen^0.5
#       ordiscores <- sweep(ordiscores,2,percen,"/")   
#       specscores <- sweep(specscores,2,percen,"*")
#     }
#     if (Rscale==F) {
#       specscores <- specscores / constant
#       ordiscores <- ordi$points
#     }        
#     ordi$points <- ordiscores
#     ordi$eig <- eigen.var
#     ordi$eig.percen <- eigen.percen
#     ordi$eig.cumpercen <- eigen.cumpercen
#     ordi$eigen.total <- tot
#     ordi$R.constant <- constant
#     ordi$Rscale <- Rscale
#     ordi$scaling <- scaling
#   }
#   specscores <- specscores * multi    
#   ordi$cproj <- specscores
#   return(ordi)
# }

tree.pcoa <-add.spec.scores(tree.pcoa, treeREL, method = "pcoa.scores")
vis <- text(tree.pcoa$cproj[,1], tree.pcoa$cproj[,2],
     labels = row.names(tree.pcoa$cproj), col = "black")
print(vis)
