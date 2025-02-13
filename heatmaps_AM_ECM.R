#Invasive AM
#take out plot ID
invasive.AM.ss <-invasive.AM.ss[,-1]


tree.bray.iAM <-vegdist(invasive.AM.ss, method = "bray")

order.iAM <-rev(attr(tree.bray.iAM, "Labels"))
levelplot(as.matrix(tree.bray.iAM)[,order], aspect = "iso", col.regions =inferno,
          xlab = "Plot", ylab = "Plot", scales =list(cex =.5),
          main = "Bray-Curtis Distance")
