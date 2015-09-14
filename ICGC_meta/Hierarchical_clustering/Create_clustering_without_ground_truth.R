#!/bin/R

library("vegan")
arg <- commandArgs()
name<-arg[3]
SNVcalls<-read.table(file = as.character(arg[2]), header = TRUE, sep = "\t")
d<-vegdist(x=t(SNVcalls[,-c(1,2,3)]),method="jaccard",binary=TRUE)
h<-hclust(d,method = "ward.D")
tiff(paste(name,".tiff",sep=""),res = 100, width = 22, height = 15, units ='in')
plot(h)
dev.off()
  
