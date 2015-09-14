#!/bin/R
setwd('/path/to/analysis')
library("vegan")
arg <- commandArgs()
name<-arg[3]
SNVcalls<-read.table(file = as.character(arg[2]), header = TRUE, sep = "\t")
truth<-read.table(file = paste(name,".truth_pos.bed",sep=""),header = FALSE , sep ="\t")

Ground_Truth<-numeric()
for(chr in unique(SNVcalls[,1])){
	spare_vcf_pos<-truth[truth[,1]==chr,2]
	spare_pipeline_pos<-SNVcalls[SNVcalls[,1]==chr,3]
	Ground_Truth<-c(Ground_Truth,sapply(X = spare_pipeline_pos , FUN = function(z) {if(sum(as.numeric(z) %in% spare_vcf_pos)>0){return(1)}else{return(0)}} ) )
}

SNVs<-cbind(SNVcalls, Ground_Truth)
d<-vegdist(x=t(SNVs[,-c(1,2,3)]),method="jaccard",binary=TRUE)
h<-hclust(d,method = "ward.D")
tiff(paste(name,".tiff",sep=""),res = 100, width = 22, height = 15, units ='in')
plot(h)
dev.off()
  
