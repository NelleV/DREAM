#!/bin/R

library(randomForest)
library(glmnet)
library(ROCR)
### Adapted from Stéphane Tufféry: Modélisation et apprentissage statistique avec R p254-256 
RForestLog = function(apprent,validat, varY,varX,nb_var,crit = "BIC",nsimul = 1){
	nobs<-nrow(apprent)
	y<-apprent[,varY]
	formule<-as.formula(paste("y ~ ",paste(names(apprent[,varX]),collapse ="+")))
	sature<-glm(formule,data =apprent[,c(varX,varY)],family = binomial(link = "logit"))
	coef <- matrix (0,length(coef(sature)),nsimul+2)
	rownames(coef)<-names(coef(sature))
	
	predic<-matrix(NA,nrow(validat),nsimul)

	auc<-matrix(0,nsimul,1)
	for(i in (1:nsimul)){
		##random sampling
		size<-length(varX) ##How many explicative features
		s<-sort(sample(size,nb_var,replace=F))
		predicteurs<-varX[s]
		print(predicteurs)
		s<-sample(nobs,nobs,replace = T)
		#writing formula with all randomly chosen predictors

		if(nb_var >1){
			formule<-as.formula(paste("y~ ", paste(names(apprent[,predicteurs]),collapse = "+")))
		} else {
			formule<- as.formula(paste("y ~ ",predicteurs))
		}
		target<-apprent[s,varY]
		#minimal model
		logit<-glm(target ~ 1, data = apprent[s,],family = binomial(link = 'logit'))
		##step by step selection

		if(crit =="bic"){
			selection <-step(logit,direction = "forward",trace = F, k = log(nobs), scope = list(upper = formule))
		
		}else if(crit =="aic"){
			selection <-step(logit,direction = "forward",trace = F, k = 2, scope = list(upper = formule))
		}
		else{ print("unknown criterion... exiting"); return(NA)}
		## applying adjusted logit
		predic[,i] <-predict(selection, newdata =validat, type = "response")
		## mean of predictions on first i aggregated models
		if(i==1){
			moypred <- predic[,i]
		}else{
			moypred <- apply(predic[,1:i],1,mean)					
		}

		## calculating AUC
		target <- validat[,varY]
		pred <- prediction(moypred, target,label.ordering = c(0,1))
		auc[i] <- performance(pred, "auc")@y.values[[1]]

		##keeping non-zero coef
		index <- match(names(coef(selection)), names(coef(sature)))
		coef[index,i] <- coef(selection)

	}
	##mean coef of aggregated models
	coef[,nsimul+1] <- apply(coef[,1:nsimul],1,mean)
	##non-zero coef
	coef[,nsimul+1] <- apply((coef[,1:nsimul]!=0),1,sum)
	rf <- list(auc, coef)
	return(rf)
}

add_truth_col<-function(feature,truth_bed){
	truth_col<-numeric()	
	for(chr in unique(feature$CHROM)){
		print(chr)
		spare_pos<-feature$END[feature$CHROM==chr]
		spare_truth<-truth_bed[truth[,1]==chr,2]
		truth_col<-c(truth_col,sapply(X= spare_pos, FUN= function(z) {
			if(sum(z %in% spare_truth)>0){
				return(1)
			}
			else{ return(0)}
		}))
	}
	result<-cbind(feature,truth_col)
	return(result)
}

###example below:
#features<-read.table('/data/tmp/2015...',header= TRUE)
#truth <- read.table('/data/tmp/.../Clustering.../...bed')
#features_with_truth<-add_truth_col(features,truth)
#Log<-RForestLog(apprent = features_with_truth[1:50000,],validat =  features_with_truth[50001:70001,],varY = "truth_col",varX = names(features_with_truth[,4:9]),nb_var=1, crit = "bic", nsimul = 2)

