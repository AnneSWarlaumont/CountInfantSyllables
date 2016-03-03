rm(list=ls()); graphics.off()

library(lme4)
library(lmerTest)
library(mgcv)
library(RVAideMemoire)
library(caret)
library(e1071)

# Uncomment one of the two lines below
# nocrylauveg = FALSE
nocrylauveg = TRUE

# Uncomment one of the two lines below
# nooverlap = FALSE
nooverlap = TRUE

vocdat = read.csv('~/RamsdellWarlaumontCollab/vocdat.csv',header=T)

# Find out how many judgments each listener made
length(which(!is.na(vocdat$humnumanysyl_awarlaumont2_1)))
length(which(!is.na(vocdat$humnumanysyl_afontana5_1)))
length(which(!is.na(vocdat$humnumanysyl_gmacedo_1)))

# Find out how many judgments each listener made while there was a bug that didn't record their cry/laugh/vegetative judgment
length(which(vocdat$humcrylauveg_awarlaumont2_1==""))
length(which(vocdat$humcrylauveg_afontana5_1==""))
length(which(vocdat$humcrylauveg_gmacedo_1==""))

# Remove cases where no human listener made syllable judgments or where at least one listener entered "x"
# "x" was entered when the listener thought the utterance came from someone other than the kid.
vocdat = subset(vocdat,(humnumanysyl_awarlaumont2_1!="x"|is.na(humnumanysyl_awarlaumont2_1))&(humnumcansyl_awarlaumont2_1!="x"|is.na(humnumcansyl_awarlaumont2_1))&(humnumanysyl_afontana5_1!="x"|is.na(humnumanysyl_afontana5_1))&(humnumcansyl_afontana5_1!="x"|is.na(humnumcansyl_afontana5_1))&(humnumanysyl_gmacedo_1!="x"|is.na(humnumanysyl_gmacedo_1))&(humnumcansyl_gmacedo_1!="x"|is.na(humnumcansyl_gmacedo_1)))

# Optional: limit the data to rows where all the human listeners agree there is no overlap with another sound
if (nooverlap) {
	vocdat = subset(vocdat,((humhumover_awarlaumont2_1=="n"|is.na(humhumover_awarlaumont2_1))&(humhumover_afontana5_1=="n"|is.na(humhumover_afontana5_1))&(humhumover_gmacedo_1=="n"|is.na(humhumover_gmacedo_1)))&((humnonhumover_awarlaumont2_1=="n"|is.na(humnonhumover_awarlaumont2_1))&(humnonhumover_afontana5_1=="n"|is.na(humnonhumover_afontana5_1))&(humnonhumover_gmacedo_1=="n"|is.na(humnonhumover_gmacedo_1))))
}

# Optional: limit the data to rows where all the human listeners agree the vocalization is an infant protophone (humcrylauveg is "n")
if (nooverlap) {
	vocdat = subset(vocdat,((humcrylauveg_awarlaumont2_1=="n"|is.na(humcrylauveg_awarlaumont2_1))&(humcrylauveg_afontana5_1=="n"|is.na(humcrylauveg_afontana5_1))&(humcrylauveg_gmacedo_1=="n"|is.na(humcrylauveg_gmacedo_1))))
}

# Find out how many judgments each listener has included in the final dataset
length(which(!is.na(vocdat$humnumanysyl_awarlaumont2_1)))
length(which(!is.na(vocdat$humnumanysyl_afontana5_1)))
length(which(!is.na(vocdat$humnumanysyl_gmacedo_1)))

vocdat$humnumanysyl_awarlaumont2_1 = as.numeric(as.character(vocdat$humnumanysyl_awarlaumont2_1))
vocdat$humnumcansyl_awarlaumont2_1 = as.numeric(as.character(vocdat$humnumcansyl_awarlaumont2_1))
vocdat$humnumanysyl_afontana5_1 = as.numeric(as.character(vocdat$humnumanysyl_afontana5_1))
vocdat$humnumcansyl_afontana5_1 = as.numeric(as.character(vocdat$humnumcansyl_afontana5_1))
vocdat$humnumanysyl_gmacedo_1 = as.numeric(as.character(vocdat$humnumanysyl_gmacedo_1))
vocdat$humnumcansyl_gmacedo_1 = as.numeric(as.character(vocdat$humnumcansyl_gmacedo_1))

vocdat$humnumanysyl_avg = rowMeans(cbind(vocdat$humnumanysyl_awarlaumont2_1,vocdat$humnumanysyl_afontana5_1,vocdat$humnumanysyl_gmacedo_1),na.rm=T)
vocdat$humnumcansyl_avg = rowMeans(cbind(vocdat$humnumcansyl_awarlaumont2_1,vocdat$humnumcansyl_afontana5_1,vocdat$humnumcansyl_gmacedo_1),na.rm=T)

vocdat$humnumcansyl_awarlaumont2_1_categorical = vocdat$humnumcansyl_awarlaumont2_1
vocdat$humnumcansyl_awarlaumont2_1_categorical[vocdat$humnumcansyl_awarlaumont2_1>2] = 3
vocdat$humnumanysyl_awarlaumont2_1_categorical = vocdat$humnumanysyl_awarlaumont2_1
vocdat$humnumanysyl_awarlaumont2_1_categorical[vocdat$humnumanysyl_awarlaumont2_1>3] = 4

vocdat$humnumcansyl_afontana5_1_categorical = vocdat$humnumcansyl_afontana5_1
vocdat$humnumcansyl_afontana5_1_categorical[vocdat$humnumcansyl_afontana5_1>2] = 3
vocdat$humnumanysyl_afontana5_1_categorical = vocdat$humnumanysyl_afontana5_1
vocdat$humnumanysyl_afontana5_1_categorical[vocdat$humnumanysyl_afontana5_1>3] = 4

vocdat$humnumcansyl_gmacedo_1_categorical = vocdat$humnumcansyl_gmacedo_1
vocdat$humnumcansyl_gmacedo_1_categorical[vocdat$humnumcansyl_gmacedo_1>2] = 3
vocdat$humnumanysyl_gmacedo_1_categorical = vocdat$humnumanysyl_gmacedo_1
vocdat$humnumanysyl_gmacedo_1_categorical[vocdat$humnumanysyl_gmacedo_1>3] = 4

vocdat$humnumcansyl_avg_categorical = round(vocdat$humnumcansyl_avg)
vocdat$humnumcansyl_avg_categorical[vocdat$humnumcansyl_avg_categorical>2] = 3
vocdat$humnumanysyl_avg_categorical = vocdat$humnumanysyl_avg
vocdat$humnumanysyl_avg_categorical[vocdat$humnumanysyl_avg_categorical>3] = 4

vocdat$DurS = vocdat$DurMs/1000

vocdat$numlmksyl[is.na(vocdat$numlmksyl)] = 0

# Get correlations between each measure and age
cor.test(vocdat$numsalons,vocdat$AgeInDays,method="spearman",exact=F)
cor.test(vocdat$numlmksyl,vocdat$AgeInDays,method="spearman",exact=F)
cor.test(vocdat$numdjwsyl,vocdat$AgeInDays,method="spearman",exact=F)
cor.test(vocdat$numsphcon,vocdat$AgeInDays,method="spearman",exact=F)
cor.test(vocdat$numsphvow,vocdat$AgeInDays,method="spearman",exact=F)
cor.test(vocdat$humnumanysyl_awarlaumont2_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1/vocdat$humnumanysyl_awarlaumont2_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1/vocdat$DurS,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumanysyl_afontana5_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_afontana5_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_afontana5_1/vocdat$humnumanysyl_afontana5_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_afontana5_1/vocdat$DurS,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumanysyl_gmacedo_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_gmacedo_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_gmacedo_1/vocdat$humnumanysyl_gmacedo_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_gmacedo_1/vocdat$DurS,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumanysyl_avg,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_avg,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_avg/vocdat$humnumanysyl_avg,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_avg/vocdat$DurS,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)

# Get correlations between the automated measures and the human judgments

cor.test(vocdat$numsalons,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumanysyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumcansyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumanysyl_avg,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumcansyl_avg,method="spearman",exact=F,na.rm=T)

cor.test(vocdat$numlmksyl,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumanysyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumcansyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumanysyl_avg,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumcansyl_avg,method="spearman",exact=F,na.rm=T)

cor.test(vocdat$numdjwsyl,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumanysyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumcansyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumanysyl_avg,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumcansyl_avg,method="spearman",exact=F,na.rm=T)

cor.test(vocdat$numsphcon,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphcon,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphcon,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphcon,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphcon,vocdat$humnumanysyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphcon,vocdat$humnumcansyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphcon,vocdat$humnumanysyl_avg,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphcon,vocdat$humnumcansyl_avg,method="spearman",exact=F,na.rm=T)

cor.test(vocdat$numsphvow,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphvow,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphvow,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphvow,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphvow,vocdat$humnumanysyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphvow,vocdat$humnumcansyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphvow,vocdat$humnumanysyl_avg,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsphvow,vocdat$humnumcansyl_avg,method="spearman",exact=F,na.rm=T)


# Get awarlaumont2_1 and afontana5_1 interrater correlations
cor.test(vocdat$humnumanysyl_awarlaumont2_1,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
nrow(subset(vocdat,!is.na(humnumcansyl_awarlaumont2_1)&!is.na(humnumcansyl_afontana5_1))) # How many points were included in the above interrater correlations?

# Get awarlaumont2_1 and gmacedo_1 interrater correlations
cor.test(vocdat$humnumanysyl_awarlaumont2_1,vocdat$humnumanysyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1,vocdat$humnumcansyl_gmacedo_1,method="spearman",exact=F,na.rm=T)
nrow(subset(vocdat,!is.na(humnumcansyl_awarlaumont2_1)&!is.na(humnumcansyl_gmacedo_1))) # How many points were included in the above interrater correlations?

# Get gmacedo_1 and afontana5_1 interrater correlations
cor.test(vocdat$humnumanysyl_gmacedo_1,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_gmacedo_1,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
nrow(subset(vocdat,!is.na(humnumcansyl_gmacedo_1)&!is.na(humnumcansyl_afontana5_1))) # How many points were included in the above interrater correlations?

# Get leave-one-child-out cross-validation predictions
# Pilot work showed gam to be better than pca and svr
# Pilot work showed equalizing category counts to provide better results

#Initialize the columns that will hold the cross-validation predictions
vocdat$looanysylestpcagam = rep(NA,nrow(vocdat)) # PCA predicting total syllable count
vocdat$loocansylestpcagam = rep(NA,nrow(vocdat)) # PCA predicting canonical syllable count
vocdat$looanysylestsalgam = rep(NA,nrow(vocdat)) # Salience predicting total syllable count
vocdat$looanysylestlmkgam = rep(NA,nrow(vocdat)) # Salience predicting canonical syllable count
vocdat$looanysylestdjwgam = rep(NA,nrow(vocdat)) # de Jong & Wempe predicting total syllable count
vocdat$loocansylestsalgam = rep(NA,nrow(vocdat)) # de Jong & Wempe predicting canonical syllable count
vocdat$looanysylestlmkgam = rep(NA,nrow(vocdat)) # Landmarks predicting total syllable count
vocdat$loocansylestdjwgam = rep(NA,nrow(vocdat)) # Landmarks predicting canonical syllable count
vocdat$looanysylestsphcon = rep(NA,nrow(vocdat)) # Sphinx consonants predicting total syllable count
vocdat$loocansylestsphcon = rep(NA,nrow(vocdat)) # Sphinx consonants predicting canonical syllable count
vocdat$looanysylestsphvow = rep(NA,nrow(vocdat)) # Sphinx vowels predicting total syllable count
vocdat$loocansylestsphvow = rep(NA,nrow(vocdat)) # Sphinx vowels predicting canonical syllable count

for (id in levels(vocdat$ID)){ # for each child to be left out
	
	# For every # of syllables except the # of syllables with the most frequent exemplars, resample until that # of syllables has the same number of exemplars as the # of exemplars in the max # of syllables category.

	eqtraindat_canonical = subset(vocdat,(ID!=id) & !is.na(humnumcansyl_avg_categorical))
	eqtraindat_any = subset(vocdat,(ID!=id) & !is.na(humnumanysyl_avg_categorical))
	maxexemplars_canonical = max(sum(eqtraindat_canonical$humnumcansyl_avg_categorical==0),sum(eqtraindat_canonical$humnumcansyl_avg_categorical==1),sum(eqtraindat_canonical$humnumcansyl_avg_categorical==2),sum(eqtraindat_canonical$humnumcansyl_avg_categorical==3))
	maxexemplars_any = max(sum(eqtraindat_any$humnumanysyl_avg_categorical==0),sum(eqtraindat_any$humnumanysyl_avg_categorical==1),sum(eqtraindat_any$humnumanysyl_avg_categorical==2),sum(eqtraindat_any$humnumanysyl_avg_categorical==3))
	
	# Resample so all categories have the same number of exemplars
	for (sylnum in c(0,1,2,3)){
		exemplarnumdif_canonical = maxexemplars_canonical - sum(eqtraindat_canonical$humnumcansyl_avg_categorical==sylnum)
		sylnumind_canonical = which(eqtraindat_canonical$humnumcansyl_avg_categorical %in% sylnum)
		newexemplars_canonical = sample(sylnumind_canonical,size = exemplarnumdif_canonical, replace = T)
		eqtraindat_canonical = rbind(eqtraindat_canonical,eqtraindat_canonical[newexemplars_canonical,])
	}
	for (sylnum in c(1,2,3,4)){
		exemplarnumdif_any = maxexemplars_any - sum(eqtraindat_any$humnumanysyl_avg_categorical==sylnum)
		sylnumind_any = which(eqtraindat_any$humnumanysyl_avg_categorical %in% sylnum)
		newexemplars_any = sample(sylnumind_any,size = exemplarnumdif_any, replace = T)
		eqtraindat_any = rbind(eqtraindat_any,eqtraindat_any[newexemplars_any,])
	}

	tempPreProcFunction_canonical = preProcess(eqtraindat_canonical[,c('DurMs','numsalons','numlmksyl','numdjwsyl','numsphcon','numsphvow')],method=c('BoxCox','center','scale','pca'))
	tempPreProcFunction_any = preProcess(eqtraindat_any[,c('DurMs','numsalons','numlmksyl','numdjwsyl','numsphcon','numsphvow')],method=c('BoxCox','center','scale','pca'))
	trainPreProcValues_canonical = predict(tempPreProcFunction_canonical,eqtraindat_canonical[,c('DurMs','numsalons','numlmksyl','numdjwsyl','numsphcon','numsphvow')])
	trainPreProcValues_any = predict(tempPreProcFunction_any,eqtraindat_any[,c('DurMs','numsalons','numlmksyl','numdjwsyl','numsphcon','numsphvow')])
	testPreProcValues_canonical = predict(tempPreProcFunction_canonical,subset(vocdat,(ID==id))[,c('DurMs','numsalons','numlmksyl','numdjwsyl','numsphcon','numsphvow')])
	testPreProcValues_any = predict(tempPreProcFunction_any,subset(vocdat,(ID==id))[,c('DurMs','numsalons','numlmksyl','numdjwsyl','numsphcon','numsphvow')])
	traindat_canonical = trainPreProcValues_canonical
	traindat_any = trainPreProcValues_any
	traindat_canonical$humnumcansyl_avg_categorical = eqtraindat_canonical$humnumcansyl_avg_categorical
	traindat_any$humnumanysyl_avg_categorical = eqtraindat_any$humnumanysyl_avg_categorical
	loocansylestpcagam = gam(humnumcansyl_avg_categorical ~ PC1 + PC2 + PC3, dat = traindat_canonical)
	looanysylestpcagam = gam(humnumanysyl_avg_categorical ~ PC1 + PC2 + PC3, dat = traindat_any)
	vocdat$loocansylestpcagam[vocdat$ID==id] = predict(loocansylestpcagam,newdata = testPreProcValues_canonical,type="response")
	vocdat$looanysylestpcagam[vocdat$ID==id] = predict(looanysylestpcagam,newdata = testPreProcValues_any,type="response")
	
}

# Human-machine correlation in number of canonical syllables per utterance
print(cor.test(subset(vocdat,!is.na(humnumcansyl_avg_categorical))$humnumcansyl_avg_categorical,subset(vocdat,!is.na(humnumcansyl_avg_categorical))$loocansylestpcagam,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humnumcansyl_avg_categorical))$humnumcansyl_avg_categorical,subset(vocdat,!is.na(humnumcansyl_avg_categorical))$loocansylestpcagam,nrep=1000,conf.level=.95)

# Human-machine correlation in number of syllables of any type per utterance
print(cor.test(subset(vocdat,!is.na(humnumanysyl_avg_categorical))$humnumanysyl_avg_categorical,subset(vocdat,!is.na(humnumanysyl_avg_categorical))$looanysylestpcagam,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humnumanysyl_avg_categorical))$humnumanysyl_avg_categorical,subset(vocdat,!is.na(humnumanysyl_avg_categorical))$looanysylestpcagam,nrep=1000,conf.level=.95)

axmax = ceiling(max(c(subset(vocdat,!is.na(humnumcansyl_avg_categorical))$humnumcansyl_avg_categorical,subset(vocdat,!is.na(humnumcansyl_avg_categorical))$loocansylestpcagam)))
quartz(width=5,height=5)
par(mfrow=c(1,1))
boxplot(loocansylestpcagam ~factor(humnumcansyl_avg_categorical,levels=0:axmax),data=subset(vocdat,!is.na(loocansylestpcagam)),ylim=c(0,axmax),xlim=c(.5,4.5),main="Machine vs. human syllable counts",xlab="Human-judged canonical syllable count",ylab="Machine-estimated canonical syllable count")
if (nocrylauveg & nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_vs_mach_canonicalsyllablecounts_avg_nocrylaughveg_nooverlap.pdf",type="pdf")
} else if (nocrylauveg & !nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_vs_mach_canonicalsyllablecounts_avg_nocrylaughveg.pdf",type="pdf")
} else if (!nocrylauveg & nooverlap){	
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_vs_mach_canonicalsyllablecounts_avg_nooverlap.pdf",type="pdf")
} else if (!nocrylauveg & !nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_vs_mach_canonicalsyllablecounts_avg.pdf",type="pdf")
}

axmax = ceiling(max(c(subset(vocdat,!is.na(humnumanysyl_avg_categorical))$humnumanysyl_avg_categorical,subset(vocdat,!is.na(humnumanysyl_avg_categorical))$looanysylestpcagam)))
quartz(width=5,height=5)
par(mfrow=c(1,1))
boxplot(looanysylestpcagam ~factor(humnumanysyl_avg_categorical,levels=1:axmax),data=subset(vocdat,!is.na(looanysylestpcagam)),ylim=c(0,axmax),xlim=c(.5,4.5),main="Machine vs. human syllable counts",xlab="Human-judged all syllables count",ylab="Machine-estimated all syllables count")
if (nocrylauveg & nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_vs_mach_anysyllablecounts_avg_nocrylaughveg_nooverlap.pdf",type="pdf")
} else if (nocrylauveg & !nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_vs_mach_anysyllablecounts_avg_nocrylaughveg.pdf",type="pdf")
} else if (!nocrylauveg & nooverlap){	
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_vs_mach_anysyllablecounts_avg_nooverlap.pdf",type="pdf")
} else if (!nocrylauveg & !nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_vs_mach_anysyllablecounts_avg.pdf",type="pdf")
}

# Get day-level data
daydat = data.frame(matrix(NA,nrow=1,ncol=18))
names(daydat) = c('ID','AgeInDays','AveDurMs','AveDurS','AveNumSalOns','AveNumLmkSyl','AveNumDjwSyl','AveHumNumAnySyl_awarlaumont2_1','AveHumNumCanSyl_awarlaumont2_1','AveHumNumAnySyl_afontana5_1','AveHumNumCanSyl_afontana5_1','AveHumNumAnySyl_gmacedo_1','AveHumNumCanSyl_gmacedo_1','AveHumNumAnySyl_avg','AveHumNumCanSyl_avg','AveLooAnySylEstPCAGAM','AveLooCanSylEstPCAGAM')
daycnt = 1
for (id in levels(vocdat$ID)){
	childdat = subset(vocdat,ID==id)
	for (age in levels(as.factor(childdat$AgeInDays))){	
		if (daycnt > 1){
			daydat = rbind(daydat,setNames(as.data.frame(matrix(NA,nrow=1,ncol=18)),names(daydat)))
		}
		subvocdat = subset(childdat,as.factor(AgeInDays)==age)
		daydat$ID[daycnt] = id
		daydat$AgeInDays[daycnt] = as.numeric(age)
		daydat$AveDurMs[daycnt] = mean(subvocdat$DurMs)
		daydat$AveDurS[daycnt] = mean(subvocdat$DurS)
		daydat$AveNumSalOns[daycnt] = mean(subvocdat$numsalons)
		daydat$AveNumLmkSyl[daycnt] = mean(subvocdat$numlmksyl)
		daydat$AveNumDjwSyl[daycnt] = mean(subvocdat$numdjwsyl)
		daydat$AveHumNumAnySyl_awarlaumont2_1[daycnt] = mean(subvocdat$humnumanysyl_awarlaumont2_1,na.rm=T)
		daydat$AveHumNumCanSyl_awarlaumont2_1[daycnt] = mean(subvocdat$humnumcansyl_awarlaumont2_1,na.rm=T)
		daydat$AveHumNumAnySyl_afontana5_1[daycnt] = mean(subvocdat$humnumanysyl_afontana5_1,na.rm=T)
		daydat$AveHumNumCanSyl_afontana5_1[daycnt] = mean(subvocdat$humnumcansyl_afontana5_1,na.rm=T)
		daydat$AveHumNumAnySyl_gmacedo_1[daycnt] = mean(subvocdat$humnumanysyl_gmacedo_1,na.rm=T)
		daydat$AveHumNumCanSyl_gmacedo_1[daycnt] = mean(subvocdat$humnumcansyl_gmacedo_1,na.rm=T)
		daydat$AveHumNumAnySyl_avg[daycnt] = mean(subvocdat$humnumanysyl_avg,na.rm=T)
		daydat$AveHumNumCanSyl_avg[daycnt] = mean(subvocdat$humnumcansyl_avg,na.rm=T)
		daydat$AveLooAnySylEstPCAGAM[daycnt] = mean(subvocdat$looanysylestpcagam)
		daydat$AveLooCanSylEstPCAGAM[daycnt] = mean(subvocdat$loocansylestpcagam)
		daydat
		daycnt = daycnt + 1
	}
}

# Human-machine correlation in number of canonical syllables per utterance at the day level
print(cor.test(subset(daydat,!is.na(AveHumNumCanSyl_avg))$AveLooCanSylEstPCAGAM,subset(daydat,!is.na(AveHumNumCanSyl_avg))$AveHumNumCanSyl_avg,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumCanSyl_avg))$AveLooCanSylEstPCAGAM,subset(daydat,!is.na(AveHumNumCanSyl_avg))$AveHumNumCanSyl_avg,nrep=1000,conf.level=.95)

# Human-machine correlation in number of syllables per utterance at the day level
print(cor.test(subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveLooAnySylEstPCAGAM,subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumAnySyl_avg,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveLooAnySylEstPCAGAM,subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumAnySyl_avg,nrep=1000,conf.level=.95)

# Human-machine correlation in syllable ratio at the day level
print(cor.test(subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveLooCanSylEstPCAGAM/subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveLooAnySylEstPCAGAM,subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumCanSyl_avg/subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumAnySyl_avg,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveLooCanSylEstPCAGAM/subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveLooAnySylEstPCAGAM,subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumCanSyl_avg/subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumAnySyl_avg,nrep=1000,conf.level=.95)

# Human-human (awarlaumont2-afontana5) correlation in number of canonical syllables per utterance at the day level
print(cor.test(subset(daydat,!is.na(AveHumNumCanSyl_afontana5_1)&!is.na(AveHumNumCanSyl_awarlaumont2_1))$AveHumNumCanSyl_awarlaumont2_1,subset(daydat,!is.na(AveHumNumCanSyl_afontana5_1)&!is.na(AveHumNumCanSyl_awarlaumont2_1))$AveHumNumCanSyl_afontana5_1,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumCanSyl_afontana5_1)&!is.na(AveHumNumCanSyl_awarlaumont2_1))$AveHumNumCanSyl_awarlaumont2_1,subset(daydat,!is.na(AveHumNumCanSyl_afontana5_1)&!is.na(AveHumNumCanSyl_awarlaumont2_1))$AveHumNumCanSyl_afontana5_1,nrep=1000,conf.level=.95)

# Human-human (awarlaumont2-afontana5) correlation in number of syllables per utterance at the day level
print(cor.test(subset(daydat,!is.na(AveHumNumAnySyl_afontana5_1)&!is.na(AveHumNumAnySyl_awarlaumont2_1))$AveHumNumAnySyl_awarlaumont2_1,subset(daydat,!is.na(AveHumNumAnySyl_afontana5_1)&!is.na(AveHumNumAnySyl_awarlaumont2_1))$AveHumNumAnySyl_afontana5_1,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumAnySyl_afontana5_1)&!is.na(AveHumNumAnySyl_awarlaumont2_1))$AveHumNumanySyl_awarlaumont2_1,subset(daydat,!is.na(AveHumNumAnySyl_afontana5_1)&!is.na(AveHumNumAnySyl_awarlaumont2_1))$AveHumNumAnySyl_afontana5_1,nrep=1000,conf.level=.95)

# Human-human (awarlaumont2-gmacedo) correlation in number of canonical syllables per utterance at the day level
print(cor.test(subset(daydat,!is.na(AveHumNumCanSyl_gmacedo_1)&!is.na(AveHumNumCanSyl_awarlaumont2_1))$AveHumNumCanSyl_awarlaumont2_1,subset(daydat,!is.na(AveHumNumCanSyl_gmacedo_1)&!is.na(AveHumNumCanSyl_awarlaumont2_1))$AveHumNumCanSyl_gmacedo_1,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumCanSyl_gmacedo_1)&!is.na(AveHumNumCanSyl_awarlaumont2_1))$AveHumNumCanSyl_awarlaumont2_1,subset(daydat,!is.na(AveHumNumCanSyl_gmacedo_1)&!is.na(AveHumNumCanSyl_awarlaumont2_1))$AveHumNumCanSyl_gmacedo_1,nrep=1000,conf.level=.95)

# Human-human (awarlaumont2-gmacedo) correlation in number of syllables per utterance at the day level
print(cor.test(subset(daydat,!is.na(AveHumNumAnySyl_gmacedo_1)&!is.na(AveHumNumAnySyl_awarlaumont2_1))$AveHumNumAnySyl_awarlaumont2_1,subset(daydat,!is.na(AveHumNumAnySyl_gmacedo_1)&!is.na(AveHumNumAnySyl_awarlaumont2_1))$AveHumNumAnySyl_gmacedo_1,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumAnySyl_gmacedo_1)&!is.na(AveHumNumAnySyl_awarlaumont2_1))$AveHumNumAnySyl_awarlaumont2_1,subset(daydat,!is.na(AveHumNumAnySyl_gmacedo_1)&!is.na(AveHumNumAnySyl_awarlaumont2_1))$AveHumNumAnySyl_gmacedo_1,nrep=1000,conf.level=.95)

# Human-human (gmacedo-afontana5) correlation in number of canonical syllables per utterance at the day level
print(cor.test(subset(daydat,!is.na(AveHumNumCanSyl_afontana5_1)&!is.na(AveHumNumCanSyl_gmacedo_1))$AveHumNumCanSyl_gmacedo_1,subset(daydat,!is.na(AveHumNumCanSyl_afontana5_1)&!is.na(AveHumNumCanSyl_gmacedo_1))$AveHumNumCanSyl_afontana5_1,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumCanSyl_afontana5_1)&!is.na(AveHumNumCanSyl_gmacedo_1))$AveHumNumCanSyl_gmacedo_1,subset(daydat,!is.na(AveHumNumCanSyl_afontana5_1)&!is.na(AveHumNumCanSyl_gmacedo_1))$AveHumNumCanSyl_afontana5_1,nrep=1000,conf.level=.95)

# Human-human (gmacedo-afontana5) correlation in number of canonical syllables per utterance at the day level
print(cor.test(subset(daydat,!is.na(AveHumNumAnySyl_afontana5_1)&!is.na(AveHumNumAnySyl_gmacedo_1))$AveHumNumAnySyl_gmacedo_1,subset(daydat,!is.na(AveHumNumAnySyl_afontana5_1)&!is.na(AveHumNumAnySyl_gmacedo_1))$AveHumNumAnySyl_afontana5_1,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumAnySyl_afontana5_1)&!is.na(AveHumNumAnySyl_gmacedo_1))$AveHumNumAnySyl_gmacedo_1,subset(daydat,!is.na(AveHumNumAnySyl_afontana5_1)&!is.na(AveHumNumAnySyl_gmacedo_1))$AveHumNumAnySyl_afontana5_1,nrep=1000,conf.level=.95)

# Day-level correlation between age and afontana5_1 human-judged canonical syllables per utterance
print(cor.test(daydat$AgeInDays,daydat$AveHumNumCanSyl_afontana5_1))
summary(lmer(scale(AgeInDays)~scale(AveHumNumCanSyl_afontana5_1)+(1|ID),dat=daydat))
summary(lmer(scale(AgeInDays)~scale(AveHumNumCanSyl_afontana5_1)+(scale(AveHumNumCanSyl_afontana5_1)|ID),dat=daydat))

# Day-level correlation between age and afontana5_1 human-judged total syllables per utterance
print(cor.test(daydat$AgeInDays,daydat$AveHumNumAnySyl_afontana5_1))
summary(lmer(scale(AgeInDays)~scale(AveHumNumAnySyl_afontana5_1)+(1|ID),dat=daydat))
summary(lmer(scale(AgeInDays)~scale(AveHumNumAnySyl_afontana5_1)+(scale(AveHumNumAnySyl_afontana5_1)|ID),dat=daydat))

# Day-level correlation between age and awarlaumont2_1 human-judged canonical syllables per utterance
print(cor.test(daydat$AgeInDays,daydat$AveHumNumCanSyl_awarlaumont2_1))
summary(lmer(scale(AgeInDays)~scale(AveHumNumCanSyl_awarlaumont2_1)+(1|ID),dat=subset(daydat,!is.na(AveHumNumCanSyl_awarlaumont2_1))))
summary(lmer(scale(AgeInDays)~scale(AveHumNumCanSyl_awarlaumont2_1)+(scale(AveHumNumCanSyl_awarlaumont2_1)|ID),dat=subset(daydat,!is.na(AveHumNumCanSyl_awarlaumont2_1))))

# Day-level correlation between age and awarlaumont2_1 human-judged total syllables per utterance
print(cor.test(daydat$AgeInDays,daydat$AveHumNumAnySyl_awarlaumont2_1))
summary(lmer(scale(AgeInDays)~scale(AveHumNumAnySyl_awarlaumont2_1)+(1|ID),dat=subset(daydat,!is.na(AveHumNumAnySyl_awarlaumont2_1))))
summary(lmer(scale(AgeInDays)~scale(AveHumNumAnySyl_awarlaumont2_1)+(scale(AveHumNumAnySyl_awarlaumont2_1)|ID),dat=subset(daydat,!is.na(AveHumNumAnySyl_awarlaumont2_1))))

# Day-level correlation between age and gmacedo_1 human-judged canonical syllables per utterance
print(cor.test(daydat$AgeInDays,daydat$AveHumNumCanSyl_gmacedo_1))
summary(lmer(scale(AgeInDays)~scale(AveHumNumCanSyl_gmacedo_1)+(1|ID),dat=subset(daydat,!is.na(AveHumNumCanSyl_gmacedo_1))))
summary(lmer(scale(AgeInDays)~scale(AveHumNumCanSyl_gmacedo_1)+(scale(AveHumNumCanSyl_gmacedo_1)|ID),dat=subset(daydat,!is.na(AveHumNumCanSyl_gmacedo_1))))

# Day-level correlation between age and gmacedo_1 human-judged total syllables per utterance
print(cor.test(daydat$AgeInDays,daydat$AveHumNumAnySyl_gmacedo_1))
summary(lmer(scale(AgeInDays)~scale(AveHumNumAnySyl_gmacedo_1)+(1|ID),dat=subset(daydat,!is.na(AveHumNumAnySyl_gmacedo_1))))
summary(lmer(scale(AgeInDays)~scale(AveHumNumAnySyl_gmacedo_1)+(scale(AveHumNumAnySyl_gmacedo_1)|ID),dat=subset(daydat,!is.na(AveHumNumAnySyl_gmacedo_1))))

# Day-level correlation between age and avg human-judged canonical syllables per utterance
print(cor.test(daydat$AgeInDays,daydat$AveHumNumCanSyl_avg))
summary(lmer(scale(AgeInDays)~scale(AveHumNumCanSyl_avg)+(1|ID),dat=subset(daydat,!is.na(AveHumNumCanSyl_avg))))
summary(lmer(scale(AgeInDays)~scale(AveHumNumCanSyl_avg)+(scale(AveHumNumCanSyl_avg)|ID),dat=subset(daydat,!is.na(AveHumNumCanSyl_avg))))

# Day-level correlation between age and avg human-judged total syllables per utterance
print(cor.test(daydat$AgeInDays,daydat$AveHumNumAnySyl_avg))
summary(lmer(scale(AgeInDays)~scale(AveHumNumAnySyl_avg)+(1|ID),dat=subset(daydat,!is.na(AveHumNumAnySyl_avg))))
summary(lmer(scale(AgeInDays)~scale(AveHumNumAnySyl_avg)+(scale(AveHumNumAnySyl_avg)|ID),dat=subset(daydat,!is.na(AveHumNumAnySyl_avg))))

# Day-level correlation between age and avg human-judged syllable ratio
print(cor.test(daydat$AgeInDays,daydat$AveHumNumCanSyl_avg/daydat$AveHumNumAnySyl_avg))
summary(lmer(scale(AgeInDays)~scale(AveHumNumCanSyl_avg/AveHumNumAnySyl_avg)+(1|ID),dat=subset(daydat,!is.na(AveHumNumAnySyl_avg))))
summary(lmer(scale(AgeInDays)~scale(AveHumNumCanSyl_avg/AveHumNumAnySyl_avg)+(scale(AveHumNumCanSyl_avg/AveHumNumAnySyl_avg)|ID),dat=subset(daydat,!is.na(AveHumNumAnySyl_avg))))

# Day-level correlation between age and machine-judged canonical syllables per utterance
print(cor.test(daydat$AgeInDays,daydat$AveLooCanSylEstPCAGAM))
summary(lmer(scale(AgeInDays)~scale(AveLooCanSylEstPCAGAM)+(1|ID),dat=subset(daydat,!is.na(AveLooCanSylEstPCAGAM))))
summary(lmer(scale(AgeInDays)~scale(AveLooCanSylEstPCAGAM)+(scale(AveLooCanSylEstPCAGAM)|ID),dat=subset(daydat,!is.na(AveLooCanSylEstPCAGAM))))

# Day-level correlation between age and machine-judged total syllables per utterance
print(cor.test(daydat$AgeInDays,daydat$AveLooAnySylEstPCAGAM))
summary(lmer(scale(AgeInDays)~scale(AveLooAnySylEstPCAGAM)+(1|ID),dat=subset(daydat,!is.na(AveLooAnySylEstPCAGAM))))
summary(lmer(scale(AgeInDays)~scale(AveLooAnySylEstPCAGAM)+(scale(AveLooAnySylEstPCAGAM)|ID),dat=subset(daydat,!is.na(AveLooAnySylEstPCAGAM))))

# Day-level correlation between age and machine-judged syllable ratio
print(cor.test(daydat$AgeInDays,daydat$AveLooCanSylEstPCAGAM/daydat$AveLooAnySylEstPCAGAM))
summary(lmer(scale(AgeInDays)~scale(AveLooCanSylEstPCAGAM/AveLooAnySylEstPCAGAM)+(1|ID),dat=subset(daydat,!is.na(AveLooAnySylEstPCAGAM))))
summary(lmer(scale(AgeInDays)~scale(AveLooCanSylEstPCAGAM/AveLooAnySylEstPCAGAM)+(scale(AveLooCanSylEstPCAGAM/AveLooAnySylEstPCAGAM)|ID),dat=subset(daydat,!is.na(AveLooAnySylEstPCAGAM))))

# Plot human-judged canonical syllables vs. age and machine-estimated canonical syllables vs. age
quartz(width=9,height=5)
par(mfrow=c(1,2))
plot(subset(daydat,!is.na(AveHumNumCanSyl_avg))$AgeInDays,subset(daydat,!is.na(AveHumNumCanSyl_avg))$AveHumNumCanSyl_avg,main="Human listener",xlab="Age in days",ylab="Human-judged canonical syllables per utterance") 
abline(lm(subset(daydat,!is.na(AveHumNumCanSyl_avg))$AveHumNumCanSyl_avg ~subset(daydat,!is.na(AveHumNumCanSyl_avg))$AgeInDays))
plot(daydat$AgeInDays,daydat$AveLooCanSylEstPCAGAM,main="Machine estimation",xlab="Age in days",ylab="Machine estimated canonical syllables per utterance");
abline(lm(daydat$AveLooCanSylEstPCAGAM ~daydat$AgeInDays))
if (nocrylauveg & nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_syl_vs_age_nocrylaughveg_nooverlap.pdf",type="pdf")
} else if (nocrylauveg & !nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_syl_vs_age_nocrylaughveg.pdf",type="pdf")
} else if (!nocrylauveg & nooverlap){	
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_syl_vs_age_nooverlap.pdf",type="pdf")
} else if (!nocrylauveg & !nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_syl_vs_age.pdf",type="pdf")
}

# Plot human-judged total syllables vs. age and machine-estimated total syllables vs. age
quartz(width=9,height=5)
par(mfrow=c(1,2))
plot(subset(daydat,!is.na(AveHumNumAnySyl_avg))$AgeInDays,subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumAnySyl_avg,main="Human listener",xlab="Age in days",ylab="Human-judged total syllables per utterance") 
abline(lm(subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumAnySyl_avg ~subset(daydat,!is.na(AveHumNumAnySyl_avg))$AgeInDays))
plot(daydat$AgeInDays,daydat$AveLooAnySylEstPCAGAM,main="Machine estimation",xlab="Age in days",ylab="Machine estimated total syllables per utterance");
abline(lm(daydat$AveLooAnySylEstPCAGAM ~daydat$AgeInDays))
if (nocrylauveg & nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_totsyl_vs_age_nocrylaughveg_nooverlap.pdf",type="pdf")
} else if (nocrylauveg & !nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_totsyl_vs_age_nocrylaughveg.pdf",type="pdf")
} else if (!nocrylauveg & nooverlap){	
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_totsyl_vs_age_nooverlap.pdf",type="pdf")
} else if (!nocrylauveg & !nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_totsyl_vs_age.pdf",type="pdf")
}

# Plot human-judged canonical-to-all-syllables ratio vs. age and machine-estimated canonical-to-all-syllables ratio vs. age
quartz(width=9,height=5)
par(mfrow=c(1,2))
plot(subset(daydat,!is.na(AveHumNumAnySyl_avg))$AgeInDays,subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumCanSyl_avg/subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumAnySyl_avg,main="Human listener",xlab="Age in days",ylab="Human-judged total syllables per utterance") 
abline(lm(subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumCanSyl_avg/subset(daydat,!is.na(AveHumNumAnySyl_avg))$AveHumNumAnySyl_avg ~subset(daydat,!is.na(AveHumNumAnySyl_avg))$AgeInDays))
plot(daydat$AgeInDays,daydat$AveLooCanSylEstPCAGAM/daydat$AveLooAnySylEstPCAGAM,main="Machine estimation",xlab="Age in days",ylab="Machine estimated canonical-to-all-syllables ratio");
abline(lm(daydat$AveLooCanSylEstPCAGAM/daydat$AveLooAnySylEstPCAGAM ~daydat$AgeInDays))
if (nocrylauveg & nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_sylratio_vs_age_nocrylaughveg_nooverlap.pdf",type="pdf")
} else if (nocrylauveg & !nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_sylratio_vs_age_nocrylaughveg.pdf",type="pdf")
} else if (!nocrylauveg & nooverlap){	
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_sylratio_vs_age_nooverlap.pdf",type="pdf")
} else if (!nocrylauveg & !nooverlap){
	quartz.save("~/RamsdellWarlaumontCollab/StatisticalResults/hum_and_mach_sylratio_vs_age.pdf",type="pdf")
}