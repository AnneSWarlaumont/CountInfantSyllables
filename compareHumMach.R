library(lme4)
library(lmerTest)
library(mgcv)
library(RVAideMemoire)
library(caret)
library(e1071)

vocdat = read.csv('~/RamsdellWarlaumontCollab/vocdat.csv',header=T)

# Remove the rows where the human listener entered "x".
# "x" was entered when the listener thought the utterance came from someone other than the kid.
vocdat = subset(vocdat,((humnumanysyl_awarlaumont2_1!="x")|is.na(humnumanysyl_awarlaumont2_1))&((humnumcansyl_awarlaumont2_1!="x")|is.na(humnumcansyl_awarlaumont2_1))&((humnumanysyl_afontana5_1!="x")|is.na(humnumanysyl_afontana5_1))&((humnumcansyl_afontana5_1!="x")|is.na(humnumcansyl_afontana5_1)))

vocdat$humnumanysyl_awarlaumont2_1 = as.numeric(as.character(vocdat$humnumanysyl_awarlaumont2_1))
vocdat$humnumcansyl_awarlaumont2_1 = as.numeric(as.character(vocdat$humnumcansyl_awarlaumont2_1))
vocdat$humnumanysyl_afontana5_1 = as.numeric(as.character(vocdat$humnumanysyl_afontana5_1))
vocdat$humnumcansyl_afontana5_1 = as.numeric(as.character(vocdat$humnumcansyl_afontana5_1))

vocdat$humnumcansyl_afontana5_1_categorical = vocdat$humnumcansyl_afontana5_1
vocdat$humnumcansyl_afontana5_1_categorical[vocdat$humnumcansyl_afontana5_1>2] = 3

vocdat$DurS = vocdat$DurMs/1000

vocdat$numlmksyl[is.na(vocdat$numlmksyl)] = 0

# Get correlations between each measure and age
cor.test(vocdat$numsalons,vocdat$AgeInDays,method="spearman",exact=F)
cor.test(vocdat$numlmksyl,vocdat$AgeInDays,method="spearman",exact=F)
cor.test(vocdat$numdjwsyl,vocdat$AgeInDays,method="spearman",exact=F)
cor.test(vocdat$humnumanysyl_awarlaumont2_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1/vocdat$humnumanysyl_awarlaumont2_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1/vocdat$DurS,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumanysyl_afontana5_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_afontana5_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_afontana5_1/vocdat$humnumanysyl_afontana5_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_afontana5_1/vocdat$DurS,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)

# # Get correlations between the automated measures and the human judgments
cor.test(vocdat$numsalons,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)

# Get awarlaumont2_1 and afontana5_1 interrater correlations
cor.test(vocdat$humnumanysyl_awarlaumont2_1,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
nrow(subset(vocdat,!is.na(humnumcansyl_awarlaumont2_1)&!is.na(humnumcansyl_afontana5_1))) # How many points were included in the above interrater correlations?

# Get day-level data
daydat = data.frame(matrix(NA,nrow=1,ncol=13))
names(daydat) = c('ID','AgeInDays','AveDurMs','AveDurS','AveNumSalOns','AveNumLmkSyl','AveNumDjwSyl','AveHumNumAnySyl_awarlaumont2_1','AveHumNumCanSyl_awarlaumont2_1','AveHumNumAnySyl_afontana5_1','AveHumNumCanSyl_afontana5_1')
daycnt = 1
for (id in levels(vocdat$ID)){
	childdat = subset(vocdat,ID==id)
	for (age in levels(as.factor(childdat$AgeInDays))){	
		if (daycnt > 1){
			daydat = rbind(daydat,setNames(as.data.frame(matrix(NA,nrow=1,ncol=13)),names(daydat)))
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
		daydat$AveHumNumAnySyl_awarlaumont2_1[daycnt] = mean(subvocdat$humnumanysyl_awarlaumont2_1,na.rm=T)
		daydat
		daycnt = daycnt + 1
	}
}

# Get leave-one-child-out cross-validation predictions
# Pilot work showed gam to be better than pca and svr
# Pilot work showed equalizing category counts to provide better results

#Initialize the columns that will hold the cross-validation predictions
vocdat$looanysylestpcagam_afontana5_1 = rep(NA,nrow(vocdat)) # PCA predicting total syllable count
vocdat$loocansylestpcagam_afontana5_1 = rep(NA,nrow(vocdat)) # PCA predicting canonical syllable count
vocdat$looanysylestsalgam_afontana5_1 = rep(NA,nrow(vocdat)) # Salience predicting total syllable count
vocdat$looanysylestlmkgam_afontana5_1 = rep(NA,nrow(vocdat)) # Salience predicting canonical syllable count
vocdat$looanysylestdjwgam_afontana5_1 = rep(NA,nrow(vocdat)) # de Jong & Wempe predicting total syllable count
vocdat$loocansylestsalgam_afontana5_1 = rep(NA,nrow(vocdat)) # de Jong & Wempe predicting canonical syllable count
vocdat$loocansylestlmkgam_afontana5_1 = rep(NA,nrow(vocdat)) # Landmarks predictiong total syllable count
vocdat$loocansylestdjwgam_afontana5_1 = rep(NA,nrow(vocdat)) # Landmarks predicting canonical syllable count

for (id in levels(vocdat$ID)){ # for each child to be left out
	
	# For every # of syllables except the # of syllables with the most frequent exemplars, resample until that # of syllables has the same number of exemplars as the # of exemplars in the max # of syllables category.
	eqtraindat = subset(vocdat,(ID!=id) & !is.na(humnumcansyl_afontana5_1_categorical))
	maxexemplars = max(length(eqtraindat$humnumcansyl_afontana5_1_categorical==0),length(eqtraindat$humnumcansyl_afontana5_1_categorical==1),length(eqtraindat$humnumcansyl_afontana5_1_categorical==2),length(eqtraindat$humnumcansyl_afontana5_1_categorical==3))
	# Resample so all categories have the same number of exemplars
	for (sylnum in c(0,1,2,3)){
		exemplarnumdif = maxexemplars - length(eqtraindat$humnumcansyl_afontana5_1_categorical==sylnum)
		sylnumind = which(eqtraindat$humnumcansyl_afontana5_1_categorical %in% sylnum)
		newexemplars = sample(sylnumind,size = exemplarnumdif, replace = T)
		eqtraindat = rbind(eqtraindat,eqtraindat[newexemplars,])
	}
	tempPreProcFunction = preProcess(eqtraindat[,c('DurMs','numsalons','numlmksyl','numdjwsyl')],method=c('BoxCox','center','scale','pca'))
	trainPreProcValues = predict(tempPreProcFunction,eqtraindat[,c('DurMs','numsalons','numlmksyl','numdjwsyl')])
	testPreProcValues = predict(tempPreProcFunction,subset(vocdat,(ID==id))[,c('DurMs','numsalons','numlmksyl','numdjwsyl')])
	traindat = trainPreProcValues
	traindat$humnumcansyl_afontana5_1_categorical = eqtraindat$humnumcansyl_afontana5_1_categorical
	loocansylestpcagam_afontana5_1 = gam(humnumcansyl_afontana5_1_categorical ~ PC1 + PC2 + PC3, dat = traindat)
	vocdat$loocansylestpcagam_afontana5_1[vocdat$ID==id] = predict(loocansylestpcagam_afontana5_1,newdata = testPreProcValues,type="response")
	
}

print(cor.test(subset(vocdat,!is.na(humnumsyl2))$humnumsyl,subset(vocdat,!is.na(humnumsyl2))$looautsylestgam,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humnumsyl2))$humnumsyl,subset(vocdat,!is.na(humnumsyl2))$looautsylestgam,nrep=1000,conf.level=.95)

print(cor.test(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$looautsylestgam,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$looautsylestgam,nrep=1000,conf.level=.95)

print(cor.test(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$loosylestpcagam,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$loosylestpcagam,nrep=1000,conf.level=.95)

print(cor.test(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$loosylestpcaglm,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$loosylestpcaglm,nrep=1000,conf.level=.95)

print(cor.test(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$loosylestpcasvr,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$loosylestpcasvr,nrep=1000,conf.level=.95)

print(cor.test(subset(vocdat,!is.na(humnumsyl2))$humnumsyl,subset(vocdat,!is.na(humnumsyl2))$humnumsyl2,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humnumsyl2))$humnumsyl,subset(vocdat,!is.na(humnumsyl2))$humnumsyl2,nrep=1000,conf.level=.95)

print(cor.test(subset(vocdat,!is.na(humrhy))$humrhy,subset(vocdat,!is.na(humrhy))$looautrhyestgam,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humrhy))$humrhy,subset(vocdat,!is.na(humrhy))$looautrhyestgam,nrep=1000,conf.level=.95)

print(cor.test(subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$humnumcansyl_afontana5_1_categorical,subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$loosylestpcagam_afontana5_1,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$humnumcansyl_afontana5_1_categorical,subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$loosylestpcagam_afontana5_1,nrep=1000,conf.level=.95)

print(cor.test(subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$humnumcansyl_afontana5_1_categorical,subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$loosylestpcagameq,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$humnumcansyl_afontana5_1_categorical,subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$loosylestpcagameq,nrep=1000,conf.level=.95)

# Predict the human2-judged number of canonical syllables based on the automatically estimated number of syllables
print(cor.test(subset(vocdat,!is.na(hum2numcansyl))$hum2numcansyl,subset(vocdat,!is.na(hum2numcansyl))$looauthum2cansylestgam,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(hum2numcansyl))$hum2numcansyl,subset(vocdat,!is.na(hum2numcansyl))$looauthum2cansylestgam,nrep=1000,conf.level=.95)

# Predict the human2-judged number of syllables of any type based on the automatically estimated number of syllables of any type
print(cor.test(subset(vocdat,!is.na(hum2numanysyl))$hum2numanysyl,subset(vocdat,!is.na(hum2numanysyl))$looauthum2anysylestgam,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(hum2numanysyl))$hum2numanysyl,subset(vocdat,!is.na(hum2numanysyl))$looauthum2anysylestgam,nrep=1000,conf.level=.95)

# Predict the human2-judged canonical to any syllable ratio based on the automatically estimated canonical to any syllable ratio
print(cor.test(subset(vocdat,!is.na(hum2numcansyl))$hum2numcansyl/subset(vocdat,!is.na(hum2numanysyl))$hum2numanysyl,subset(vocdat,!is.na(hum2numcansyl))$looauthum2cananysylestgam,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(hum2numcansyl))$hum2numcansyl/subset(vocdat,!is.na(hum2numanysyl))$hum2numanysyl,subset(vocdat,!is.na(hum2numcansyl))$looauthum2cananysylestgam,nrep=1000,conf.level=.95)

# Predict the human2-judged canonical to any syllable ratio based on the automatically estimated number of canonical syllables divided by the automatically estimated number of syllables of any time
print(cor.test(subset(vocdat,!is.na(hum2numcansyl))$hum2numcansyl/subset(vocdat,!is.na(hum2numanysyl))$hum2numanysyl,subset(vocdat,!is.na(hum2numcansyl))$looauthum2cansylestgam/subset(vocdat,!is.na(hum2numanysyl))$looauthum2anysylestgam,method="spearman",exact=FALSE))
spearman.ci(subset(vocdat,!is.na(hum2numcansyl))$hum2numcansyl/subset(vocdat,!is.na(hum2numanysyl))$hum2numanysyl,subset(vocdat,!is.na(hum2numcansyl))$looauthum2cansylestgam/subset(vocdat,!is.na(hum2numanysyl))$looauthum2anysylestgam,nrep=1000,conf.level=.95)

axmax = ceiling(max(c(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl2))$humnumsyl2,subset(vocdat,!is.na(humnumsyl))$looautsylestgam)))
quartz(width=9,height=5)
par(mfrow=c(1,2))
# Might need to fix xlim in the plots below, now that I've fixed the conversion from character to factor so that 0 doesn't map onto 1 but to 0 etc.
boxplot(humnumsyl2~factor(humnumsyl,levels=0:axmax),data=subset(vocdat,!is.na(humnumsyl2)),ylim=c(0,axmax),xlim=c(.5,axmax+.5),main="Human vs. human syllable counts",xlab="Human canonical syllable count",ylab="Human coder's second canonical syllable count")
boxplot(looautsylestgam~factor(humnumsyl,levels=0:axmax),data=subset(vocdat,!is.na(humnumsyl2)),ylim=c(0,axmax),xlim=c(.5,axmax+.5),main="Machine vs. human syllable counts",xlab="Human canonical syllable count",ylab="Machine estimated number of canonical syllables")
# quartz.save("~/RamsdellWarlaumontCollab/StatisticalAnalyses/hum_vs_mach_canonicalsyllablecounts.pdf",type="pdf")

axmax = ceiling(max(c(subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$humnumcansyl_afontana5_1_categorical,subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$loosylestpcagam_afontana5_1,subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical))$loosylestpcagameq)))
quartz(width=9,height=5)
par(mfrow=c(1,2))
# Might need to fix xlim in the plots below, now that I've fixed the conversion from character to factor so that 0 doesn't map onto 1 but to 0 etc.
boxplot(loosylestpcagam_afontana5_1 ~factor(humnumcansyl_afontana5_1_categorical,levels=0:axmax),data=subset(vocdat,!is.na(loosylestpcagam_afontana5_1)),ylim=c(0,axmax),xlim=c(.5,4.5),main="PCA GAM vs. human syllable counts",xlab="Human canonical syllable count",ylab="PCA GAM's estimated canonical syllable count")
boxplot(loosylestpcagameq~factor(humnumcansyl_afontana5_1_categorical,levels=0:axmax),data=subset(vocdat,!is.na(humnumcansyl_afontana5_1_categorical)),ylim=c(0,axmax),xlim=c(.5,4.5),main="PCA GAM equal categories vs. human syllable counts",xlab="Human canonical syllable count",ylab="PCA GAM equal categories' estimated number of canonical syllables")

print(cor.test(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$loosalsylest,method="spearman",exact=FALSE))
print(cor.test(subset(vocdat,!is.na(humrhy))$humrhy,subset(vocdat,!is.na(humrhy))$loosalsylest,method="spearman",exact=FALSE))

print(cor.test(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$loolmksylest,method="spearman",exact=FALSE))
print(cor.test(subset(vocdat,!is.na(humrhy))$humrhy,subset(vocdat,!is.na(humrhy))$loolmksylest,method="spearman",exact=FALSE))

print(cor.test(subset(vocdat,!is.na(humnumsyl))$humnumsyl,subset(vocdat,!is.na(humnumsyl))$loodjwsylest,method="spearman",exact=FALSE))
print(cor.test(subset(vocdat,!is.na(humrhy))$humrhy,subset(vocdat,!is.na(humrhy))$loodjwsylest,method="spearman",exact=FALSE))

# Human-machine correlation in number of syllables per utterance at the day level.
print(cor.test(subset(daydat,!is.na(AveHumNumSyl))$looautsylest,subset(daydat,!is.na(AveHumNumSyl))$AveHumNumSyl,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumSyl))$looautsylest,subset(daydat,!is.na(AveHumNumSyl))$AveHumNumSyl,nrep=1000,conf.level=.95)

# Human-machine correlation in number of syllables per second at the day level.
print(cor.test(subset(daydat,!is.na(AveHumNumSyl))$looautsylpersest,subset(daydat,!is.na(AveHumNumSyl))$AveHumNumSyl/subset(daydat,!is.na(AveHumNumSyl))$AveDurS,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumSyl))$looautsylpersest,subset(daydat,!is.na(AveHumNumSyl))$AveHumNumSyl/subset(daydat,!is.na(AveHumNumSyl))$AveDurS,nrep=1000,conf.level=.95)

# Human-human intrarater correlation in number of syllables per second at the day level. Probably not enough re-test syllable judgments to make this a stable estimate.
print(cor.test(subset(daydat,!is.na(AveHumNumSyl2))$AveHumNumSyl/subset(daydat,!is.na(AveHumNumSyl2))$AveDurS,subset(daydat,!is.na(AveHumNumSyl2))$AveHumNumSyl2/subset(daydat,!is.na(AveHumNumSyl2))$AveDurS,method="spearman",exact=FALSE))
spearman.ci(subset(daydat,!is.na(AveHumNumSyl2))$AveHumNumSyl/subset(daydat,!is.na(AveHumNumSyl2))$AveDurS,subset(daydat,!is.na(AveHumNumSyl2))$AveHumNumSyl2/subset(daydat,!is.na(AveHumNumSyl2))$AveDurS,nrep=1000,conf.level=.95)

print(cor.test(daydat$AgeInDays,daydat$looautsylest))
summary(lmer(scale(AgeInDays)~scale(looautsylest)+(1|ID),dat=daydat))
summary(lmer(scale(AgeInDays)~scale(looautsylest)+(scale(looautsylest)|ID),dat=daydat))

print(cor.test(daydat$AgeInDays,daydat$AveHumNumSyl))
summary(lmer(scale(AgeInDays)~scale(AveHumNumSyl)+(1|ID),dat=daydat))
summary(lmer(scale(AgeInDays)~scale(AveHumNumSyl)+(scale(AveHumNumSyl)|ID),dat=daydat))

print(cor.test(daydat$AgeInDays,daydat$looautsylpersest))
summary(lmer(scale(AgeInDays)~scale(looautsylpersest)+(1|ID),dat=daydat))
summary(lmer(scale(AgeInDays)~scale(looautsylpersest)+(scale(looautsylpersest)|ID),dat=daydat))

print(cor.test(daydat$AgeInDays,daydat$looautsylest/daydat$AveDurS))
summary(lmer(scale(AgeInDays)~scale(looautsylest/AveDurS)+(1|ID),dat=daydat))
summary(lmer(scale(AgeInDays)~scale(looautsylest/AveDurS)+(scale(looautsylest/AveDurS)|ID),dat=daydat))

print(cor.test(subset(daydat,!is.na(AveHumNumSyl))$AgeInDays,subset(daydat,!is.na(AveHumNumSyl))$AveHumNumSyl/subset(daydat,!is.na(AveHumNumSyl))$AveDurS))
summary(lmer(scale(AgeInDays)~scale(AveHumNumSyl/AveDurS)+(1|ID),dat=subset(daydat,!is.na(AveHumNumSyl))))
summary(lmer(scale(AgeInDays)~scale(AveHumNumSyl/AveDurS)+(scale(AveHumNumSyl/AveDurS)|ID),dat=subset(daydat,!is.na(AveHumNumSyl))))

# Add titles and axis labels to the plots below.
quartz(width=9,height=5)
par(mfrow=c(1,2))
plot(subset(daydat,!is.na(AveHumNumSyl))$AgeInDays,subset(daydat,!is.na(AveHumNumSyl))$AveHumNumSyl,main="Human listener",xlab="Age in days",ylab="Human-judged canonical syllables per utterance") 
abline(lm(subset(daydat,!is.na(AveHumNumSyl))$AveHumNumSyl~subset(daydat,!is.na(AveHumNumSyl))$AgeInDays))
plot(daydat$AgeInDays,daydat$looautsylest,main="Machine estimation",xlab="Age in days",ylab="Machine estimated canonical syllables per utterance");
abline(lm(daydat$looautsylest~daydat$AgeInDays))
quartz.save("~/RamsdellWarlaumontCollab/StatisticalAnalyses/hum_and_mach_syl_vs_age.pdf",type="pdf")

quartz(width=13)
par(mfrow=c(1,2))
plot(subset(daydat,!is.na(AveHumNumSyl))$AgeInDays,subset(daydat,!is.na(AveHumNumSyl))$AveHumNumSyl/subset(daydat,!is.na(AveHumNumSyl))$AveDurS); abline(lm(subset(daydat,!is.na(AveHumNumSyl))$AveHumNumSyl/subset(daydat,!is.na(AveHumNumSyl))$AveDurS~subset(daydat,!is.na(AveHumNumSyl))$AgeInDays))
plot(daydat$AgeInDays,daydat$looautsylpersest); abline(lm(daydat$looautsylpersest~daydat$AgeInDays))
