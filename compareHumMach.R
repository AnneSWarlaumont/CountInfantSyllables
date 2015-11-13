library(lme4)
library(lmerTest)
library(mgcv)
library(RVAideMemoire)
library(caret)
library(e1071)

vocdat = read.csv('~/RamsdellWarlaumontCollab/vocdat.csv',header=T)

# Remove the rows where the human listener entered "x".
# "x" was entered when the listener thought the utterance came from someone other than the kid.
vocdat = subset(vocdat,((humnumsyl!="x")|is.na(humnumsyl))&((humnumsyl2!="x")|is.na(humnumsyl2))&((humrhy!="x")|is.na(humrhy))&((humrhy2!="x")|is.na(humrhy2))&((hum2numanysyl!="x")|is.na(hum2numanysyl))&((hum2numcansyl!="x")|is.na(hum2numcansyl))&((humnumanysyl_awarlaumont2_1!="x")|is.na(humnumanysyl_awarlaumont2_1))&((humnumcansyl_awarlaumont2_1!="x")|is.na(humnumcansyl_awarlaumont2_1))&((humnumanysyl_afontana5_1!="x")|is.na(humnumanysyl_afontana5_1))&((humnumcansyl_afontana5_1!="x")|is.na(humnumcansyl_afontana5_1)))

vocdat$humnumsyl = as.numeric(as.character(vocdat$humnumsyl))
vocdat$humnumsyl2 = as.numeric(as.character(vocdat$humnumsyl2))
vocdat$humrhy = as.numeric(as.character(vocdat$humrhy))
vocdat$humrhy2 = as.numeric(as.character(vocdat$humrhy2))
vocdat$hum2numanysyl = as.numeric(as.character(vocdat$hum2numanysyl))
vocdat$hum2numcansyl = as.numeric(as.character(vocdat$hum2numcansyl))
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
cor.test(vocdat$humrhy,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumsyl,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$hum2numanysyl,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$hum2numcansyl,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$hum2numcansyl/vocdat$hum2numanysyl,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$hum2numcansyl/vocdat$DurS,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumanysyl_awarlaumont2_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1/vocdat$humnumanysyl_awarlaumont2_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1/vocdat$DurS,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumanysyl_afontana5_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_afontana5_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_afontana5_1/vocdat$humnumanysyl_afontana5_1,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_afontana5_1/vocdat$DurS,vocdat$AgeInDays,method="spearman",exact=F,na.rm=T)

# # Get correlations between the automated measures and the human judgments
cor.test(vocdat$numsalons,vocdat$humrhy,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumsyl,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numsalons,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humrhy,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumsyl,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numlmksyl,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humrhy,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumsyl,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$numdjwsyl,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)

# # Get correlation between the human-judged rhythmicity and the human-judged number of syllables
# cor.test(vocdat$humrhy,vocdat$humnumsyl,method="spearman",exact=F,na.rm=T)

# Get correlation between the human2-judged number of syllables of any type and the human2-judged number of canonical syllables
cor.test(vocdat$hum2numanysyl,vocdat$hum2numcansyl,method="spearman",exact=F,na.rm=T)

# # Get human intrarater correlations
cor.test(vocdat$humrhy,vocdat$humrhy2,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumsyl,vocdat$humnumsyl2,method="spearman",exact=F,na.rm=T)

# Get human and human2 interrater correlations
cor.test(vocdat$humrhy,vocdat$hum2numanysyl,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumsyl,vocdat$hum2numcansyl,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumsyl,vocdat$hum2numanysyl,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humrhy,vocdat$hum2numcansyl,method="spearman",exact=F,na.rm=T)
nrow(subset(vocdat,!is.na(humrhy)&!is.na(hum2numanysyl))) # How many points were included in the above interrater correlations?

# Get awarlaumont2_1 and afontana5_1 interrater correlations
cor.test(vocdat$humnumanysyl_awarlaumont2_1,vocdat$humnumanysyl_afontana5_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$humnumcansyl_awarlaumont2_1,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
nrow(subset(vocdat,!is.na(humnumcansyl_awarlaumont2_1)&!is.na(humnumcansyl_afontana5_1))) # How many points were included in the above interrater correlations?

# Get human (1) and afontana5_1 interrater correlations
cor.test(vocdat$humnumsyl,vocdat$humnumcansyl_afontana5_1,method="spearman",exact=F,na.rm=T)
nrow(subset(vocdat,!is.na(humnumsyl)&!is.na(humnumcansyl_afontana5_1))) # How many points were included in the above interrater correlation?

# Get human (1) and awarlaumont2_1 interrater correlations
cor.test(vocdat$humnumsyl,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
nrow(subset(vocdat,!is.na(humnumsyl)&!is.na(humnumcansyl_awarlaumont2_1))) # How many points were included in the above interrater correlation?

# Get human2 and awarlaumont2_1 intrarater correlations
cor.test(vocdat$hum2numanysyl,vocdat$humnumanysyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
cor.test(vocdat$hum2numcansyl,vocdat$humnumcansyl_awarlaumont2_1,method="spearman",exact=F,na.rm=T)
nrow(subset(vocdat,!is.na(hum2numcansyl)&!is.na(humnumcansyl_awarlaumont2_1))) # How many points were included in the above intrarater correlations?

# Get day-level data
daydat = data.frame(matrix(NA,nrow=1,ncol=13))
names(daydat) = c('ID','AgeInDays','AveDurMs','AveDurS','AveNumSalOns','AveNumLmkSyl','AveNumDjwSyl','AveHumNumSyl','AveHumNumSyl2','AveHumRhy','AveHumRhy2','AveHum2NumCanSyl','AveHum2NumAnySyl')
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
		daydat$AveHumNumSyl[daycnt] = mean(subvocdat$humnumsyl,na.rm=T)
		daydat$AveHumNumSyl2[daycnt] = mean(subvocdat$humnumsyl2,na.rm=T)
		daydat$AveHumRhy[daycnt] = mean(subvocdat$humrhy,na.rm=T)
		daydat$AveHumRhy2[daycnt] = mean(subvocdat$humrhy2,na.rm=T)
		daydat$AveHum2NumCanSyl[daycnt] = mean(subvocdat$hum2numcansyl,na.rm=T)
		daydat$AveHum2NumAnySyl[daycnt] = mean(subvocdat$hum2numanysyl,na.rm=T)
		daydat
		daycnt = daycnt + 1
	}
}

# # Predict age on date of recording

# dayautsyllm = lmer(scale(AgeInDays)~scale(AveNumSalOns)+scale(AveNumLmkSyl)+scale(AveNumDjwSyl)+(1|ID),dat=daydat)
# summary(dayautsyllm)
# quartz(); plot(predict(dayautsyllm),daydat$AgeInDays)

# dayhumsyllm = lmer(scale(AgeInDays)~scale(AveHumNumSyl)+(1|ID),dat=daydat)
# summary(dayhumsyllm)
# quartz(); plot(predict(dayhumsyllm),daydat$AgeInDays)

# dayhum2cansyllm = lmer(scale(AgeInDays)~scale(AveHum2NumCanSyl)+(1|ID),dat=daydat)
# summary(dayhum2cansyllm)
# quartz(); plot(predict(dayhum2cansyllm),daydat$AgeInDays)


# # Predict age on date of recording based on average duration of vocalizations on that day

# daydurlm = lmer(scale(AgeInDays)~scale(AveDurS)+(1|ID),dat=daydat)
# summary(daydurlm)
# quartz(); plot(predict(daydurlm),daydat$AgeInDays)


# # Predict age on date of recording based on average syllables divided by average duration of utterance on that date

# dayautsylperslm = lmer(scale(AgeInDays)~scale(AveNumSalOns/AveDurS)+scale(AveNumLmkSyl/AveDurS)+scale(AveNumDjwSyl/AveDurS) + (1|ID),dat=daydat)
# summary(dayautsylperslm)
# quartz(); plot(predict(dayautsylperslm),daydat$AgeInDays)

# #Including random slopes:
# dayautsylperslm = lmer(scale(AgeInDays)~scale(AveNumSalOns/AveDurS)+scale(AveNumLmkSyl/AveDurS)+scale(AveNumDjwSyl/AveDurS) + (scale(AveNumSalOns/AveDurS)|ID) + (scale(AveNumLmkSyl/AveDurS)|ID) + (scale(AveNumDjwSyl/AveDurS)|ID),dat=daydat)
# summary(dayautsylperslm)
# quartz(); plot(predict(dayautsylperslm),daydat$AgeInDays)

# dayhumsylperslm = lmer(scale(AgeInDays)~scale(AveHumNumSyl/AveDurS) + (1|ID),dat=daydat)
# summary(dayhumsylperslm)
# quartz(); plot(predict(dayhumsylperslm),daydat$AgeInDays)

# # Including random slopes:
# dayhumsylperslm = lmer(scale(AgeInDays)~scale(AveHumNumSyl/AveDurS) + (scale(AveHumNumSyl/AveDurS)|ID),dat=daydat)
# summary(dayhumsylperslm)
# quartz(); plot(predict(dayhumsylperslm),daydat$AgeInDays)


# Get leave-one-child-out cross-validation predictions

vocdat$looautsylest = rep(NA,nrow(vocdat)) #Initialize the column that will hold the cross-validation predictions and the actual values

vocdat$looautsylestgam = rep(NA,nrow(vocdat))
vocdat$looautrhyestgam = rep(NA,nrow(vocdat))

vocdat$loosalsylest = rep(NA,nrow(vocdat))
vocdat$loolmksylest = rep(NA,nrow(vocdat))
vocdat$loodjwsylest = rep(NA,nrow(vocdat))

vocdat$looauthum2cansylestgam = rep(NA,nrow(vocdat))
vocdat$looauthum2anysylestgam = rep(NA,nrow(vocdat))
vocdat$looauthum2cananysylestgam = rep(NA,nrow(vocdat))

daydat$looautsylest = rep(NA,nrow(daydat))
daydat$looautsylpersest = rep(NA,nrow(daydat))

vocdat$loosylestpcaglm = rep(NA,nrow(vocdat))
vocdat$loosylestpcagam = rep(NA,nrow(vocdat))
vocdat$loosylestpcasvr = rep(NA,nrow(vocdat))

vocdat$loosylestpcagam_afontana5_1 = rep(NA,nrow(vocdat))

vocdat$loosylestpcagameq = rep(NA,nrow(vocdat))

for (id in levels(vocdat$ID)){ # for each child

	# looautsyllm = glm(humnumsyl~numsalons+numlmksyl+numdjwsyl,dat=subset(vocdat,(ID!=id) & !is.na(humnumsyl)),family=poisson)
	# pchisq(summary(looautsyllm)$deviance,summary(looautsyllm)$df.residual) # Test that Poisson model assumptions are met: http://datavoreconsulting.com/programming-tips/count-data-glms-choosing-poisson-negative-binomial-zero-inflated-poisson/
	# vocdat$looautsylest[vocdat$ID==id] = predict(looautsyllm,newdata = subset(vocdat,ID==id), type = "response")

	# looautsylgam = gam(humnumsyl~numsalons+numlmksyl+numdjwsyl,dat=subset(vocdat,(ID!=id) & !is.na(humnumsyl)))
	# vocdat$looautsylestgam[vocdat$ID==id] = predict(looautsylgam,newdata = subset(vocdat,ID==id), type = "response")

	loosalsyllm = glm(humnumsyl~numsalons,dat=subset(vocdat,(ID!=id) & !is.na(humnumsyl)),family=poisson)
	vocdat$loosalsylest[vocdat$ID==id] = predict(loosalsyllm,newdata = subset(vocdat,ID==id), type = "response")
	
	loolmksyllm = glm(humnumsyl~numlmksyl,dat=subset(vocdat,(ID!=id) & !is.na(humnumsyl)),family=poisson)
	vocdat$loolmksylest[vocdat$ID==id] = predict(loolmksyllm,newdata = subset(vocdat,ID==id), type = "response")
	
	loodjwsyllm = glm(humnumsyl~numdjwsyl,dat=subset(vocdat,(ID!=id) & !is.na(humnumsyl)),family=poisson)
	vocdat$loodjwsylest[vocdat$ID==id] = predict(loodjwsyllm,newdata = subset(vocdat,ID==id), type = "response")
	
	# loodayautsyllm = lm(AveHumNumSyl~scale(AveNumSalOns)+scale(AveNumLmkSyl)+scale(AveNumDjwSyl),dat=subset(daydat,ID!=id))
	# daydat$looautsylest[daydat$ID==id] = predict(loodayautsyllm,newdata = subset(daydat,ID==id), type = "response")
	
	# loodayautsylperslm = lm(AveHumNumSyl/AveDurS~scale(AveNumSalOns/AveDurS)+scale(AveNumLmkSyl/AveDurS)+scale(AveNumDjwSyl/AveDurS),dat=subset(daydat,ID!=id))
	# daydat$looautsylpersest[daydat$ID==id] = predict(loodayautsylperslm,newdata = subset(daydat,ID==id), type = "response")
	
	looautsyllm = glm(humnumsyl~numsalons+numlmksyl+numdjwsyl+DurS,dat=subset(vocdat,(ID!=id) & !is.na(humnumsyl)),family=poisson)
	pchisq(summary(looautsyllm)$deviance,summary(looautsyllm)$df.residual) # Test that Poisson model assumptions are met: http://datavoreconsulting.com/programming-tips/count-data-glms-choosing-poisson-negative-binomial-zero-inflated-poisson/
	vocdat$looautsylest[vocdat$ID==id] = predict(looautsyllm,newdata = subset(vocdat,ID==id), type = "response")

	looautsylgam = gam(humnumsyl~numsalons+numlmksyl+numdjwsyl+DurS,dat=subset(vocdat,(ID!=id) & !is.na(humnumsyl)))
	vocdat$looautsylestgam[vocdat$ID==id] = predict(looautsylgam,newdata = subset(vocdat,ID==id), type = "response")
	
	looautrhygam = gam(humrhy~numsalons+numlmksyl+numdjwsyl+DurS,dat=subset(vocdat,(ID!=id) & !is.na(humrhy)))
	vocdat$looautrhyestgam[vocdat$ID==id] = predict(looautrhygam,newdata = subset(vocdat,ID==id), type = "response")
	
	looauthum2cansylgam = gam(hum2numcansyl~numsalons+numlmksyl+numdjwsyl+DurS,dat=subset(vocdat,(ID!=id) & !is.na(hum2numcansyl)))
	vocdat$looauthum2cansylestgam[vocdat$ID==id] = predict(looauthum2cansylgam,newdata = subset(vocdat,ID==id), type = "response")
	
	looauthum2anysylgam = gam(hum2numanysyl~numsalons+numlmksyl+numdjwsyl+DurS,dat=subset(vocdat,(ID!=id) & !is.na(hum2numanysyl)))
	vocdat$looauthum2anysylestgam[vocdat$ID==id] = predict(looauthum2anysylgam,newdata = subset(vocdat,ID==id), type = "response")
	
	looauthum2cananysylgam = gam(hum2numcansyl/hum2numanysyl~numsalons+numlmksyl+numdjwsyl+DurS,dat=subset(vocdat,(ID!=id) & !is.na(hum2numanysyl)))
	vocdat$looauthum2cananysylestgam[vocdat$ID==id] = predict(looauthum2cananysylgam,newdata = subset(vocdat,ID==id), type = "response")
	
	loodayautsyllm = lm(AveHumNumSyl~scale(AveNumSalOns)+scale(AveNumLmkSyl)+scale(AveNumDjwSyl)+scale(AveDurS),dat=subset(daydat,ID!=id))
	daydat$looautsylest[daydat$ID==id] = predict(loodayautsyllm,newdata = subset(daydat,ID==id), type = "response")
	
	loodayautsylperslm = lm(AveHumNumSyl/AveDurS~scale(AveNumSalOns/AveDurS)+scale(AveNumLmkSyl/AveDurS)+scale(AveNumDjwSyl/AveDurS),dat=subset(daydat,ID!=id))
	daydat$looautsylpersest[daydat$ID==id] = predict(loodayautsylperslm,newdata = subset(daydat,ID==id), type = "response")
	
	tempPreProcFunction = preProcess(subset(vocdat,(ID!=id) & !is.na(humnumsyl))[,c('DurMs','numsalons','numlmksyl','numdjwsyl')],method=c('BoxCox','center','scale','pca')) # https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/#ref2
	trainPreProcValues = predict(tempPreProcFunction,subset(vocdat,(ID!=id) & !is.na(humnumsyl))[,c('DurMs','numsalons','numlmksyl','numdjwsyl')])
	testPreProcValues = predict(tempPreProcFunction,subset(vocdat,(ID==id))[,c('DurMs','numsalons','numlmksyl','numdjwsyl')])
	traindat = trainPreProcValues
	traindat$humnumsyl = subset(vocdat,(ID!=id) & !is.na(humnumsyl))$humnumsyl
	loosylestpcagam = gam(humnumsyl ~ PC1 + PC2 + PC3 + PC4, dat = traindat)
	vocdat$loosylestpcagam[vocdat$ID==id] = predict(loosylestpcagam,newdata = testPreProcValues,type="response")
	loosylestpcaglm = glm(humnumsyl ~ PC1 + PC2 + PC3 + PC4, dat = traindat, family=poisson)
	vocdat$loosylestpcaglm[vocdat$ID==id] = predict(loosylestpcaglm,newdata = testPreProcValues,type="response")
	loosylestpcasvr = svm(humnumsyl ~ PC1 + PC2 + PC3 + PC4,traindat) # http://www.svm-tutorial.com/2014/10/support-vector-regression-r/
	vocdat$loosylestpcasvr[vocdat$ID==id] = predict(loosylestpcasvr,newdata = testPreProcValues,type="response")
	
	tempPreProcFunction = preProcess(subset(vocdat,(ID!=id) & !is.na(humnumcansyl_afontana5_1))[,c('DurMs','numsalons','numlmksyl','numdjwsyl')],method=c('BoxCox','center','scale','pca')) # https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/#ref2
	trainPreProcValues = predict(tempPreProcFunction,subset(vocdat,(ID!=id) & !is.na(humnumcansyl_afontana5_1))[,c('DurMs','numsalons','numlmksyl','numdjwsyl')])
	testPreProcValues = predict(tempPreProcFunction,subset(vocdat,(ID==id))[,c('DurMs','numsalons','numlmksyl','numdjwsyl')])
	traindat = trainPreProcValues
	traindat$humnumcansyl_afontana5_1 = subset(vocdat,(ID!=id) & !is.na(humnumcansyl_afontana5_1))$humnumcansyl_afontana5_1
	loosylestpcagam_afontana5_1 = gam(humnumcansyl_afontana5_1 ~ PC1 + PC2 + PC3 + PC4, dat = traindat)
	vocdat$loosylestpcagam_afontana5_1[vocdat$ID==id] = predict(loosylestpcagam_afontana5_1,newdata = testPreProcValues,type="response")
	
	# TODO: Modify the following code block to make use of vocdat$humnumcansyl_afontana5_1_categorical
	# For every # of syllables except the # of syllables with the most frequent exemplars, resample until that # of syllables has the same number of exemplars as the # of exemplars in the max # of syllables category.
	# Categorize into 0, 1, 2, and 3 or more
	eqtraindat = subset(vocdat,(ID!=id) & !is.na(humnumcansyl_afontana5_1))
	eqtraindat$humnumcansyl_afontana5_1[eqtraindat$humnumcansyl_afontana5_1>2] = 3
	maxexemplars = max(length(eqtraindat$humnumcansyl_afontana5_1[eqtraindat$humnumcansyl_afontana5_1==0]),length(eqtraindat$humnumcansyl_afontana5_1[eqtraindat$humnumcansyl_afontana5_1==1]),length(eqtraindat$humnumcansyl_afontana5_1[eqtraindat$humnumcansyl_afontana5_1==2]),length(eqtraindat$humnumcansyl_afontana5_1[eqtraindat$humnumcansyl_afontana5_1==3]))
	# Resample so all categories have the same number of exemplars
	for (sylnum in c(0,1,2,3)){
		exemplarnumdif = maxexemplars - length(eqtraindat$humnumcansyl_afontana5_1[eqtraindat$humnumcansyl_afontana5_1==sylnum])
		sylnumind = which(eqtraindat$humnumcansyl_afontana5_1 %in% sylnum)
		newexemplars = sample(sylnumind,size = exemplarnumdif, replace = T)
		eqtraindat = rbind(eqtraindat,eqtraindat[newexemplars,])
	}
	tempPreProcFunction = preProcess(eqtraindat[,c('DurMs','numsalons','numlmksyl','numdjwsyl')],method=c('BoxCox','center','scale','pca'))
	trainPreProcValues = predict(tempPreProcFunction,eqtraindat[,c('DurMs','numsalons','numlmksyl','numdjwsyl')])
	testPreProcValues = predict(tempPreProcFunction,subset(vocdat,(ID==id))[,c('DurMs','numsalons','numlmksyl','numdjwsyl')])
	traindat = trainPreProcValues
	traindat$humnumcansyl_afontana5_1 = eqtraindat$humnumcansyl_afontana5_1
	loosylestpcagameq = gam(humnumcansyl_afontana5_1 ~ PC1 + PC2 + PC3, dat = traindat)
	vocdat$loosylestpcagameq[vocdat$ID==id] = predict(loosylestpcagameq,newdata = testPreProcValues,type="response")
	
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
