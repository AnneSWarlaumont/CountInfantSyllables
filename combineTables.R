# Load the files that contain the child info. for each sound, Cecilia's rating for each sound, and the salience onsets for each sound
vocdat = read.csv('~/RamsdellWarlaumontCollab/session_label_list.csv',header=T)

# Get each sound's duration
vocdat$DurMs = vocdat$EndMs - vocdat$StartMs

# Add the automated estimate of number of salience onsets
saldat = read.csv('~/RamsdellWarlaumontCollab/uttsal.csv',header=T)
vocdat$numsalons = saldat$nonsets
rm(saldat)

# Add the automated estimate of number of syllables based on Stevens' landmarks
lmkdat = read.csv('~/RamsdellWarlaumontCollab/uttlmk.csv',header=T)
vocdat$numlmksyl = lmkdat$nsyl

# Add the de Jong & Wempe estimate of number of syllable nuclei
djwFiles = list.files(path='~/RamsdellWarlaumontCollab/deJongWempeSyllables/',pattern="*.csv")
vocdat$numdjwsyl = rep(NA,nrow(vocdat))
for (djwFile in djwFiles){
	djwdat = read.csv(paste('~/RamsdellWarlaumontCollab/deJongWempeSyllables/',djwFile,sep=""))
	for (djwvoc in 1:nrow(djwdat)){
		vocrow = which(vocdat$WavFilename==paste(djwdat$soundname[djwvoc],'.wav',sep=''))
		vocdat$numdjwsyl[vocrow] = djwdat$nsyll[djwvoc]
	}
}

# # Add the first set of human ratings of number of syllables and rhythmicity
# humdat = read.csv('~/RamsdellWarlaumontCollab/CeciliaValdovinos_responses.csv',header=T) # Read in the file
# humdat = subset(humdat,select=-c(X)) # Clean up the data frame by removing the empty final column
# humdat$WavFilename = gsub(' ','',humdat$WavFilename) # Clean up the data frame by removing the space characters
# humdat$Rhythmicity = gsub(' ','',humdat$Rhythmicity) # Clean up the data frame by removing the space characters
# humdat$CanonicalSyllables = gsub(' ','',humdat$CanonicalSyllables) # Clean up the data frame by removing the space characters
# vocdat$humnumsyl = rep(NA,nrow(vocdat))
# vocdat$humnumsyl2 = rep(NA,nrow(vocdat))
# vocdat$humrhy = rep(NA,nrow(vocdat))
# vocdat$humrhy2 = rep(NA,nrow(vocdat))
# for (lis in 1:nrow(humdat)){ # For each row of the record of human listening judgments, i.e. for each listening trial
	# vocrow = which(vocdat$WavFilename==humdat$WavFilename[lis]) # Find which row of the vocdat corresponds to this listening trial's wav file
	# if (length(vocrow)>0){
		# if (is.na(vocdat$humrhy[vocrow])){
			# vocdat$humrhy[vocrow] = as.character(humdat$Rhythmicity[lis])
			# vocdat$humnumsyl[vocrow] = as.character(humdat$CanonicalSyllables[lis])
		# } else if (is.na(vocdat$humrhy2[vocrow])){
			# vocdat$humrhy2[vocrow] = as.character(humdat$Rhythmicity[lis])
			# vocdat$humnumsyl2[vocrow] = as.character(humdat$Rhythmicity[lis])
		# }
	# }
# }
# rm(humdat)

# # Add the second set of human ratings of number of syllables and number of canonical syllables
# hum2dat = read.csv('~/RamsdellWarlaumontCollab/SyllableDetectionCode/AnneOldListening/Anne1_responses.csv',header=T) # Read in the file
# vocdat$hum2numanysyl = rep(NA,nrow(vocdat))
# vocdat$hum2numcansyl = rep(NA,nrow(vocdat))
# for (lis in 1:nrow(hum2dat)){
	# vocrow = which(vocdat$WavFilename==as.character(hum2dat$WavFilename[lis]))
	# vocdat$hum2numanysyl[vocrow] = as.character(hum2dat$AllSyllables[lis])
	# vocdat$hum2numcansyl[vocrow] = as.character(hum2dat$CanonicalSyllables[lis])
# }
# rm(hum2dat)

# Add the awarlaumont2_1 human ratings of number of syllables and number of canonical syllables
# humdat_awarlaumont2_1 = read.csv('~/RamsdellWarlaumontCollab/SyllableDetectionCode/awarlaumont2_1_responses.csv',header=T,na.strings=c("","NA")) # Read in the file
humdat_awarlaumont2_1 = read.csv('~/RamsdellWarlaumontCollab/SyllableDetectionCode/awarlaumont2_1_responses.csv',header=T) # Read in the file
vocdat$humnumanysyl_awarlaumont2_1 = rep(NA,nrow(vocdat))
vocdat$humnumcansyl_awarlaumont2_1 = rep(NA,nrow(vocdat))
vocdat$humcrylauveg_awarlaumont2_1 = rep(NA,nrow(vocdat))
vocdat$humhumover_awarlaumont2_1 = rep(NA,nrow(vocdat))
vocdat$humnonhumover_awarlaumont2_1 = rep(NA,nrow(vocdat))
for (lis in 1:nrow(humdat_awarlaumont2_1)){
	vocrow = which(vocdat$WavFilename==as.character(humdat_awarlaumont2_1$WavFilename[lis]))
	vocdat$humnumanysyl_awarlaumont2_1[vocrow] = as.character(humdat_awarlaumont2_1$AllSyllables[lis])
	vocdat$humnumcansyl_awarlaumont2_1[vocrow] = as.character(humdat_awarlaumont2_1$CanonicalSyllables[lis])
	vocdat$humhumover_awarlaumont2_1[vocrow] = as.character(humdat_awarlaumont2_1$HumanOverlap[lis])
	vocdat$humnonhumover_awarlaumont2_1[vocrow] = as.character(humdat_awarlaumont2_1$NonhumanOverlap[lis])
	vocdat$humcrylauveg_awarlaumont2_1[vocrow] = as.character(humdat_awarlaumont2_1$CryLaughVeg[lis])
}
rm(humdat_awarlaumont2_1)

# Add the afontana5_1 human ratings of number of syllables and number of canonical syllables
humdat_afontana5_1 = read.csv('~/RamsdellWarlaumontCollab/SyllableDetectionCode/afontana5_1_responses.csv',header=T,na.strings=c("","NA")) # Read in the file
vocdat$humnumanysyl_afontana5_1 = rep(NA,nrow(vocdat))
vocdat$humnumcansyl_afontana5_1 = rep(NA,nrow(vocdat))
vocdat$humcrylauveg_afontana5_1 = rep(NA,nrow(vocdat))
vocdat$humhumover_afontana5_1 = rep(NA,nrow(vocdat))
vocdat$humnonhumover_afontana5_1 = rep(NA,nrow(vocdat))
for (lis in 1:nrow(humdat_afontana5_1)){
	vocrow = which(vocdat$WavFilename==as.character(humdat_afontana5_1$WavFilename[lis]))
	vocdat$humnumanysyl_afontana5_1[vocrow] = as.character(humdat_afontana5_1$AllSyllables[lis])
	vocdat$humnumcansyl_afontana5_1[vocrow] = as.character(humdat_afontana5_1$CanonicalSyllables[lis])
	vocdat$humhumover_afontana5_1[vocrow] = as.character(humdat_afontana5_1$HumanOverlap[lis])
	vocdat$humnonhumover_afontana5_1[vocrow] = as.character(humdat_afontana5_1$NonhumanOverlap[lis])
	vocdat$humcrylauveg_afontana5_1[vocrow] = as.character(humdat_afontana5_1$CryLaughVeg[lis])
}
rm(humdat_afontana5_1)

# Add the gmacedo_1 human ratings of number of syllables and number of canonical syllables
humdat_gmacedo_1 = read.csv('~/RamsdellWarlaumontCollab/SyllableDetectionCode/gmacedo_1_responses.csv',header=T,na.strings=c("","NA")) # Read in the file
vocdat$humnumanysyl_gmacedo_1 = rep(NA,nrow(vocdat))
vocdat$humnumcansyl_gmacedo_1 = rep(NA,nrow(vocdat))
vocdat$humcrylauveg_gmacedo_1 = rep(NA,nrow(vocdat))
vocdat$humhumover_gmacedo_1 = rep(NA,nrow(vocdat))
vocdat$humnonhumover_gmacedo_1 = rep(NA,nrow(vocdat))
for (lis in 1:nrow(humdat_gmacedo_1)){
	vocrow = which(vocdat$WavFilename==as.character(humdat_gmacedo_1$WavFilename[lis]))
	vocdat$humnumanysyl_gmacedo_1[vocrow] = as.character(humdat_gmacedo_1$AllSyllables[lis])
	vocdat$humnumcansyl_gmacedo_1[vocrow] = as.character(humdat_gmacedo_1$CanonicalSyllables[lis])
	vocdat$humhumover_gmacedo_1[vocrow] = as.character(humdat_gmacedo_1$HumanOverlap[lis])
	vocdat$humnonhumover_gmacedo_1[vocrow] = as.character(humdat_gmacedo_1$NonhumanOverlap[lis])
	vocdat$humcrylauveg_gmacedo_1[vocrow] = as.character(humdat_gmacedo_1$CryLaughVeg[lis])
}
rm(humdat_gmacedo_1)

write.csv(vocdat,file='~/RamsdellWarlaumontCollab/vocdat.csv',row.names=F)
