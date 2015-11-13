import csv, glob, os, subprocess

outf = open('/Users/awarlau/RamsdellWarlaumontCollab/uttsph.csv','w')
csvfile = open('/Users/awarlau/RamsdellWarlaumontCollab/session_label_list.csv')

outf.write('WavFilename,sphout,concnt,vowcnt\n')
indat = csv.reader(csvfile,delimiter = ',')
count = 0
for row in indat:
    if count==0:
        WavFilenameCol = row.index('WavFilename')
        IDCol = row.index('ID')
    else:
        wavfilename = '/Users/awarlau/RamsdellWarlaumontCollab/' + row[IDCol] + '_clips_ch1_16000/ch1_16000_' + row[WavFilenameCol]
        print(wavfilename)
        # Run the Sphinx command and save its hypothesized phones to a variable called phones
        phones = subprocess.check_output(['pocketsphinx_continuous', '-infile', wavfilename, '-hmm', '/Users/awarlau/RamsdellWarlaumontCollab/SyllableDetectionCode/pocketsphinx-master/model/en-us/en-us', '-allphone', '/Users/awarlau/RamsdellWarlaumontCollab/SyllableDetectionCode/pocketsphinx-master/model/en-us/en-us-phone.lm.bin', '-backtrace', 'yes', '-beam', '1e-20', '-pbeam', '1e-20', '-lw', '2.0'], stderr=open(os.devnull, 'w'))
        sphout = phones.replace('\n',' ')
        phonelist = sphout.split(" ")
        concnt = sum(1 for p in phonelist if p=="B" or p=="CH" or p=="D" or p=="DH" or p=="F" or p=="G" or p=="JH" or p=="K" or p=="L" or p=="M" or p=="N" or p=="NG" or p=="P" or p=="R" or p=="S" or p=="SH" or p=="T" or p=="TH" or p=="V" or p=="W" or p=="Y" or p=="Z" or p=="ZH") # Excluding "HH"
        vowcnt = sum(1 for p in phonelist if p=="AA" or p=="AE" or p=="AH" or p=="AO" or p=="AW" or p=="AY" or p=="EH" or p=="ER" or p=="EY" or p=="IH" or p=="IY" or p=="OW" or p=="OY" or p=="UH" or p=="UW")
        outf.write(row[WavFilenameCol] + ',' + sphout + ',' + str(concnt) + ',' + str(vowcnt) + '\n')
    count= count+1
    
outf.close()

# Write a table where the first column is the wav filename, the second column is the Sphinx output, the third column is the consonant count, the fourth column is the vowel count, and the fifth column is the non-speech sound count
