function [] = getAllLandmarkCounts(uttdatfile,outfile)
%GETALLLANDMARKCOUNTS Get the number of landmarks of various types, using
%the SMMLToolBox.
%
% uttdatfile should be a filename for a table containing utterance-level
% information.
%
% outfile should be the filename where you want to save the salience data
% Example:
% getAllLandmarkCounts('~/RamsdellWarlaumontCollab/session_label_list.csv','~/RamsdellWarlaumontCollab/uttlmk.csv')

% Read in the utterance-level data
uttdat = readtable(uttdatfile);

% Open the output file for writing
outfid = fopen(outfile,'a');
printstr = char('WavFilename,nsyl');
fprintf(outfid,'%s\n',printstr);

% For each wav file, calculate the perceptual onsets.
for utt = 1:length(uttdat.WavFilename)
    uttdir = regexprep(uttdat.WavFilename(utt),'_.*','_clips/');
    [sig,fs] = audioread(char(strcat('~/RamsdellWarlaumontCollab/', uttdir, uttdat.WavFilename(utt))));
    try
        [lms,tvals,pcont,env,envthr,voicg,vthr] = landmarks(sig(:,1),fs,[],false,'child');
        nsyl = lm_syl_count(lms);
        printstr = char(strcat(uttdat.WavFilename(utt),',',num2str(nsyl)));
    catch
        printstr = char(strcat(uttdat.WavFilename(utt),',NA'));
    end
    fprintf(outfid,'%s\n',printstr);
end

fclose(outfid)

end

