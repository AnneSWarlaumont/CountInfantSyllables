function [] = getAllSalienceOnsets(uttdatfile,outfile)
%GETALLSALIENCEONSETS Get the number of salience onsets for the full set of
%vocalizations, using Coath & Denham's toolbox.
%
% uttdatfile should be a filename for a table containing utterance-level
% information.
%
% outfile should be the filename where you want to save the salience data
%
% Example:
% getAllSalienceOnsets('~/RamsdellWarlaumontCollab/session_label_list.csv','~/RamsdellWarlaumontCollab/uttsal.csv')

warning('off','all');

% Read in the utterance-level data
uttdat = readtable(uttdatfile);

% Open the output file for writing
outfid = fopen(outfile,'a');
printstr = char('WavFilename,nonsets');
fprintf(outfid,'%s\n',printstr);

% For each wav file, calculate the perceptual onsets.
for utt = 1:length(uttdat.WavFilename)
    uttdir = regexprep(uttdat.WavFilename(utt),'_.*','_clips/');
    salinfo = auditoryPerceptualOnsets(char(strcat('~/RamsdellWarlaumontCollab/', uttdir, uttdat.WavFilename(utt))),.3,1,0);
    printstr = char(strcat(uttdat.WavFilename(utt),',',num2str(size(salinfo.pOnsets,1))));
    fprintf(outfid,'%s\n',printstr);
end

fclose(outfid)

warning('on','all');

end