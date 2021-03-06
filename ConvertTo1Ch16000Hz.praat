###########################################################################
# Anne S. Warlaumont
###########################################################################

form Open all wav files in a directory
   comment Path to the directory containing the input .wav files (no final slash)
   text indirectory
   comment Path to the directory for the output .wav files (no final slash)
   text outdirectory
endform

Create Strings as file list... list 'indirectory$'/*.wav
numberOfFiles = Get number of strings

for file to numberOfFiles
   select Strings list
   fileName$ = Get string... file
   Read from file... 'indirectory$'/'fileName$'
   soundName$ = selected$("Sound")
   Extract one channel: 1
   Resample: 16000, 50
   Save as WAV file... 'outdirectory$'/ch1_16000_'fileName$'
   select Sound 'soundName$'
   plus Sound 'soundName$'_ch1
   plus Sound 'soundName$'_ch1_16000
   Remove
endfor
