form Specify the object name.
        sentence directory temp
        sentence filename temp.828697
	sentence selectedword CAUGHT
endform

# Get the files

toread$ = directory$ + filename$ + ".TextGrid"
do ("Read from file...", toread$)

toread$ = directory$ + filename$ + ".wav"
do ("Read from file...", toread$)

####  Praat doesn't like periods in names, so they will appear as _s in the actual object names
name$ = replace$(filename$, ".", "_", 0)

soundname$ = "Sound " + name$
partsoundname$ = "Sound " + name$ + "_part"

textgridname$ = "TextGrid " + name$
parttextgridname$ = "TextGrid " + name$ + "_part"

# Extract the parts we want.
# First locate the word.
selectObject(textgridname$)
numberofwords = do("Get number of intervals...",  2)
i = 1
wordlabel$ = ""
repeat
	i = i + 1
	selectObject(textgridname$)
	wordlabel$ = do$("Get label of interval...", 2, i)
	if wordlabel$ = selectedword$
		st= do("Get start point...", 2, i)
		et= do("Get end point...", 2, i)
		do("Extract part...", st, et, 1)
		selectObject(soundname$)
		do("Extract part...", st, et, "rectangular", 1.0, 1)

		#  Save wav file of the word
		selectObject(partsoundname$)
		do("Scale intensity...", 80)
		do("Reverse")
		do("Save as WAV file...", directory$ + filename$ + ".w" + wordlabel$ + ".reverse.wav")

		# Make the picture
		# Adapted from Pauline Welby's script draw-waveform-sgram-f0.praat

		Times
     		Font size... 15
     		Black
		# Define size and position of waveform (by specifying grid coordinates)
		do("Erase all")
     		do("Select inner viewport...", 0, 8, 0, 3)

     		# Draw waveform
		selectObject(partsoundname$)
     		do("Draw...", 0, 0, 0, 0, "no", "curve")
 
     		# Define size and position of spectrogram
     		do("Select inner viewport...", 0, 8, 3, 5)
		selectObject(partsoundname$)
		partspecname$ = "Spectrogram " + name$ + "_part"
 		do("To Spectrogram...", 0.005, 10000, 0.002, 20, "Gaussian")
    		do("Paint...", 0, 0, 0, 0, 100, "yes", 50, 6, 0, "no")
  		# Define size and position of TextGrid
#     		do("Select inner viewport...", 0, 8, 3, 6)

   		# Draw TextGrid
#		selectObject(parttextgridname$)
#   		do("Draw...", 0, 0, "yes", "yes", "no")
     		do("Select inner viewport...", 0, 8, 0, 6)
		do("Save as EPS file...", directory$ + filename$ + ".w" + wordlabel$ + ".reverse.eps")

	endif
until (i = numberofwords)  or (wordlabel$ = selectedword$)

selectObject(partsoundname$)
plusObject(soundname$)
plusObject(textgridname$)
plusObject(parttextgridname$)
plusObject(partspecname$)
do("Remove")
