form Specify the object name.
        sentence filename temp
	sentence selectedword CAUGHT
endform

# Get the files

toread$ = filename$ + ".TextGrid"
do ("Read from file...", toread$)
do("Rename...", "biggrid")

toread$ = filename$ + ".wav"
do ("Read from file...", toread$)
numChan = do("Get number of channels")
if numChan > 1
   do("Extract one channel...", 1)
endif
do("Resample...", 22050, 50)
do("Rename...", "bigwav")

soundname$ = "Sound bigwav"
textgridname$ = "TextGrid biggrid"
tablename$ = "Table biggrid"
partsoundname$ = "Sound bigwav_part"
parttextgridname$ = "TextGrid biggrid_part"

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
	endif
until (i = numberofwords)  or (wordlabel$ = selectedword$)

# Now we flip it

selectObject(partsoundname$)
selectObject(parttextgridname$)

numberofphones = do("Get number of intervals...",  1)

i = numberofphones + 1
phonelabel$ = ""
repeat
	i = i - 1
	selectObject(parttextgridname$)
	phonelabel$ = do$("Get label of interval...", 1, i)
	st= do("Get start point...", 1, i)
	et= do("Get end point...", 1, i)
	do("Extract part...", st, et, 1)
	do("Rename...",  string$(i))
	selectObject(soundname$)
	do("Extract part...", st, et, "rectangular", 1.0, 1)
	do("Rename...", string$(i))

	do("Create Sound from formula...", "silence" + string$(i), 1, 0, 0.5, 22050, "0")
until (i = 1)

selectObject("Sound " + string$(numberofphones))
i = numberofphones
repeat
	i = i - 1
	plusObject("Sound " + string$(i))
until (i = 1)
do("Concatenate")
do("Rename...", "newsound")

selectObject("Sound " + string$(numberofphones))
i = numberofphones
repeat
	plusObject("Sound silence" + string$(i))
	i = i - 1
	plusObject("Sound " + string$(i))
until (i = 1)
do("Concatenate")
do("Rename...", "newsoundseparated")


selectObject("TextGrid " + string$(numberofphones))
i = numberofphones
repeat
	i = i - 1
	plusObject("TextGrid " + string$(i))
until (i = 1)
do("Concatenate")
do("Rename...", "newgrid")
do("Remove tier...", 2)

#  Save wav file of the word
selectObject("Sound newsound")
do("Scale intensity...", 80)
do("Save as WAV file...", filename$ + ".w" + wordlabel$ + ".backwards.wav")

selectObject("Sound newsoundseparated")
do("Scale intensity...", 80)
do("Save as WAV file...", filename$ + ".w" + wordlabel$ + ".backwards.separated.wav")

# Make the picture
# Adapted from Pauline Welby's script draw-waveform-sgram-f0.praat

Times
	Font size... 15
	Black
# Define size and position of waveform (by specifying grid coordinates)
do("Erase all")
	do("Select inner viewport...", 0, 8, 0, 2)

	# Draw waveform
selectObject("Sound newsound")
	do("Draw...", 0, 0, 0, 0, "no", "curve")

	# Define size and position of spectrogram
	do("Select inner viewport...", 0, 8, 2, 4.5)
selectObject("Sound newsound")
do("To Spectrogram...", 0.005, 10000, 0.002, 20, "Gaussian")
	do("Paint...", 0, 0, 0, 0, 100, "yes", 50, 6, 0, "no")
# Define size and position of TextGrid
	do("Select inner viewport...", 0, 8, 2, 6)


# Draw TextGrid
selectObject("TextGrid newgrid")
do("Draw...", 0, 0, "yes", "yes", "no")
	do("Select inner viewport...", 0, 8, 0, 6)
do("Save as EPS file...", filename$ + ".w" + wordlabel$ + ".backwards.eps")



selectObject(partsoundname$)
plusObject(soundname$)
plusObject("Sound newsound")
plusObject("Sound newsoundseparated")
plusObject("Spectrogram newsound")
plusObject("TextGrid newgrid")
plusObject(textgridname$)
plusObject(parttextgridname$)
i = numberofphones
repeat
	plusObject("TextGrid " + string$(i))
	plusObject("Sound " + string$(i))
	plusObject("Sound silence" + string$(i))
	i = i - 1
until (i = 0)

do("Remove")
