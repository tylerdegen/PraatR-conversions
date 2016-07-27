form Specify the object name.
        sentence soundgridfile
        sentence outputfile
endform

# Put the headers on the formant file

soundfile$ = soundgridfile$ + ".wav"
gridfile$ = soundgridfile$ + ".TextGrid"

header$ = "soundlabel" + tab$ + "start" + tab$ + "end" +  tab$ + "precedingSegment"+  tab$ + "followingSegment"+  tab$ + "meanPitch" + tab$ + "minPitch" + tab$ + "maxPitch"

writeFileLine(outputfile$, header$) 

# Get the files

do ("Read from file...", soundfile$)
do("Rename...", "sound")

do ("Read from file...", gridfile$)
do("Rename...", "grid")

selectObject("Sound sound")
do("To Pitch (ac)...", 0, 75, 15, 1, 0.03, 0.45, 0.01, 0.35, 0.14, 600)



# Extract the data
selectObject("TextGrid grid")
numberofsegments = do("Get number of intervals...",  1)
i = 0
soundlabel$ = ""
wordlabel$ = ""
repeat
	i = i + 1
	selectObject("TextGrid grid")
	soundlabel$ = do$("Get label of interval...", 1, i)

	if length(	soundlabel$) > 2

		st= do("Get start point...", 1, i)
		et= do("Get end point...", 1, i)
		mid = (st + et)/2

		j = do("Get interval at time...", 2, mid)
		wordlabel$ = do$("Get label of interval...", 2, j)

	     	precedingsegment$ = "NA"
	     	followingsegment$ = "NA"
		if (i > 1)
			precedingsegment$ = do$("Get label of interval...", 1, i-1)
		endif
		if (i < numberofsegments)
			followingsegment$ = do$("Get label of interval...", 1, i+1)
		endif

		if precedingsegment$ = ""
		     precedingsegment$ = "NA"
		endif     

		if followingsegment$ = ""
		     followingsegment$ = "NA"
		endif     

		selectObject("Pitch sound")
		meanPitch$ = Get mean... st et Hertz
		meanPitch$ = meanPitch$ - " Hz"

		minPitch$ = Get minimum... st et Hertz Parabolic
		minPitch$ = minPitch$ - " Hz"

		maxPitch$ = Get maximum... st et Hertz Parabolic
		maxPitch$ = maxPitch$ - " Hz"

		if meanPitch$ == "--undefined--"
		     meanPitch$ = "NA"
		endif
		if minPitch$ == "--undefined--"
		     minPitch$ = "NA"
		endif
		if maxPitch$ == "--undefined--"
		     maxPitch$ = "NA"
		endif

		st$ = fixed$ (st, 10)
		et$ = fixed$ (et, 10)

		line$ = soundlabel$ + tab$ + st$ + tab$ + et$ + tab$ + precedingsegment$ + tab$ + followingsegment$ + tab$ + meanPitch$ + tab$ + minPitch$ + tab$ + maxPitch$
		appendFileLine(outputfile$, line$) 

	endif
until (i = numberofsegments)  

selectObject("Sound sound")
 plusObject("TextGrid grid")
 plusObject("Pitch sound")
do("Remove")

