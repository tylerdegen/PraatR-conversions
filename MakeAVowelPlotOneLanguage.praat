form Specify the object name.
        sentence filename t111.u220TOt116.u220
        sentence datafile female
        sentence sex female
        sentence guise plain
endform

# Set acoustic parameters

topFormant = 0
if sex$ == "female"
	topFormant = 5500
elsif sex$ == "male"
	topFormant = 5000
endif

# Put the headers on the formant file

if guise$ == "spanish"
   	  header$ = "guise" + tab$ + "soundlabel" + tab$ + "start" + tab$ + "end" +  tab$ + "precedingSegment"+  tab$ + "followingSegment"+  tab$ + "f1" + tab$ + "f2" + newline$
elsif guise$ == "spanish_plain"
   	  header$ = "guise" + tab$ + "soundlabel" + tab$ + "start" + tab$ + "end" +  tab$ + "precedingSegment"+  tab$ + "followingSegment"+  tab$ + "f1" + tab$ + "f2" + newline$
else
	header$ = "guise" + tab$ + "soundlabel" + tab$ + "start" + tab$ + "end" +  tab$ + "precedingSegment"+  tab$ + "followingSegment"+  tab$ + "f1" + tab$ + "f2" + tab$ + "word"+ newline$
endif
#header$ = "soundlabel" + tab$ + "wordlabel" + tab$ + "start" + tab$ + "end" +  tab$ + "precedingSegment"+  tab$ + "followingSegment"+  tab$ + "f1" + tab$ + "f2" + newline$
#falseRow$ = "NA" + tab$ + "é" + tab$ + "0" + tab$ + "0" +  tab$ + "NA"+  tab$ + "NA"+  tab$ + "0" + tab$ + "0" + newline$

fileappend 'datafile$' 'header$'
#fileappend 'datafile$' 'falseRow$'


# Get the files

toread$ = filename$ + ".TextGrid"
do ("Read from file...", toread$)
do("Rename...", "item")

toread$ = filename$ + ".wav"
do ("Read from file...", toread$)
do("Rename...", "item")


####  Praat doesn't like periods in names, so they will appear as _s in the actual object names
name$ = replace$(filename$, ".", "_", 0)

soundname$ = "Sound item"
textgridname$ = "TextGrid item"
formantname$ = "Formant item"


# Create the formant objects
do("To Formant (burg)...", 0, 5, topFormant, 0.025, 50)


# Extract the data
selectObject(textgridname$)
numberofsegments = do("Get number of intervals...",  1)
i = 0
soundlabel$ = ""
wordlabel$ = ""
repeat
	i = i + 1
	selectObject(textgridname$)
	soundlabel$ = do$("Get label of interval...", 1, i)

	if length(	soundlabel$) > 2

		st= do("Get start point...", 1, i)
		et= do("Get end point...", 1, i)
		mid = st + (et - st)/2

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

		selectObject(formantname$)
		f1 =  Get value at time... 1 'mid' Hertz Linear
		f2 =  Get value at time... 2 'mid' Hertz Linear

		# Fix any undefined formants by checking right next to it, or assigning NA.

		if f1 = undefined
			f1 =  do("Get value at time...", 1, mid+10, "Hertz", "Linear")
			if f1 = undefined
				f1$ = "NA"
			else
				f1$ = fixed$ (f1, 2)
			endif
		else
			f1$ = fixed$ (f1, 2)
		endif

		if f2 = undefined
			f2 =  do("Get value at time...", 2, mid+10, "Hertz", "Linear")
			if f2 = undefined
				f2$ = "NA"
			else
				f2$ = fixed$ (f2, 2)
			endif
		else
			f2$ = fixed$ (f2, 2)
		endif

		st$ = fixed$ (st, 10)
		et$ = fixed$ (et, 10)


		if guise$ == "spanish"    
		   line$ = guise$ + tab$ + soundlabel$ + tab$ + st$ + tab$ + et$ + tab$ + precedingsegment$ + tab$ + followingsegment$ + tab$ + f1$ + tab$ + f2$
		elsif guise$ == "spanish_plain"
		   line$ = guise$ + tab$ + soundlabel$ + tab$ + st$ + tab$ + et$ + tab$ + precedingsegment$ + tab$ + followingsegment$ + tab$ + f1$ + tab$ + f2$
		else
		   line$ = guise$ + tab$ + soundlabel$ + tab$ + st$ + tab$ + et$ + tab$ + precedingsegment$ + tab$ + followingsegment$ + tab$ + f1$ + tab$ + f2$ + tab$ + wordlabel$
		endif
		appendFileLine(datafile$, line$) 

	endif
until (i = numberofsegments)  

plusObject(soundname$)
plusObject(textgridname$)
do("Remove")

