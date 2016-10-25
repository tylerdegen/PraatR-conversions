library("PraatR")
library(jsonlite)

#Sets fullpath of the file for R consistency
FullPath = function(FileName){
DataDirectory = "/home/degen.19/projects/PraatR-conversions/conversions/"
#TODO find where I set it in the praat tutorial on SYSdev
return( paste(DataDirectory,FileName,sep="") )
}


#get variables as arguments: filename, datafile, female, plain
#TODO: These are just default values
filename <- "t143.u189.TO.t148.u189"
datafile <- "female"
sex <- "female"
guise <- "plain"


#set acoustic parameters
topFormant <- 5500

#NOTE: else must be on same line as closing bracket } of if{}
if (sex == "female"){
    topFormant <- 5500
} else if (sex == "male"){
    topFormant <- 5000
}

# Put the headers on the formant file
if (guise == "spanish"){
    header<- c("guise", "soundlabel", "start", "end", "precedingSegment", "followingSegment", "f1", "f2", "meanPitch", "minPitch", "maxPitch")
} else if (guise == "spanish_plain"){
#TODO: is this just the same as guise==spanish??
    header<- c("guise", "soundlabel", "start", "end", "precedingSegment", "followingSegment", "f1", "f2", "meanPitch", "minPitch", "maxPitch")
} else {
    header <- c("guise", "soundlabel", "start", "end", "precedingSegment", "followingSegment", "f1", "f2", "meanPitch", "minPitch", "maxPitch", "word")
}
columns <- length(header)
#TODO: error if zero length header?

filename_fortable <- FullPath(datafile)
#TODO: Will we need something header esque for the JSON database?
JSONheader <- toJSON(header)
cat(JSONheader, file=FullPath(datafile), fill=TRUE, append=FALSE)

# Get the files

#TODO this is redundant, I think it's safe to replace ln71 toread with soundname BUT will wait til I can isolate the change
toread <- paste(filename, ".TextGrid", sep="")

toread <- paste(filename, ".wav", sep="")

#### Praat doesn't like periods in names, so they will appear as underscores in object names
name <- gsub("[.]", "_", filename)
#TODO: is the sound just the original file? or something more praat specific, ie "Create Sound as Tone"
soundname <- paste(filename, ".wav", sep="")
textgridname <- paste(filename, ".TextGrid", sep="")
formantname <- paste(filename, ".formant", sep="")
pitchname <- paste(filename, ".pitch", sep="")

# Create the formant objects
praat( "To Formant (burg)...", input=FullPath(toread), output=FullPath(formantname), arguments=list(0,5,topFormant,0.025,50), overwrite=TRUE)

#Create the pitch objects
praat( "To Pitch (ac)...", input=FullPath(toread), output=FullPath(pitchname), arguments=list(0, 75, 15, "yes", 0.03, 0.45, 0.01, 0.35, 0.14, 600), overwrite=TRUE)


# Extract the data
#arguments can't be "", " ", or contain "__". so be careful with soundlabel and wordlabel
numberofsegments <- praat("Get number of intervals...", input=FullPath(textgridname), arguments=list(1))
i <- 1
soundlabel <- ""
wordlabel <- ""
for(i in 1:numberofsegments){
	#command is "Get label of interval...", input is textgridname, arguments is list(1, i)
	soundlabel <- praat("Get label of interval...", input=FullPath(textgridname), arguments=list(1, i))
	if ( nchar(soundlabel) > 2){
		#"Get start point..." on textgridname, args=1, i
		st <- praat("Get start point...", input=FullPath(textgridname), arguments=list(1,i))
		#"Get end point..." same deal
		et <- praat("Get end point...", input=FullPath(textgridname), arguments=list(1,i))
        
        #parse et and st for the actual value of the seconds
        etparse <- as.numeric(gsub("[^[:digit:].]", "\\1", et))
        stparse <- as.numeric(gsub("[^[:digit:].]", "\\1", st))
        mid <- stparse + (etparse - stparse)/2
        
        
		#"Get label of interval...", textgrid name, args=(2, mid)
        #mid needs to be whole number?
        round(mid)
        #TODO: Praat doesn't like that the first time through tries to use a values less than 1, maybe put this line down by if i < numberofsegments?
		j <- praat("Get label of interval...", input=FullPath(textgridname), arguments=list(2,mid))
        cat("J: ", j, "\n")
        #TODO: I think this one doesn't like wordlabel being a string and therefore crashes? come back to this one
        wordlabel <- praat("Get label of interval...", input=FullPath(textgridname), arguments=list(2,j))
		
		precedingsegment <- "NA"
		followingsegment <- "NA"
		
        
		if (i > 1){
			precedingsegment <- praat("Get label of interval...", input=FullPath(textgridname), arguments=list(1,i-1))
		}
		if (i < numberofsegments){
			followingsegment <- praat("Get label of interval...", input=FullPath(textgridname), arguments=list(1,i+1))
		}
		
		if (precedingsegment == ""){
			precedingsegment <- "NA"
		}
		if (followingsegment == ""){
			followingsegment <- "NA"
		}
		
        
        #FORMANT EXTRACTION
        
		#selected object is now formant name...might be worth just making that a variable?
        f1 <- praat("Get value at time...", input=FullPath(formantname), arguments=list(1, mid, "Hertz", "Linear"))
        f2 <- praat("Get value at time...", input=FullPath(formantname), arguments=list(2, mid, "Hertz", "Linear"))
        
        # Fix any undefined formants by checking right next to it, or assigning NA
        ##undefined in praat is currently printing out as --undefined--, so compare against the string of that value

		if (f1 == "--undefined--"){
			f1 <- praat("Get value at time...", input=FullPath(formantname), arguments=list(1, mid+10, "Hertz", "Linear"))
			if (f1=="--undefined--"){
				f1 <- "NA"
            }
		}
		
        
        
		if (f2 == "--undefined--"){
			f1 <- praat("Get value at time...", input=FullPath(formantname), arguments=list(2, mid+10, "Hertz", "Linear"))
			if (f2=="--undefined--"){
				f2 <- "NA"
			}
		}
        
        #f1 and f2 are returned with Hertz tag as strings, need to strip them and make nums
        #truncate f1 to hundreths precision
        if (f1 != "--undefined--" || "NA"){
            f1 <- as.numeric(gsub("[^[:digit:].]", "\\1", f1))
            f1 <- round(f1, digits=2)
        }
        if (f2 != "--undefined--" || "NA"){
            f2 <- as.numeric(gsub("[^[:digit:].]", "\\1", f2))
            f2 <- round(f1, digits=2)
        }

        #no good way to comment out code in R, use if(FALSE) if needed
            
        #PITCH EXTRACTION
		meanPitch <- praat("Get mean...", input=FullPath(pitchname), arguments=list(stparse, etparse, "Hertz"))
		#IT WORKS, IT STILL IS STRING WITH Hz AT END BUT IT WORKS
		#print(meanPitch)
		minPitch <- praat("Get minimum...", input=FullPath(pitchname), arguments=list(stparse, etparse, "Hertz", "Parabolic"))
		maxPitch <- praat("Get maximum...", input=FullPath(pitchname), arguments=list(stparse, etparse, "Hertz", "Parabolic"))
		
		#TODO: getting actual number values for pitch, error checking for --undefined--
        
		stparse <- round(stparse, digits=10)
		etparse <- round(etparse, digits=10)
		
        #TODO: Change line to row or column
		if (guise == "spanish"){
            line <- c(guise, soundlabel, stparse, etparse, precedingsegment, followingsegment, f1, f2, meanPitch, minPitch, maxPitch)
		} else if (guise == "spanish_plain"){
            line <- c(guise, soundlabel, stparse, etparse, precedingsegment, followingsegment, f1, f2, meanPitch, minPitch, maxPitch)
		} else {
            line <- c(guise, soundlabel, stparse, etparse, precedingsegment, followingsegment, f1, f2, meanPitch, minPitch, maxPitch, wordlabel)
		}
        
        filename_fortable <- FullPath(datafile)
        JSONline <- toJSON(line)
        cat(JSONline, file=FullPath(datafile), fill=TRUE, append=TRUE)
		
	}
}

#TODO: expand so it has pitch and duration, THEN rearrange php and R code that makes the graphs so that it'll just ask for the JSON from the database
