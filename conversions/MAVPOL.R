library("PraatR")

FullPath = function(FileName){
#DataDirectory = "C:/Users/MyUsername/Desktop/Tutorial/"
#DataDirectory = "E:/praatR/Tutorial/"
DataDirectory = "/home/degen/Desktop/PRScripts/"
#TODO find where I set it in the praat tutorial on SYSdev
return( paste(DataDirectory,FileName,sep="") )
} # End function


#get variables as arguments: filename, datafile, female, plain
#TODO: These are just default values
#filename <- "t111.u220TOt116.u220"
filename <- "t143.u189.TO.t148.u189"
datafile <- "female"
sex <- "female"
guise <- "plain"
#set acoustic parameters
topFormant <- 0

#NOTE: else must be on same line as closing bracket } of if{}
if (sex == "female"){
topFormant <- 5500
} else if (sex == "male"){
topFormant <- 5000
}

# Put the headers on the formant file
#TODO: OH WHAT I CAN DO IS JUST MAKE THE HEADER A VECTOR AND THEN PASTE IT WITH sep=\t
#AND THEN I CAN GET NUMBER OF COLUMNS WITH length()
if (guise == "spanish"){
#TODO: does the header of the excel file read \t from R as tab$ from praat? newline$ -> \n?
#header$ = "guise" + tab$ + "soundlabel" + tab$ + "start" + tab$ + "end" +  tab$ + "precedingSegment"+  tab$ + "followingSegment"+  tab$ + "f1" + tab$ + "f2" + newline$
#header <- "guise\tsoundlabel\tstart\tend\tprecedingSegment\tfollowingSegment\tf1\tf2\n"
#TODO: \n needed at end for newline??
header<- c("guise", "soundlabel", "start", "end", "precedingSegment", "followingSegment", "f1", "f2")
} else if (guise == "spanish_plain"){
#TODO: is this just the same as guise==spanish??
#header$ = "guise" + tab$ + "soundlabel" + tab$ + "start" + tab$ + "end" +  tab$ + "precedingSegment"+  tab$ + "followingSegment"+  tab$ + "f1" + tab$ + "f2" + newline$
#header <- "guise\tsoundlabel\tstart\tend\tprecedingSegment\tfollowingSegment\tf1\tf2\n"
header<- c("guise", "soundlabel", "start", "end", "precedingSegment", "followingSegment", "f1", "f2")
} else {
#header$ = "guise" + tab$ + "soundlabel" + tab$ + "start" + tab$ + "end" +  tab$ + "precedingSegment"+  tab$ + "followingSegment"+  tab$ + "f1" + tab$ + "f2" + tab$ + "word"+ newline$
#header <- "guise\tsoundlabel\tstart\tend\tprecedingSegment\tfollowingSegment\tf1\tf2\tword\n"
header <- c("guise", "soundlabel", "start", "end", "precedingSegment", "followingSegment", "f1", "f2", "word")
}
columns <- length(header)
#TODO: error if zero length header?

#fileappend writes header to datafile
#write.table(header, file = FullPath(datafile), quote = TRUE, append = FALSE, sep="\t", col.names = TRUE)
#NOTE! append=FALSE OVERWRITES what was there originally, BE CAREFUL
#write(header, file=FullPath(datafile), append = FALSE, sep="\t", ncol=columns)

#need to transpose and convert to matrix
header_mt <- as.matrix(t(columns))

filename_fortable <- FullPath(datafile)
write.table(header, file=filename_fortable, append=FALSE, quote = TRUE, sep="\t", row.names=FALSE, col.names=FALSE)


# Get the files
#TODO: need to read in the textgrids and rename them

toread <- paste(filename, ".TextGrid", sep="")
#TODO: renames toread to 'item' in object window -- what do we do with this?

toread <- paste(filename, ".wav", sep="")
#TODO: renames toread to 'item' in object window -- what do we do with this??

#### Praat doesn't like periods in names, so they will appear as underscores in object names
name <- gsub("[.]", "_", filename)
soundname <- "Sound item"
#textgridname <- "TextGrid item"
textgridname <- paste(filename, ".TextGrid", sep="")
#formantname <- "Formant item"
formantname <- "tmp.formant"

# Create the formant objects
#this is gonna be so killer when it works
#HANG ON I THINK INPUT IS THE FILE IT'S WORKING ON AND ARGUMENTS ARE WHAT IT DOES TO IT
#YEAH THAT'S IT WOO
#TODO: what's this business about renaming the stuff to "item", cause right now
#TODO: (cont) I'm just getting the original filename in here
#overwrite = TRUE
praat( "To Formant (burg)...", input=FullPath(toread), output=FullPath("tmp.formant"), arguments=list(0,5,topFormant,0.025,50), overwrite=TRUE)


# Extract the data
#input will be textgridname
#arguments can't be "", " ", or contain "__". so be careful with soundlabel and wordlabel
numberofsegments <- praat("Get number of intervals...", input=FullPath(textgridname), arguments=list(1))
i <- 1
soundlabel <- ""
wordlabel <- ""
for(i in 1:numberofsegments){
	#command is "Get label of interval...", input is textgridname, arguments is list(1, i)
	soundlabel <- praat("Get label of interval...", input=FullPath(textgridname), arguments=list(1, i))
	if ( nchar(soundlabel) < 2){
		#"Get start point..." on textgridname, args=1, i
		st <- praat("Get start point...", input=FullPath(textgridname), arguments=list(1,i))
		#"Get end point..." same deal
		et <- praat("Get end point...", input=FullPath(textgridname), arguments=list(1,i))
        
        #parse et and st for the actual value of the seconds
        etparse <- as.numeric(gsub("[^[:digit:].]", "\\1", et))
        #print(etparse)
        stparse <- as.numeric(gsub("[^[:digit:].]", "\\1", st))
        #print(stparse)
        mid <- stparse + (etparse - stparse)/ 2
        #print(mid)
        
        
		#"Get label of interval...", textgrid name, args=(2, mid)
        #mid needs to be whole number?
        round(mid)
        cat("mid: ", mid, "\n")
        #TODO: Praat doesn't like that the first time through tries to use a values less than 1, maybe put this line down by if i < numberofsegments?
		j <- praat("Get label of interval...", input=FullPath(textgridname), arguments=list(2,mid))
        cat("J: ", j, "\n")
        #TODO: I think this one doesn't like wordlabel being a string? come back to this one
        #wordlabel <- praat("Get label of interval...", input=FullPath(textgridname), arguments=list(2,j))
		
		precedingsegment <- "NA"
		followingsegment <- "NA"
		
        cat("i: ", i, "\n")
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
		
        #no good way to comment out code in R so going to use if(FALSE)
        

        
		#selected object is now formant name...might be worth just making that a variable?
        f1 <- praat("Get value at time...", input=FullPath(formantname), arguments=list(1, mid, "Hertz", "Linear"))
        f2 <- praat("Get value at time...", input=FullPath(formantname), arguments=list(2, mid, "Hertz", "Linear"))
        
        cat("f1: ", f1, "\n")
        cat("f2: ", f2, "\n")
        

		
		# Fix any undefined formants by checking right next to it, or assigning NA
		#TODO: what will undefined in praat return to R?
        ##It's currently printing out as --undefined--, so try the string of that value

		if (f1 == "--undefined--"){
			f1 <- praat("Get value at time...", input=FullPath(formantname), arguments=list(1, mid+10, "Hertz", "Linear"))
			if (f1=="--undefined--"){
				f1 <- "NA"
            } else{ #TODO: R doesn't like trying to round the f1 string with "hertz" at the end
				#truncate f1 to hundreths precision
                #f1 <- round(f1, digits=2)
			}
		} else{
            #f1 <- round(f1, digits=2)
		}
		
        
        
		if (f2 == "--undefined--"){
			f1 <- praat("Get value at time...", input=FullPath(formantname), arguments=list(2, mid+10, "Hertz", "Linear"))
			if (f2=="--undefined--"){
				f2 <- "NA"
			} else{ #TODO: R doesn't like trying to round the f1 string with "hertz" at the end
                #f2 <- round(f2, digits=2)
			}
		} else{
            #f2 <- round(f2, digits=2)
		}
        
        
        f1parse <- as.numeric(gsub("[^[:digit:].]", "\\1", f1))
        f2parse <- as.numeric(gsub("[^[:digit:].]", "\\1", f2))
        cat("f1parse: ", f1parse, "\n")
        cat("f2parse: ", f2parse, "\n")
        #f1 and f2 are returned with the Hertz tag, need to strip that from them
		
        #if (FALSE){
            #begin if false
        
		#TODO praat syntax
		stparse <- round(stparse, digits=10)
		etparse <- round(etparse, digits=10)
		
		if (guise == "spanish"){
			#guise soundlabel st et precedingsegment followingsegment f1 f2
            line <- c(guise, soundlabel, stparse, etparse, precedingsegment, followingsegment, f1parse, f2parse)
		} else if (guise == "spanish_plain"){
            #guise soundlabel st et precedingsegment followingsegment f1 f2
            line <- c(guise, soundlabel, stparse, etparse, precedingsegment, followingsegment, f1parse, f2parse)
		} else {
            #guise soundlabel st et precedingsegment followingsegment f1 f2 wordlabel
            line <- c(guise, soundlabel, stparse, etparse, precedingsegment, followingsegment, f1parse, f2parse, wordlabel)
		}
        
        #end if false
        #}
        
		#TODO: appendFileLine(datafile, line)
        #write(line, file=FullPath(datafile), append = TRUE, sep="\t", ncol=columns)
        filename_fortable <- FullPath(datafile)
        write.table(line, file=filename_fortable, append=TRUE, quote = TRUE, sep="\t", eol="\n", row.names=FALSE, col.names=FALSE)
        #change write.table to something that writes as just a line
        #or just instead of printing to file, create a dataframe with these columns, and then each line is a row in the dataframe
        #then write.table the whole dataframe and it should come out
		
	}
}

#I don't think we need to remove the two objects, since this isn't a Praat workspace?
#expand so it has pitch and duration, THEN rearrange php and R code that makes the graphs so that it'll just ask for the JSON from the database
