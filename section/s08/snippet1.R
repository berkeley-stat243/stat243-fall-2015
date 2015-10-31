library (stringr)

####################################
#To obtain the number of applauses
countApplause <- function(context)
{
  speechEndLine <- gsub("<p>", "\n",context) # Insert endline
  speechLinks <- str_replace_all(speechEndLine, "<.*?>","") # Remove links
  return(str_count(speechLinks, "\\[Applause\\]"))
}

####################################
#To obtain the number of laughters
countLaughter <- function(context)
{
  speechEndLine <- gsub("<p>", "\n",context) # Insert endline
  speechLinks <- str_replace_all(speechEndLine, "<.*?>","") # Remove links
  return(str_count(speechLinks, "\\[Laughter\\]"))
}

####################################
#To obtain a nicely formatted context
cleanTxt <- function(context)
{
  speechEndLine <- gsub("<p>", "\n",context) # Insert endline
  speechLinks <- str_replace_all(speechEndLine, "<.*?>","") # Remove links
  speechApplause <- gsub("\\[Applause\\]","",speechLinks) # Remove applause
  speechFinal <- gsub("\\[Laughter\\]","",speechApplause)

  return (speechFinal)
}

library (stringr)

for (i in 1:length(speechTitle))
  {
   #Obtain cleaned speenches
   speechClean[[i]] <- cleanTxt(speechTxt[[i]])

   #obtain president's name and year of the speech
   name[i] <- gsub (":.*","",speechTitle[[i]])
   year[i] <- gsub(" ","", gsub(".*,","",speechTitle[[i]]))

   #Obtain number of applauses and laughters
   applause[i] <- countApplause(speechTxt[[i]])
   laughter[i] <- countLaughter(speechTxt[[i]])
  }
