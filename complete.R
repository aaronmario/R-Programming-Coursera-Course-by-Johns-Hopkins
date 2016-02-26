complete  <- function (directory, id=1:332)
{
originalwd <- getwd() #store current working directory
setwd(directory) #change working directory to where csvs are stored

ListOfFiles = list.files(getwd())
selectFiles <- ListOfFiles[id]
summary <- data.frame(id = id, nobs = 0)
    for (i in 1:length(id)) { 
            dataRead <- read.csv(selectFiles[i]) # read relevant files
            nobs <- sum(complete.cases(dataRead)) # complete cases of each file
		summary [i, "nobs"] <- nobs
    }

setwd(originalwd) #reset working directory
return(summary)
}