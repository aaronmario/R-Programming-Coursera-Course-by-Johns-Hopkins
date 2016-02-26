pollutantmean <- function (directory, pollutant, id=1:332)
{
originalwd <- getwd() #store current working directory
setwd(directory) #change working directory to where csvs are stored

ListOfFiles = list.files(getwd())
numericFileList = as.numeric(sub("\\.csv$","",ListOfFiles ))

filesToRead = ListOfFiles[match(id,numericFileList)]
pollutantData <- lapply(filesToRead, read.csv) #read all requested files/ids
pollutantDataFinal = do.call(rbind.data.frame,pollutantData)#convert list to frame

meanOfData <- mean(pollutantDataFinal [,pollutant],na.rm=TRUE)
print(meanOfData)#display mean of specified pollutant


setwd(originalwd) #reset working directory


}