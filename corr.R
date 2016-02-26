corr <- function (directory, threshold=0)
{
tempCompleteCases<-complete(directory)
x<-nrow(complete(directory))
numCases<-0
completeCaseGreThreshold<-vector(mode="numeric", length=0)
result<-vector(mode="numeric", length=0)


originalwd <- getwd() #store current working directory
setwd(directory) #change working directory to where csvs are stored
listOfFiles2<- list.files(getwd())#get list of all files in directory
numericFileList = as.numeric(sub("\\.csv$","",listOfFiles2))

for(i in 1:x)
{
	if(tempCompleteCases[i,2]>threshold)
	{
		numCases<-numCases+1
		completeCaseGreThreshold<-c(completeCaseGreThreshold,i)
		fileToRead = listOfFiles2[match(i,numericFileList)]
		dataRead <- read.csv(fileToRead) 
		corrtemp<-cor(dataRead [,2], dataRead [,3], use = "pairwise.complete.obs", method = "pearson")
		result<-c(result,corrtemp)
	}
}
setwd(originalwd) #reset working directory
return(result)
}