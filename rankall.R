rankall <- function(outcome, num = "best") {


## Read outcome data
healthData<-read.csv("outcome-of-care-measures.csv", colClasses = "character")


## Check that state and outcome are valid
outcomes<-c("heart attack", "heart failure", "pneumonia")

if(! outcome %in% outcomes)
	stop("invalid outcome")


##change all Not Available from relevant columns to large numeric value
for(i in 1:nrow(healthData))
{
	#healthData[i,11]<-na.omit(healthData[i,11])

	if(healthData[i,11]=="Not Available")healthData[i,11]<-as.numeric(9999)
	if(healthData[i,17]=="Not Available")healthData[i,17]<-9999
	if(healthData[i,23]=="Not Available")healthData[i,23]<-9999
}


## For each state, find the hospital of the given rank. Return a data frame with the hospital names and the (abbreviated) state name

#for heart attack
if(outcome%in%"heart attack")
{
	healthData[,11]=as.numeric(healthData[,11])
	countRows<-1
	sicknessData<- data.frame(y = numeric(), z=character(),x = character(),stringsAsFactors = FALSE)
	for(i in 1:nrow(healthData))
	{
		if(healthData[i,11]<9999 )
		{	
			sicknessData[countRows,]<-list(healthData[i,11],toString(healthData[i,7]),toString(healthData[i,2]))
			countRows<-countRows+1
		}
	}
	sicknessData<-sicknessData[order(sicknessData$z,sicknessData$y,sicknessData$x),]	
}
	

	
#for heart failure
if(outcome%in%"heart failure")
{
	healthData[,17]=as.numeric(healthData[,17])
	countRows<-1
	sicknessData<- data.frame(y = numeric(), z=character(),x = character(),stringsAsFactors = FALSE)
	for(i in 1:nrow(healthData))
	{
		if(healthData[i,17]<9999 )
		{	
			sicknessData[countRows,]<-list(healthData[i,17],toString(healthData[i,7]),toString(healthData[i,2]))
			countRows<-countRows+1
		}
	}
	sicknessData<-sicknessData[order(sicknessData$z,sicknessData$y,sicknessData$x),]	
}


#for pneumonia
if(outcome%in%"pneumonia")
{
	healthData[,23]=as.numeric(healthData[,23])
	countRows<-1
	sicknessData<- data.frame(y = numeric(), z=character(),x = character(),stringsAsFactors = FALSE)
	for(i in 1:nrow(healthData))
	{
		if(healthData[i,23]<9999 )
		{	
			sicknessData[countRows,]<-list(healthData[i,23],toString(healthData[i,7]),toString(healthData[i,2]))
			countRows<-countRows+1
		}
	}
	sicknessData<-sicknessData[order(sicknessData$z,sicknessData$y,sicknessData$x),]	
}


# final rank frame for partcular hospital name of chosen rank for every state
# and printing data printing
allStates<-unique(sicknessData$z)
	sicknessDataRank<- data.frame(z=character(),x = character(),stringsAsFactors = FALSE)
	for(i in 1:length(allStates))
	{
		firstPos<-which(sicknessData$z==allStates[i])[1]
		lastPosList<-which(sicknessData$z==allStates[i])
		lastPos=lastPosList[length(lastPosList)]
		if(num %in%"best") sicknessDataRank[i,]<-list(toString(sicknessData[firstPos,2]),toString(sicknessData[firstPos,3]))
		else if(num %in%"worst") sicknessDataRank[i,]<-list(toString(sicknessData[lastPos,2]),toString(sicknessData[lastPos,3]))
		else if(as.numeric(num)<(lastPos-firstPos))sicknessDataRank[i,]<-list(allStates[i],toString(sicknessData[(firstPos+as.numeric(num)-1),3]))
		else sicknessDataRank[i,]<-list(allStates[i],"NA")
	}
	return(sicknessDataRank)
}