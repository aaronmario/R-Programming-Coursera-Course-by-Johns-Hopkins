rankhospital <- function(state, outcome, num = "best") {


## Read outcome data
healthData<-read.csv("outcome-of-care-measures.csv", colClasses = "character")


## Check that state and outcome are valid
states<-unique(healthData[,7])
outcomes<-c("heart attack", "heart failure", "pneumonia")
if(!state %in% states)
	stop("invalid state")

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



## Return hospital name in that state with the given rank 30-day death rate


#for heart attack

if(outcome%in%"heart attack")
{
	healthData[,11]=as.numeric(healthData[,11])
	countRows<-1
	sicknessData<- data.frame(x = character(), y = numeric(), stringsAsFactors = FALSE)
	for(i in 1:nrow(healthData))
	{
		if(healthData[i,7] %in% state & !(healthData[i,2] %in% sicknessData$x) & healthData[i,11]<9999 )
		{	
			sicknessData[countRows,]<-list(toString(healthData[i,2]),healthData[i,11])
			countRows<-countRows+1
		}
	}
	sicknessData<-sicknessData[order(sicknessData$y,sicknessData$x),]
}


	
#for heart failure
if(outcome%in%"heart failure")
{
	healthData[,17]=as.numeric(healthData[,17])
	countRows<-1
	sicknessData<- data.frame(x = character(), y = numeric(), stringsAsFactors = FALSE)
	for(i in 1:nrow(healthData))
	{
		if(healthData[i,7] %in% state & !(healthData[i,2] %in% sicknessData$x) & healthData[i,17]<9999 )
		{	
			sicknessData[countRows,]<-list(toString(healthData[i,2]),healthData[i,17])
			countRows<-countRows+1
		}
	}
	sicknessData<-sicknessData[order(sicknessData$y,sicknessData$x),]
}



#for pneumonia
if(outcome%in%"pneumonia")
{
	healthData[,23]=as.numeric(healthData[,23])
	countRows<-1
	sicknessData<- data.frame(x = character(), y = numeric(), stringsAsFactors = FALSE)
	for(i in 1:nrow(healthData))
	{
		if(healthData[i,7] %in% state & !(healthData[i,2] %in% sicknessData$x) & healthData[i,23]<9999 )
		{	
			sicknessData[countRows,]<-list(toString(healthData[i,2]),healthData[i,23])
			countRows<-countRows+1
		}
	}
	sicknessData<-sicknessData[order(sicknessData$y,sicknessData$x),]
}




#Print Final results
if(num %in%"best") return(sicknessData[1,]$x)
else if(num %in%"worst") return(sicknessData[nrow(sicknessData),]$x)
else if(num<=nrow(sicknessData))return(sicknessData[num,]$x)
else return(NA)

}