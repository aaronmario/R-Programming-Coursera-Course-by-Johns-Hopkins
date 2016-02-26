best <- function(state, outcome) {

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
	if(healthData[i,11]=="Not Available")healthData[i,11]<-9999
	if(healthData[i,17]=="Not Available")healthData[i,17]<-9999
	if(healthData[i,23]=="Not Available")healthData[i,23]<-9999
}



## Return hospital name in that state with lowest 30-day death rate
minCases<-9999
minCasesName<-"zzz"


#for heart attack
healthData[,11]=as.numeric(healthData[,11])
if(outcome%in%"heart attack")
{
	for(i in 1:nrow(healthData))
	{
		if(healthData[i,7] %in% state & healthData[i,11]<minCases)
		{	
			minCases<-healthData[i,11]
			minCasesName<-healthData[i,2]
		}
		if(healthData[i,7] %in% state & healthData[i,11]== minCases & healthData[i,2]<minCasesName)	
			minCasesName<-healthData[i,2]
	}
	cat(minCasesName,"\n")
}

	
#for heart failure
healthData[,17]=as.numeric(healthData[,17])
if(outcome%in%"heart failure")
{
	for(i in 1:nrow(healthData))
	{
		if(healthData[i,7] %in% state & healthData[i,17]<minCases)
		{	
			minCases<-healthData[i,17]
			minCasesName<-healthData[i,2]
		}
		if(healthData[i,7] %in% state & healthData[i,17]== minCases & healthData[i,2]<minCasesName)	
			minCasesName<-healthData[i,2]
	}
	cat(minCasesName,"\n")
}



#for pneumonia
healthData[,23]=as.numeric(healthData[,23])
if(outcome%in%"pneumonia")
{
	for(i in 1:nrow(healthData))
	{
		if(healthData[i,7] %in% state & healthData[i,23]<minCases)
		{	
			minCases<-healthData[i,23]
			minCasesName<-healthData[i,2]
		}
		if(healthData[i,7] %in% state & healthData[i,23]== minCases & healthData[i,2]<minCasesName)	
			minCasesName<-healthData[i,2]
	}
	cat(minCasesName,"\n")
}

}