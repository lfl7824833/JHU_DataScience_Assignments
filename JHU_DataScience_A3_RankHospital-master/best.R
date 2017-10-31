best <- function(state,outcome){
  # point 1. outcome : heart attack, heart failure, pneumonia
  # point 2. 30-day mortality rate. Not readmission rate
  outcomeNumber<-0
  validOutcomes <- c("heart attack","heart failure","pneumonia")
  AllData<-read.csv("outcome-of-care-measures.csv",colClasses = "character",header = TRUE)
  
  # Step 1: Check arguments
  if (!(state %in% AllData$State) ){
    stop("invalid state")
  }
  if (!(outcome %in% validOutcomes) ){
    stop("invalid outcome")
  }
  
  TargetState_Data<-AllData[which(AllData[,7]==state),]
  
  if(outcome==validOutcomes[1]){
    outcomeNumber<-11
  }
  if(outcome==validOutcomes[2]){
    outcomeNumber<-17
  }
  if(outcome==validOutcomes[3]){
    outcomeNumber<-23
  }
  
  shouldDelete<-vector(mode = "logical",length = length(TargetState_Data[,outcomeNumber]))

  for(i in 1:length(TargetState_Data[,outcomeNumber])){
    
    if("Not Available" %in% TargetState_Data[i,outcomeNumber]){
      shouldDelete[i]<-FALSE

    }else{
      shouldDelete[i]<-TRUE
    }
    
  }
  TargetState_Data<-TargetState_Data[shouldDelete,]
#  print(TargetState_Data[,c(7,17)])

  Index<-order(as.numeric(TargetState_Data[,outcomeNumber]))

  Samevalue_Lg<-length(
    which(TargetState_Data[,outcomeNumber]
          ==TargetState_Data[Index[1],outcomeNumber]))
  
  Samevalue_CN<-TargetState_Data[Index[1:Samevalue_Lg],2]
  Samevalue_CN<-sort(as.character(Samevalue_CN))
  c(Samevalue_CN[1],Samevalue_Lg)

}