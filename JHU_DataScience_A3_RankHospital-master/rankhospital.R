rankhospital <- function(state, outcome, num="best"){
  
  
  outcomeNumber<-0
  validOutcomes <- c("heart attack","heart failure","pneumonia")
  AllData<-read.csv("outcome-of-care-measures.csv",colClasses = "character"
                    ,header = TRUE)
  
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
  
  shouldDelete<-vector(mode = "logical",length = 
                         length(TargetState_Data[,outcomeNumber]))
  
  for(i in 1:length(TargetState_Data[,outcomeNumber])){
    
    if("Not Available" %in% TargetState_Data[i,outcomeNumber]){
      shouldDelete[i]<-FALSE
      
    }else{
      shouldDelete[i]<-TRUE
    }
  }
  
  OKdata_ST_OT<-TargetState_Data[shouldDelete,]
    
  Index<-order(as.numeric(OKdata_ST_OT[,outcomeNumber]))
  Sort_OKdata<-OKdata_ST_OT[Index,c(2,7,outcomeNumber)]

  Samevalue_Lg_Vector<-vector(mode = "numeric",length = 
                                length(Sort_OKdata[,3]))
  
  for(i in 1: length(Sort_OKdata[,3])){
    
    Samevalue_Lg_Vector[i]<-length(
      which(Sort_OKdata[,3]
            ==Sort_OKdata[i,3]))
  }

  First<-0
  for(j in 2:length(Sort_OKdata[,3])){
      
    if(Sort_OKdata[j-1,3]==Sort_OKdata[j,3]&&First==0){
      lg<-Samevalue_Lg_Vector[j-1]
      theOrder<-order(Sort_OKdata[(j-1):(j-1+lg-1),1])
      Sort_OKdata[(j-1):(j-1+lg-1),]<-Sort_OKdata[j-1+theOrder-1,]
      First<-1
    }else{
      First<-0
    }
  }

  if (num =="best"){ num <- 1 }
  else if (num == "worst") { num<-length(Sort_OKdata[,3]) }
  else if (num >length(Sort_OKdata[,3])) {Result<-NA}
  else {num<-num}
  
  Result<-Sort_OKdata[num,1]
  Result

}

