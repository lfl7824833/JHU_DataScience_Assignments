getHospital_SingleState <- function(SingleState,outcomeNumber,num){

  shouldDelete<-vector(mode = "logical",length = 
                         length(SingleState[,outcomeNumber]))
  
  for(i in 1:length(SingleState[,outcomeNumber])){
    
    if("Not Available" %in% SingleState[i,outcomeNumber]){
      shouldDelete[i]<-FALSE
      
    }else{
      shouldDelete[i]<-TRUE
    }
  }
  
  Data_nonNAs<-SingleState[shouldDelete,]
  
  Index<-order(as.numeric(Data_nonNAs[,outcomeNumber]))
  Sort_OKdata<-Data_nonNAs[Index,c(2,7,outcomeNumber)]

  Samevalue_Lg_Vector<-vector(mode = "numeric",length = 
                                length(Sort_OKdata[,3]))
  
  for(i in 1: length(Sort_OKdata[,3])){
    
    Samevalue_Lg_Vector[i]<-length(
      which(Sort_OKdata[,3]
            ==Sort_OKdata[i,3]))
  }
  
  First<-0
  if(length(Sort_OKdata[,3])>1){
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
  }
  
  if (num =="best"){ num <- 1 }
  else if (num == "worst") { num<-length(Sort_OKdata[,3]) }
  else if (num >length(Sort_OKdata[,3])) {Result<-c(NA,Sort_OKdata[,2])}
  else {num<-num}
  
  Result<-Sort_OKdata[num,c(1,2)]
  return(Result)
}

rankall <- function(outcome,num="best"){
  
  outcomeNumber<-0
  validOutcomes <- c("heart attack","heart failure","pneumonia")
  AllData<-read.csv("outcome-of-care-measures.csv",colClasses = "character"
                    ,header = TRUE)
  
  # Step 1: Check arguments
  if (!(outcome %in% validOutcomes) ){
    stop("invalid outcome")
  }
   
  if(outcome==validOutcomes[1]){
    outcomeNumber<-11
  }
  if(outcome==validOutcomes[2]){
    outcomeNumber<-17
  }
  if(outcome==validOutcomes[3]){
    outcomeNumber<-23
  }

  
  SingleState<-split(AllData,AllData$State)
  result<-lapply(SingleState,getHospital_SingleState,
                 outcomeNumber=outcomeNumber,
                 num=num)

  print(result)
  
}
