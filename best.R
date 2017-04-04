library(data.table)
#I use datatable despite of dataframe, i'm used to use it and it's easier for me, rules are differents but the the principle is the same for here
#i transform the right column on numeric column and then i remove NA, find and return the hospital with the least number of deaths 
best <- function(state, outcome) {
        r <- fread("outcome-of-care-measures.csv", colClasses = "character")
        states <- unique(r$State)
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if ((state %in% states) == FALSE)
                stop("invalid state")
        if ((outcome %in% outcomes) == FALSE)
                stop("invalid outcome")
        r<-r[State == state]
        if ((outcome == "heart attack")){
                colname<- "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
        }else if (outcome == "heart failure"){
                colname<- "Hospital 30-Day Death (Mortality) Rates from Heart Failure"
        }else {
                colname<- "Hospital 30-Day Death (Mortality) Rates from Pneumonia"
        }
        
        r[,colname] <- as.numeric(unlist(r[,colname,with=FALSE]))
        r<-na.omit(r)
        t<-substitute(r$X, list(X=colname))
        r<-r[which(r[,eval(t)] == min(r[,eval(t)]))]
        if(nrow(r)>1){
                r <- sort(r)
                r[1]$`Hospital Name` 
        }else
                r$`Hospital Name`   
        
}
best("TX","heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

