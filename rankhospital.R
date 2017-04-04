#install.packages("data.table")
library(data.table)
rankhospital <- function(state, outcome, num = "best") {
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
        setkeyv(r,c(colname,"Hospital Name"))
        if (is.numeric(num) == TRUE){
                if (nrow(r[,2]) < num)
                        return(NA)
        }else if (is.character(num) == TRUE){
                if (num == "best") {
                        num = 1
                }else if (num == "worst") {
                        num = length(r[[colname]])
                }
        }
        r[num,]$`Hospital Name`
}
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)



