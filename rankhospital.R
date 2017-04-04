#install.packages("data.table")
library(data.table)
#I use datatable despite of dataframe, i'm used to use it and it's easier for me, rules are differents but the the principle is the same for here
#I transform the right column on numeric column and then i remove NA, find the right hospital(depend on the "num" parameter) and print it
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
                        num2 = 1
                }else if (num == "worst") {
                        t<-substitute(r$X, list(X=colname))
                        num2 = length(r[,eval(t)])
                }
                return( r[num2,]$`Hospital Name`)
        }
        r[num,]$`Hospital Name`
}
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)



