#install.packages("data.table")
library(data.table)
#I use datatable despite of dataframe, i'm used to use it and it's easier for me, rules are differents but the the principle is the same for here
#I transform the right column on numeric column and then i remove NA, for each State, i find the right hospital(depend on the "num" parameter) and i print it
rankall <- function(outcome, num = "best") {
        r2 <- fread("outcome-of-care-measures.csv", colClasses = "character")
        setkey(r2,State)
        #outcome <-"pneumonia"
        #num <- "worst"
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        state <- unique(r2$State)
        
        if ((outcome %in% outcomes) == FALSE)
                stop("invalid outcome")
        if ((outcome == "heart attack")){
                colname<- "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
        }else if (outcome == "heart failure"){
                colname<- "Hospital 30-Day Death (Mortality) Rates from Heart Failure"
        }else {
                colname<- "Hospital 30-Day Death (Mortality) Rates from Pneumonia"
        }
        hospital <- vector(mode="character")
        #i<-52
        for (i in seq(state)) {
                r<-copy(r2[State == state[i]])
                r[,colname] <- as.numeric(unlist(r[,colname,with=FALSE]))
                r<-na.omit(r)
                setkeyv(r,c(colname,"Hospital Name"))
                if (is.numeric(num) == TRUE){
                        if (nrow(r[,2]) < num)
                        {
                                hospital[i] <- NA_character_
                                next 
                        }
                }else if (is.character(num) == TRUE){
                        if (num == "best") {
                                num2 = 1
                        }else if (num == "worst") {
                                t<-substitute(r$X, list(X=colname))
                                num2 = length(r[,eval(t)])
                        }
                        hospital[i] <-  r[num2,`Hospital Name`]
                        next
                }
                hospital[i] <-  r[num,`Hospital Name`]
        }
        data.frame(hospital, state)
}
head(rankall("heart attack",20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
