rankall <- function(outcome, num="best"){
    #Reading the Outcome data located in the working directory
    data<-read.csv("outcome-of-care-measures.csv", colClasses ="character")
    #Checking whether entered state and outcome are valid
    states<- unique (data$State)
    if (identical(outcome,"heart attack")){
        recordlist<- data %>% select(2, 7, 11)
        splitlist<- split(recordlist, recordlist$State)
    }
    else if (identical(outcome,"heart failure")){
        recordlist<- data %>% select(2, 7, 17)
        splitlist<- split(recordlist, recordlist$State)
    }
    
    else if (identical(outcome,"pneumonia")){
        recordlist<- data %>% select(2, 7, 23)
        splitlist<- split(recordlist, recordlist$State)
        
    }
    else{
        stop("invalid outcome")
    }
    for (i in 1:length(states)){
        state_splitlist<- splitlist[[states[i]]]
        lowest_rate<- state_splitlist[order(as.numeric(state_splitlist[[3]]), state_splitlist[[1]]),]
        ranked<- data.frame(lowest_rate,rank=1:nrow(lowest_rate))
        if (identical(num,"best"))
            print(ranked[1,])
        else if (identical(num, "worst")){
            lowest_rate<- state_splitlist[order(as.numeric(state_splitlist[[3]]), state_splitlist[[1]], na.last = NA, decreasing = FALSE),]
            ranked<- data.frame(lowest_rate,rank=1:nrow(lowest_rate))
            print(ranked[nrow(ranked), ])
        }
        else print(ranked[num, ])
    }
}