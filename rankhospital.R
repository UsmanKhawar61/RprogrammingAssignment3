##This function is used to find the best hospital in a state
rankhospital<- function(state,outcome, num="best"){
    #Reading the Outcome data located in the working directory
    data<-read.csv("outcome-of-care-measures.csv", colClasses ="character")
    #Checking whether entered state and outcome are valid
    states<- unique (data$State)
    if (!is.element(state, states)){
        stop("invalid state")
    }
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
    state_splitlist<- splitlist[[state]]
    lowest_rate<- state_splitlist[order(as.numeric(state_splitlist[[3]]), state_splitlist[[1]]),]
    ranked<- data.frame(lowest_rate,rank=1:nrow(lowest_rate))
    if (identical(num,"best"))
        ranked[1,1]
    else if (identical(num, "worst")){
        lowest_rate<- state_splitlist[order(as.numeric(state_splitlist[[3]]), state_splitlist[[1]], na.last = NA, decreasing = FALSE),]
        ranked<- data.frame(lowest_rate,rank=1:nrow(lowest_rate))
        ranked[nrow(ranked),1 ]
    }
    else ranked[num, 1]
}