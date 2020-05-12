library(dplyr)
##This function is used to find the best hospital in a state
best<- function(state,outcome){
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
    # lowest_rate_alphabetic<- lowest_rate[order(lowest_rate[[1]]),]
    lowest_rate[1,1]
    
}
