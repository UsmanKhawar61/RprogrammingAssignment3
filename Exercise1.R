outcome<-read.csv("outcome-of-care-measures.csv", colClasses ="character")
head(outcome)
dim(outcome)
names(outcome)
hist(as.numeric(outcome[,11]))



