#preprocessing the data
#x = read.csv("HMM_sample_v2.csv",header =T)

#library(depmixS4)
HMMfit  = readRDS(file = "./man/HMMfit.rds")
x = readRDS(file = "./man/x.rds")
HMMpost = depmixS4::posterior(HMMfit)
x = cbind(x,HMMpost$state)
tmp = HMMfit@trDens
transition_mat = matrix(unlist(tmp),ncol=4, byrow=T)

##function for task 1
task1 = function(state,y){
  y = as.character(y)
  index = which(x$`HMMpost$state` == state & x$Anon.Student.Id ==y)
  concept = x[index,15]
  return(concept)

}

#function to predict task2
task2 = function(y){
  y = as.character(y)
  idx= which(x$Anon.Student.Id == y)
  prediction_prob = as.matrix(HMMpost[length(idx),c(2:5)])%*%transition_mat
  new_state  = which.max(prediction_prob)
  return(new_state)
}
