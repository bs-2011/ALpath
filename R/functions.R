#Reading the data
#x = read.csv("HMM_sample_v2.csv",header =T)

#library(depmixS4)
HMMfit  = readRDS(file = "./man/HMMfit.rds")
x = readRDS(file = "./man/x.rds")
HMMpost = depmixS4::posterior(HMMfit)
x = cbind(x,HMMpost$state)
tmp = HMMfit@trDens
transition_mat = matrix(unlist(tmp),ncol=4, byrow=T)

# Saving the Output file
Output_ALpath = x
write.csv(Output_ALpath,file = "/datadrive/dataset/ALpath_OUTPUT/ALpath_output.csv",row.names = F)

#' task1 function
#'
#' This function returns the concepts corresponding to any hidden markov model state
#' @param state -The hidden states in which student can be
#' @param student_ID - Unique value which correspond to particular student
#' @keywords
#' @export
#' @examples
#' task1(3,"stu_0102350af2")

##function for task 1
task1 = function(state,student_ID){
  student_ID = as.character(student_ID)
  index = which(x$`HMMpost$state` == state & x$Anon.Student.Id ==student_ID)
  concept = x[index,15]
  return(concept)

}

#' task2 function
#'
#' This function predicts the hidden state in successive lesson
#' @param student_ID - Unique value which correspond to particular student
#' @keywords
#' @export
#' @examples
#' task2("stu_0102350af2")

#function to predict task2
task2 = function(student_ID){
  student_ID = as.character(student_ID)
  idx= which(x$Anon.Student.Id == student_ID)
  prediction_prob = as.matrix(HMMpost[length(idx),c(2:5)])%*%transition_mat
  new_state  = which.max(prediction_prob)
  return(new_state)
}
