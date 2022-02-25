## data 


## testpahsen
 prob = c(0.9,0.85, 0.80, 0.75, 0.70, 0.65)
## stimuli A korrekt
list_stim_A = list()
cue_matrix = matrix(NA, 40,3)

 for (v in 1:6) {
 cue_matrix = matrix(NA, 40,3)
 cue_a = rbinom(40, 1, prob[v])
 cue_matrix[,1] = cue_a
   for (i in 1:40) {
     if (cue_matrix[i,1] == 1) {
       cue_matrix[i,2] = 0
     } 
     if (cue_matrix[i,1] == 0){
       cue_matrix[i,2] = 1}
     }
     
     
 
 
 colnames(cue_matrix) = c("A", "B", "Correct")
 cue_matrix[,3] = rep(0, 40)
 list_stim_A = append(list(cue_matrix), list_stim_A)
 
 }


 sixth = list_stim_A[[1]]
 fifth = list_stim_A[[2]]
 fourth = list_stim_A[[3]]
 third = list_stim_A[[4]]
 second = list_stim_A[[5]]
 first = list_stim_A[[6]]
 
 
 
 
 
 trials_stimuli_A = list()

 
 for (i in 1:45){
   trial = as.data.frame(rbind(
     first[i,], second[i,], third[i,], fourth[i,], fifth[i,],sixth[i,]
   ))

   
   trials_stimuli_A = append(list(trial), trials_stimuli_A)

   
 }

 
## stimuli B korrekt
 list_stim_B = list()
 cue_matrix = matrix(NA, 40, 3)
 
 for (v in 1:6) {
   cue_matrix = matrix(NA, 40,3)
   cue_b = rbinom(40, 1, prob[v])
   cue_matrix[,2] = cue_b
   for (i in 1:40) {
     if (cue_matrix[i,2] == 1) {
       cue_matrix[i,1] = 0
     } 
     if (cue_matrix[i,2] == 0){
       cue_matrix[i,1] = 1}
   }
   
   
   
   
   colnames(cue_matrix) = c("A", "B", "correct")
   cue_matrix[,3] = rep(1, 40)
   list_stim_B = append(list(cue_matrix), list_stim_B)
   
 }
 
 
 sixth = list_stim_B[[1]]
 fifth = list_stim_B[[2]]
 fourth = list_stim_B[[3]]
 third = list_stim_B[[4]]
 second = list_stim_B[[5]]
 first = list_stim_B[[6]]
 
 
 
 
 
 trials_stimuli_B = list()
 
 
 for (i in 1:40){
   trial = as.data.frame(rbind(
     first[i,], second[i,], third[i,], fourth[i,], fifth[i,],sixth[i,]
   ))
   
   
   trials_stimuli_B = append(list(trial), trials_stimuli_B)
   
   
 }
 
 
## full stimuli set a and b 
 
 x = sample(seq(1,80,1), 80, replace = FALSE, prob = NULL)
 
Full_trials_list = c(trials_stimuli_A, trials_stimuli_B)
 
Full_trials_list = sample(Full_trials_list, 80, replace = FALSE, prob = NULL)

Full_trials_list

## create output matrix 

stim_matrix = matrix(NA, 80, 13)

for (i in 1:(length(Full_trials_list))){
  dat = Full_trials_list[[i]]
  stim_matrix[i,13] = dat[1,3]
  stim_matrix = as.data.frame(stim_matrix)
  stim_matrix[i,1:12] = as.numeric(c(dat$A, dat$B))
  
}

colnames(stim_matrix) = c("a1", "a2", "a3", "a4", "a5", "a6", "b1", "b2", "b3", "b4", "b5", "b6", "cor")

write.csv(stim_matrix, "stim_matrix.csv")


stim_matrix

##learphases

prob = c(0.9,0.85, 0.80, 0.75, 0.70, 0.65)
## stimuli A korrekt
list_stim_A = list()
cue_matrix = matrix(NA, 15,3)

for (v in 1:6) {
  cue_matrix = matrix(NA, 15,3)
  cue_a = rbinom(15, 1, prob[v])
  cue_matrix[,1] = cue_a
  for (i in 1:15) {
    if (cue_matrix[i,1] == 1) {
      cue_matrix[i,2] = 0
    } 
    if (cue_matrix[i,1] == 0){
      cue_matrix[i,2] = 1}
  }
  
  
  
  
  colnames(cue_matrix) = c("A", "B", "Correct")
  cue_matrix[,3] = rep(0, 15)
  list_stim_A = append(list(cue_matrix), list_stim_A)
  
}


sixth = list_stim_A[[1]]
fifth = list_stim_A[[2]]
fourth = list_stim_A[[3]]
third = list_stim_A[[4]]
second = list_stim_A[[5]]
first = list_stim_A[[6]]





trials_stimuli_A = list()


for (i in 1:15){
  trial = as.data.frame(rbind(
    first[i,], second[i,], third[i,], fourth[i,], fifth[i,],sixth[i,]
  ))
  
  
  trials_stimuli_A = append(list(trial), trials_stimuli_A)
  
  
}


## stimuli B korrekt
list_stim_B = list()
cue_matrix = matrix(NA, 15,3)

for (v in 1:6) {
  cue_matrix = matrix(NA, 15,3)
  cue_b = rbinom(15, 1, prob[v])
  cue_matrix[,2] = cue_b
  for (i in 1:15) {
    if (cue_matrix[i,2] == 1) {
      cue_matrix[i,1] = 0
    } 
    if (cue_matrix[i,2] == 0){
      cue_matrix[i,1] = 1}
  }
  
  
  
  
  colnames(cue_matrix) = c("A", "B", "correct")
  cue_matrix[,3] = rep(1, 15)
  list_stim_B = append(list(cue_matrix), list_stim_B)
  
}


sixth = list_stim_B[[1]]
fifth = list_stim_B[[2]]
fourth = list_stim_B[[3]]
third = list_stim_B[[4]]
second = list_stim_B[[5]]
first = list_stim_B[[6]]





trials_stimuli_B = list()


for (i in 1:15){
  trial = as.data.frame(rbind(
    first[i,], second[i,], third[i,], fourth[i,], fifth[i,],sixth[i,]
  ))
  
  
  trials_stimuli_B = append(list(trial), trials_stimuli_B)
  
  
}


## full stimuli set a and b 

x = sample(seq(1,80,1), 80, replace = FALSE, prob = NULL)

Full_trials_list = c(trials_stimuli_A, trials_stimuli_B)

Full_trials_list = sample(Full_trials_list, 30, replace = FALSE, prob = NULL)

Full_trials_list

## create output matrix 

stim_matrix = matrix(NA, 30, 13)

for (i in 1:(length(Full_trials_list))){
  dat = Full_trials_list[[i]]
  stim_matrix[i,13] = dat[1,3]
  stim_matrix = as.data.frame(stim_matrix)
  stim_matrix[i,1:12] = as.numeric(c(dat$A, dat$B))
  
}

colnames(stim_matrix) = c("a1", "a2", "a3", "a4", "a5", "a6", "b1", "b2", "b3", "b4", "b5", "b6", "cor")

write.csv(stim_matrix, "stim_matrix_learn.csv")


stim_matrix







  







