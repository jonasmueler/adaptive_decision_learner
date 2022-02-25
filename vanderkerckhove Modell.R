prob = c(0.5,0.5, 0.5, 0.5, 0.5, 0.5)
results = matrix(NA, 600, 4)
par = c(2, 0.2, 1)

reinf<-function(par,data) {
  
  ## WADD für predictions
  
  WADD <- function(data){
    sum_a = sum(data[, 1] * prob)
    sum_b = sum(data[, 2] * prob)
    if (sum_a > sum_b){
      return(1)
    } else{
      return(0)
    }
    
  }
  
  ##
  
  results = matrix(NA, 600, 2)
  tresh_start = par[3]
  tresh_a = tresh_start 
  tresh_b = tresh_start
  
  
  for (i in 1:length(trials_stimuli)){
    ##parameters 
    lambda = par[1]
    tlc =   par[2]
    ##decision
    data = trials_stimuli[[i]]
    evidence_a = data[, 1] * prob
    evidence_b = data[, 2] * prob
  
    cum_a = evidence_a
    cum_b = evidence_b
    ## Umrechnung in log-scale 
   for (d in 1:6) {
     if (cum_a[d] > cum_b[d]) {
       cum_a[d] = log(cum_a[d]/(1-cum_a[d])) 
     }
     if (cum_b[d] > cum_a[d]) {
       cum_b[d] = log(cum_b[d]/(1-cum_b[d]))
     }
   }
   cum_a = cumsum(cum_a)
   cum_b = cumsum(cum_b)

     ## akkumulation
    for (x in 1:6){

      if (cum_a[x] >= tresh_a | cum_b[x] >= tresh_b){
        cum_evi_a = cum_a[x]
        cum_evi_b = cum_b[x]
        break
        } 
      else {cum_evi_a = cum_a[x] 
    cum_evi_b = cum_b[x]}
    }
   
    results[i, ] = c(cum_evi_a, cum_evi_b)
    print(x)
    ##updates
    
    if (cum_evi_a > cum_evi_b ) {
      bem_a = abs(abs(cum_evi_a) - abs(cum_evi_b))  
      tresh_a = tresh_a + lambda*(tlc- bem_a)
      tresh_b = tresh_b
     }
    
    else if (cum_evi_a < cum_evi_b) {
      bem_b = abs(abs(cum_evi_b) - abs(cum_evi_a)) 
      tresh_b = tresh_b + lambda*(tlc- bem_b)
      tresh_a = tresh_a
    
      
    } 
    
    
  }
  
  ##predictions 
  res = results
  pred_WADD = matrix(NA, length(trials_stimuli), 2)
  pred = matrix(NA, length(trials_stimuli), 2)
  for (d in 1:length(trials_stimuli)) {
    if (res[d,1] > res[d,2]){
      pred[d,1] = 1
      pred[d,2] = 0
    } else if (res[d,1] < res[d,2]){
      pred[d, 1] = 0
      pred[d, 2] = 1
    } else if (res[d,1] == res[d,2]){
      pred[d, 1] = rbinom(1,1, 0.5)
      pred[d, 2] = abs(pred[d,1] -1)
    }
    
    pred_WADD[d,1] = WADD(trials_stimuli[[d]])
    pred_WADD[d,2] = abs(pred_WADD[d,1] -1)
    
  }
  
  acc = rep(NA,length(trials_stimuli))
  
  for (v in 1:length(trials_stimuli)){
    if (pred_WADD[v] == pred[v]) {
      acc[v] = 1
    } else {acc[v] = 0}
  }
 acc = mean(acc)
 
 pay_off = acc * 100 - (15 * x)
  #return(list(results, pred, acc))
  #return(-acc)
  #return(-pay_off)
   }



res = reinf(fit_2$par, trials_stimuli)
res


## parameter optimization 
library(cmaes)
fit = optim(par = runif(3), reinf, data = trials_stimuli, lower = -Inf, upper = Inf)
fit_2 = cma_es(runif(3), reinf, data=trials_stimuli, control = list(trace = 0))

fit_2$par



