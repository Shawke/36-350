generate_data = function(n, p){
  L = list()
  covariates = matrix(nrow = n, ncol = p)
  for(i in 1:p){
    covariates[,i] = rnorm(p)
    }
  responses = rnorm(n)
  L$covariates = covariates
  L$responses = responses
  return(L)
  }

model_select = function(covariates, responses, cutoff){
  m1 = lm(responses ~ covariates, summary(lm(responses~covariates))[[4]][2,4] <= cutoff)
  m2 = lm(responses ~ covariates[, summary(lm(responses~covariates))[[4]][2,4] <= cutoff])
  return(summary(m2)[[4]][2,4])
}


run_simulations = function(n_trials, n, p, cutoff){
  for(i in 1:n_trials){
    data = generate_data(n, p)
    hist(model_select(data$covariates, data$responses, cutoff))
  }
}

for(num in c(100, 1000, 10000)){
  for(pv in c(10, 20, 50)){
    run_simulations(1, num, pv, cutoff = 0.05)
  }
}
