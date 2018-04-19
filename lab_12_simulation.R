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
