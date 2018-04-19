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
  mod = lm(responses ~ covariates, responses <= cutoff)
}
