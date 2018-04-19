generate_data = function(n, p){
  covariates = matrix(rnorm(p), nrow=n, ncol=p)
  responses = as.vector(rnorm(n))
  return(
    list(covariates, responses)
    )
}

model_select = function(covariates, responses, cutoff){
  coeffs = summary(lm(responses ~ covariates))$coefficients
  which(coeffs[,4] <= cutoff)
}
