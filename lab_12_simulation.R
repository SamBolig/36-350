generate_data = function(n, p){
  covariates = matrix(rnorm(p), nrow=n, ncol=p)
  responses = as.vector(rnorm(n))
  return(
    list(covariates, responses)
    )
}

model_select = function(covariates, responses, cutoff){
  coeffs = summary(lm(responses ~ covariates))$coefficients
  sig.covs = which(coeffs[,4] <= cutoff)
  new.coeffs = summary(lm(responses ~ covariates[,sig.covs]))$coefficients
  p.vector = c()
  p.vector = cbind(p.vector, new.coeffs[,4])
  return(
    list(p.vector)
  )
}
