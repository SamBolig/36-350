generate_data = function(n, p){
  covariates = matrix(rnorm(n*p), nrow=n, ncol=p)
  responses = as.vector(rnorm(n))
  return(
    list(covariates, responses)
    )
}

model_select = function(covariates, responses, cutoff){
  coeffs = summary(lm(responses ~ covariates))$coefficients
  sig.covs = which(coeffs[(ncol(covariates+1)*3 + 1):(ncol(covariates+1)*4)] <= cutoff)
  new.coeffs = summary(lm(responses ~ covariates[,sig.covs]))$coefficients
  p.vector = c()
  p.vector = cbind(p.vector, new.coeffs[,4])
  return(
    list(p.vector)
  )
}

data = generate_data(10,3)
covariates = data[[1]]
responses = data[[2]]
summary(lm(responses ~ covariates))$coefficients[((ncol(covariates)+1)*3 + 1):((ncol(covariates)+1)*4)]

model_select(generate_data(5,5)[[1]], generate_data(5,5)[[2]], cutoff = 0.25)
