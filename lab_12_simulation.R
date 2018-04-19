generate_data = function(n, p){
  covariates = matrix(rnorm(n*p), nrow=n, ncol=p)
  responses = as.vector(rnorm(n))
  return(
    list(covariates, responses)
    )
}

model_select = function(covariates, responses, cutoff){
  coeffs = summary(lm(responses ~ covariates))$coefficients
  p = coeffs[((ncol(covariates)+1)*3 + 1):((ncol(covariates)+1)*4)]
  sig.vars = covariates[,which(p <= cutoff) - 1]
  new.coeffs = summary(lm(responses ~ sig.vars))$coefficients
  p.vector = c()
  p.vector = cbind(p.vector, new.coeffs[((ncol(sig.vars)+1)*3 + 1):((ncol(sig.vars)+1)*4)])
  return(
    list(p.vector)
  )
}

data = generate_data(100,9)
covariates = data[[1]]
responses = data[[2]]
ps = summary(lm(responses ~ covariates))$coefficients[((ncol(covariates)+1)*3 + 1):((ncol(covariates)+1)*4)]

which(ps <= 0.5) - 1

sig.vars = covariates[,c((which(ps <= 0.5) - 1))]

new.coeffs = summary(lm(responses ~ sig.vars))$coefficients
p.vector = c()
p.vector = cbind(p.vector, new.coeffs[((ncol(sig.vars)+1)*3 + 1):((ncol(sig.vars)+1)*4)])

model_select(generate_data(5,5)[[1]], generate_data(5,5)[[2]], cutoff = 0.25)
