generate_data = function(n, p){
  covariates = matrix(rnorm(p), nrow=n, ncol=p)
  responses = as.vector(rnorm(n))
  return(
    list(covariates, responses)
    )
}
