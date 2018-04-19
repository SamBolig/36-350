generate_data = function(n, p){
  covariates = matrix(rnorm(n*p), nrow=n, ncol=p)
  responses = as.vector(rnorm(n))
  return(
    list(covariates, responses)
    )
}


model_select = function(covariates, responses, cutoff){
  coeffs = summary(lm(responses ~ covariates))$coefficients
  p = coeffs[,4]
  sig.vars = covariates[,c((which(p <= cutoff) - 1))]
  if(sum(sig.vars) > 0){ # Terminate if only the intercept is relevant
    new.coeffs = summary(lm(responses ~ sig.vars))$coefficients
    p.vector = new.coeffs[,4]
    return(
      list(p.vector)
    )
    } else {
      return(
        list(c())
        )
    }
}

run_simulation = function(n_trials, n, p, cutoff){
  p.vals = c()
  for(i in 1:(n_trials)){
    data = generate_data(1000, 20)
    covariates = data[[1]]
    responses = data[[2]]
    p.list = model_select(covariates, responses, 0.05)
    p.vals = rbind(p.vals, p.list[[1]][i])
  }
  p.vals
  return(hist(p.vals[[1]]))
}

run_simulation(n_trials = 1000, n = 100, p = 10, cutoff = 0.5)

run_simulation(n_trials = 1000, n = 1000, p = 10, cutoff = 0.5)

run_simulation(n_trials = 1000, n = 10000, p = 10, cutoff = 0.5)

run_simulation(n_trials = 1000, n = 100, p = 20, cutoff = 0.5)

run_simulation(n_trials = 1000, n = 1000, p = 20, cutoff = 0.5)

run_simulation(n_trials = 1000, n = 10000, p = 20, cutoff = 0.5)

run_simulation(n_trials = 1000, n = 100, p = 50, cutoff = 0.5)

run_simulation(n_trials = 1000, n = 1000, p = 50, cutoff = 0.5)

run_simulation(n_trials = 1000, n = 10000, p = 50, cutoff = 0.5)

