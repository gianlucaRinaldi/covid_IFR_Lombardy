#################################################################
# Bayesian analysis to get credible intervals for IFR estimates
#################################################################

# Parameters:

# theta_d: IFR
# theta_i: proportion infected

# mu: mean of dead per year in a normal year
# sigma: standard deviation of dead per year in a normal year

# Estimate mu and sigma from previous years
totDeathsBefore <- allDeathsByYear[variable != "totDeaths20", sum(value), by = variable]

mu <- mean(totDeathsBefore$V1)
sigma <- sd(totDeathsBefore$V1)

# thefore we have that 
dataDeaths <- allDeathsByYear[variable == "totDeaths20", sum(value)]

dataDeaths <- c(dataDeaths, mu, sigma)
dataDeaths <- as.data.frame(t(dataDeaths))
names(dataDeaths) <- c("totDeaths", "mu", "sigma")

# we assume for 2020
# deaths = mu + theta_i*pop*theta_d + epsilon_2020 where epsilon 2020 is normal with mean zero and std 

model = function(){
  #priors
  theta_d ~ dunif(0,.03)
  theta_i ~ dunif(0.1,.9)
  #theta_i ~ dbeta(.9, 1.2)
  
  #likelihood
  totDeaths ~ dnorm(theta_d*theta_i*50000 + mu, sigma)
}

model.file="model.txt"
write.model(model,model.file)

# no initial values
inits<-NULL

# what parameters we want to track
params = c("theta_d","theta_i", "totDeaths")

## hyperparameters
# number of iterations
ni = 1000
# burn in interval
nb = 100
# thinning interval
nt = 1
# number of chains
nc = 200

# compile model
jmod = jags.model(file = model.file, data = dataDeaths, n.chains = nc, inits = inits, n.adapt = 1000)

# iterate through jmod for the extent of the burn-in
update(jmod, n.iter=nb, by=1)

# draw samples from the posterior for params, given MCMC hyperparameters
post = coda.samples(jmod, params, n.iter = ni, thin = nt)

# diagnostic evaluation of posterior samples
MCMCtrace(post, params = c('theta_d','theta_i'), pdf=F)

# objectively assess convergence with gelmans diagnostic
gelman.diag(post)

# get summary of posterior samples for two parameters
MCMCsummary(post, params = c('theta_d','theta_i'), digits=2)

