#################################################################
# Bayesian analysis to get credible intervals for IFR estimates
#################################################################

# Parameters:

# theta_d: IFR
# theta_i: proportion infected

# mu: mean of dead per year in a normal year
# sigma: standard deviation of dead per year in a normal year

###########################################
yearlyDeaths <- dcast(allDeathsByYear, ageRange ~ variable)
yearlyDeaths <- rbind(yearlyDeaths, as.list(c("0,20", rep(0,6))))

# Demographic Data
relevantTownsDemData <- unique(relevantTownsDemData[, c("ageRange", "populationMale", "populationFemale")])
totPopBefore <- relevantTownsDemData[! is.na(ageRange), sum(populationFemale) + sum(populationMale), by = ageRange]
names(totPopBefore)[2] <- "population"

# Likelihood is going to be binomial for each age range, with parameters given by
dataLikelihood <- merge(totPopBefore, yearlyDeaths, by = "ageRange")

# we assume for 2020
# deaths coming from binomial with probability thetad/pop for years before 2020 and  (thetad + theta_i*theta_d)/pop for 2020

model = function(){
  #priors
  #theta_i ~ dunif(0.2,.8)
  thetadCovid[1] ~ dunif(0.0,.2)
  thetadCovid[2] ~ dunif(0.0,.2)
  thetadCovid[3] ~ dunif(0.0,.2)
  thetadCovid[4] ~ dunif(0.0,.2)
  thetadCovid[5] ~ dunif(0.0,.2)
  thetadCovid[6] ~ dunif(0.0,.2)
  thetadCovid[7] ~ dunif(0.0,.2)
  
  thetad[1] ~ dunif(0.0,.2)
  thetad[2] ~ dunif(0.0,.2)
  thetad[3] ~ dunif(0.0,.2)
  thetad[4] ~ dunif(0.0,.2)
  thetad[5] ~ dunif(0.0,.2)
  thetad[6] ~ dunif(0.0,.2)
  thetad[7] ~ dunif(0.0,.2)

  theta_i ~ dbeta(40,20)
  
  #likelihood over the 7 age groups
  for (i in 1:7){
    
    totDeaths15[i] ~ dbin(thetad[i], population[i])
    totDeaths16[i] ~ dbin(thetad[i], population[i])
    totDeaths17[i] ~ dbin(thetad[i], population[i])
    totDeaths18[i] ~ dbin(thetad[i], population[i])
    totDeaths19[i] ~ dbin(thetad[i], population[i])
    totDeaths20[i] ~ dbin(thetad[i] + thetadCovid[i]*theta_i, population[i])
    }
}

model.file="model.txt"
write.model(model,model.file)

# no initial values
inits<-NULL

# what parameters we want to track
params = c("thetad","theta_i")

## hyperparameters
# number of iterations
ni = 10000
# burn in interval
nb = 100
# thinning interval
nt = 1
# number of chains
nc = 10

# compile model
jmod = jags.model(file = model.file, data = dataLikelihood, n.chains = nc, inits = inits, n.adapt = 1000)

# iterate through jmod for the extent of the burn-in
update(jmod, n.iter=nb, by=1)

# draw samples from the posterior for params, given MCMC hyperparameters
post = coda.samples(jmod, params, n.iter = ni, thin = nt)

# diagnostic evaluation of posterior samples
MCMCtrace(post)

MCMCtrace(post,
          type = 'density',
          ind = TRUE)


# objectively assess convergence with gelmans diagnostic
gelman.diag(post)

# get summary of posterior samples for two parameters
MCMCsummary(post, params = c('thetad','theta_i'), digits=2)

