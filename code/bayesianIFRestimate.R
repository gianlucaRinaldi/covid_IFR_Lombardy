#################################################################
# Bayesian analysis to get credible intervals for IFR estimates
#################################################################

# Parameters:

# theta_d: IFR
# theta_i: proportion infected

# mu: mean of dead per year in a normal year
# sigma: standard deviation of dead per year in a normal year

###########################################
# Estimate mu and sigma from previous years
totDeathsBefore <- allDeathsByYear[variable != "totDeaths20",]
# Demographic Data
relevantTownsDemData <- unique(relevantTownsDemData[, c("ageRange", "populationMale", "populationFemale")])
totPopBefore <- relevantTownsDemData[! is.na(ageRange), sum(populationFemale) + sum(populationMale), by = ageRange]

deathRates <- merge(totPopBefore, totDeathsBefore[, mean(value), by = ageRange], by = "ageRange", all.x = T)

names(deathRates) <- c("ageRange", "population", "deathsMean")
deathRates[ageRange == "0-20", deathsMean := 0]
deathRates[deathsMean < 1, deathsMean := 1]

# Likelihood is going to be binomial for each age range, with parameters given by

mu <- totDeathsBefore[, mean(value), by = ageRange]
sigma <- sd(totDeathsBefore$V1)


# thefore we have that 
dataDeaths2020 <- allDeathsByYear[variable == "totDeaths20", ]
dataDeaths2020 <- dataDeaths2020[, c("ageRange", "value")]
names(dataDeaths2020)[2] <- "deaths2020"
dataDeaths2020 <- rbind(dataDeaths2020, list("0-20", 0))

dataLikelihood <- merge(deathRates, dataDeaths2020, by = "ageRange")

# we assume for 2020
# deaths = mu + theta_i*pop*theta_d + epsilon_2020 where epsilon 2020 is normal with mean zero and std 

model = function(){
  #priors
  #theta_i ~ dunif(0.2,.8)
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
    prob[i] <- deathsMean[i]/population[i]
    deaths2020[i] ~ dbin(prob[i] + thetad[i]*theta_i, population[i])
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

MCMCtrace(post, params = c('thetad','theta_i'), 
          type = 'density',
          ind = TRUE, 
          pdf = FALSE)


# objectively assess convergence with gelmans diagnostic
gelman.diag(post)

# get summary of posterior samples for two parameters
MCMCsummary(post, params = c('thetad','theta_i'), digits=2)

