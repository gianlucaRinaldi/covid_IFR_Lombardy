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
yearlyDeaths <- rbind(yearlyDeaths, as.list(c("0-20", rep(0,6))))

# Demographic Data
demsData <- fread(input = "Lodi_2015_2019.csv")

relevantTownsDemDataTemp <- unique(relevantTownsDemData[, c("ageRange", "populationMale", "populationFemale")])
totPopBefore <- relevantTownsDemDataTemp[! is.na(ageRange), sum(populationFemale) + sum(populationMale), by = ageRange]
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

  theta_i ~ dbeta(10,5)
  
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
params = c("thetadCovid","thetad","theta_i")

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

# Model by town
relevantTownsDemDataTemp <- unique(relevantTownsDemData[, c("Denominazione", "ageRange", "populationMale", "populationFemale")])
totPopBeforeTown <- relevantTownsDemDataTemp[! is.na(ageRange), sum(populationFemale) + sum(populationMale), by = c("Denominazione", "ageRange")]
names(totPopBeforeTown)[3] <- "totPop"

demsData <- demsData[Denominazione %in% unique(yearlyDeathsTemp$NOME_COMUNE), ]

yearlyDeathsTemp <- unique(relevantTownsDeathsDataTemp[covidAffectedPeriod == T, c("NOME_COMUNE", "ageRange", "totDeaths15",  "totDeaths16", "totDeaths17", "totDeaths18", "totDeaths19", "totDeaths20")])

dataLikelihoodTown <- merge(demsData, yearlyDeathsTemp, by.x = c("Denominazione", "ageRange"), by.y = c("NOME_COMUNE", "ageRange"), all.x = T)
dataLikelihoodTown[is.na(dataLikelihoodTown),] <- 0

modelTown = function(){
  #priors
  #theta_i ~ dunif(0.2,.8)
  thetadCovid[1] ~ dunif(0.0,.3)
  thetadCovid[2] ~ dunif(0.0,.3)
  thetadCovid[3] ~ dunif(0.0,.3)
  thetadCovid[4] ~ dunif(0.0,.3)
  thetadCovid[5] ~ dunif(0.0,.3)
  thetadCovid[6] ~ dunif(0.0,.3)
  thetadCovid[7] ~ dunif(0.0,.3)
  
  thetad[1] ~ dunif(0.0,.1)
  thetad[2] ~ dunif(0.0,.1)
  thetad[3] ~ dunif(0.0,.1)
  thetad[4] ~ dunif(0.0,.1)
  thetad[5] ~ dunif(0.0,.1)
  thetad[6] ~ dunif(0.0,.1)
  thetad[7] ~ dunif(0.0,.1)
  
  theta_i[1] ~ dunif(0,1)
  theta_i[2] ~ dunif(0,1)
  theta_i[3] ~ dunif(0,1)
  theta_i[4] ~ dunif(0,1)
  theta_i[5] ~ dunif(0,1)
  theta_i[6] ~ dunif(0,1)
  theta_i[7] ~ dunif(0,1)
  #theta_i ~ dbeta(10,5)
  
  #likelihood over the 7 age groups (j) and 7 towns (i)
  for (i in 1:7){
    for (j in 1:7){
      totDeaths15[(i-1)*7 + j] ~ dbin(thetad[j], tot2015[j])
      totDeaths16[(i-1)*7 + j] ~ dbin(thetad[j], tot2016[j])
      totDeaths17[(i-1)*7 + j] ~ dbin(thetad[j], tot2017[j])
      totDeaths18[(i-1)*7 + j] ~ dbin(thetad[j], tot2018[j])
      totDeaths19[(i-1)*7 + j] ~ dbin(thetad[j], tot2019[j])
      totDeaths20[(i-1)*7 + j] ~ dbin(thetad[j] + thetadCovid[j]*theta_i[i], tot2019[j])
    }
  }
}

modelTown.file="model.txt"
write.model(modelTown, modelTown.file)

# no initial values
inits<-NULL

# what parameters we want to track
params = c("thetadCovid","thetad","theta_i")

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
jmod = jags.model(file = model.file, data = dataLikelihoodTown, n.chains = nc, inits = inits, n.adapt = 1000)

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
MCMCsummary(post, params = c('thetad','theta_i', "thetadCovid"), digits=2)
