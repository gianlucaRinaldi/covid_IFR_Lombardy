# Estimate the CFR from early italian lockdown cities
setwd("/Users/grinaldi/Documents/randomGames/covid")
# Codogno, Castiglione d’Adda, Casalpusterlengo, Fombio, Maleo, Somaglia, Bertonico, Terranova dei Passerini, Castelgerundo e San Fiorano
library(data.table)
library(ggplot2)
# load first demographic data
lodiDem <- fread("Lodi.csv")

relevantTowns <- c("Codogno", "Castiglione d'Adda", "Casalpusterlengo", "Fombio", "Maleo", "Somaglia", "Bertonico", "Terranova dei Passerini", "Castelgerundo", "San Fiorano")
#relevantTowns <- "Robbio"

relevantTownsDemData <- lodiDem[Denominazione %in% relevantTowns, ]
relevantTownsDemData[`Età` == "Totale", sum(`Totale Femmine`)]

# Load deaths by date
# deathsData <- fread("comune_giornoUpTo21March.csv") # old data
deathsData <- fread("comune_giornoUpTo28March.csv") # new data

# Define more broad age categories
deathsData[, ageRange := cut(CL_ETA, c(0, 4, 8, 10, 12, 14, 16, 22), labels = c("0-20", "21-40", "41-50", "51-60", "61-70","71-80", "81+"), include.lowest = T)]
relevantTownsDeathsData <- deathsData[NOME_COMUNE %in% relevantTowns, ]

# Remove towns for which 2020 deaths data is missing
relevantTownsDeathsData <- relevantTownsDeathsData[! (TOTALE_20 == 9999), ]

# We have deaths data for 7 towns!
relevantTownsDeathsData[, unique(NOME_COMUNE)]
relevantTownsDemData <- relevantTownsDemData[Denominazione %in% relevantTownsDeathsData[, unique(NOME_COMUNE)], ] 

# Compute excess deaths by age and by sex
relevantTownsDeathsData[, dailyDeaths2020Male := sum(MASCHI_20), by = c("NOME_COMUNE", "ageRange", "GE")]
relevantTownsDeathsData[, dailyDeaths2020Female := sum(FEMMINE_20), by = c("NOME_COMUNE", "ageRange", "GE")]

relevantTownsDeathsData[, meanDailyDeathsPreviousYearsMale := (sum(MASCHI_15) + sum(MASCHI_16) + sum(MASCHI_17) + sum(MASCHI_18) + sum(MASCHI_19))/5, by = c("NOME_COMUNE", "ageRange", "GE")]
relevantTownsDeathsData[, meanDailyDeathsPreviousYearsFemale := (sum(FEMMINE_15) + sum(FEMMINE_16) + sum(FEMMINE_17) + sum(FEMMINE_18) + sum(FEMMINE_19))/5, by = c("NOME_COMUNE", "ageRange", "GE")]

relevantTownsDeathsData[, excess2020DeathsMale := unique(dailyDeaths2020Male - meanDailyDeathsPreviousYearsMale), by = c("NOME_COMUNE", "ageRange", "GE")]
relevantTownsDeathsData[, excess2020DeathsFemale := unique(dailyDeaths2020Female - meanDailyDeathsPreviousYearsFemale), by = c("NOME_COMUNE", "ageRange", "GE")]

# Remove dates after march 21st because no 2020 data
relevantTownsDeathsData <- relevantTownsDeathsData[GE <= 328,]
relevantTownsDeathsData[, covidAffectedPeriod := (GE %in% 221:328)]

# Look at total deaths by age cathegories to have an idea of variance
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths15 := sum(MASCHI_15 + FEMMINE_15), by = "ageRange"]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths16 := sum(MASCHI_16 + FEMMINE_16), by = "ageRange"]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths17 := sum(MASCHI_17 + FEMMINE_17), by = "ageRange"]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths18 := sum(MASCHI_18 + FEMMINE_18), by = "ageRange"]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths19 := sum(MASCHI_19 + FEMMINE_19), by = "ageRange"]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths20 := sum(MASCHI_20 + FEMMINE_20), by = "ageRange"]

# standard deviation total deaths in years before 2020
allDeathsByYear <- unique(relevantTownsDeathsData[(! is.na(totDeaths15)) & (! is.na(totDeaths16)) & (! is.na(totDeaths17)) & (! is.na(totDeaths18)) & (! is.na(totDeaths19)) & (! is.na(totDeaths20)), c("ageRange", "totDeaths15","totDeaths16", "totDeaths17", "totDeaths18", "totDeaths19", "totDeaths20")])

relevantTownsDeathsData[covidAffectedPeriod == T,  sd(c(sum(MASCHI_15 + FEMMINE_15),sum(MASCHI_16 + FEMMINE_16), sum(MASCHI_17 + FEMMINE_17), sum(MASCHI_18 + FEMMINE_18), sum(MASCHI_19 + FEMMINE_19))),]
allDeathsByYear <- melt(allDeathsByYear)
deathsByYearOverall <- allDeathsByYear[, sum(value), by = variable]
deathsByYearOverall[, variable := substr(variable, 10,11)]
ggplot(deathsByYearOverall, aes(variable, V1)) + geom_bar(stat = "identity") + xlab("") + ylab("Total deaths between Feb 21 and March 28") + theme_bw(base_size = 12)
sd(allDeathsByYear[! variable == "totDeaths20", sum(value), by = variable]$V1)
sdByAge <- allDeathsByYear[! variable == "totDeaths20", sd(value), by = c("ageRange")]

relevantTownsDeathsData <- unique(relevantTownsDeathsData[,c("NOME_COMUNE", "ageRange", "GE", "covidAffectedPeriod", "excess2020DeathsMale", "excess2020DeathsFemale")])


# Do excess deaths seem to decrease?
excessDeathsByDate <- relevantTownsDeathsData[, sum(excess2020DeathsMale+excess2020DeathsFemale), by = GE]
names(excessDeathsByDate)[2] <- "excessDeaths"
excessDeathsByDate[, month := substr(as.character(GE), 1,1)]
excessDeathsByDate[, day := substr(as.character(GE), 2,3)]
excessDeathsByDate[, date := as.Date(paste0(month, "/", day, "/2020"), format = "%m/%d/%Y")]
excessDeathsByDate <- excessDeathsByDate[order(date),]
excessDeathsByDate[, rollAvg3ExcessDeaths := frollmean(excessDeaths, n = 5)]

ggplot(excessDeathsByDate, aes(date, excessDeaths)) + 
  geom_line() + 
  theme_bw(base_size = 16) + 
  xlab("") + 
  ylab("Excess Deaths") + 
  geom_hline(yintercept = 0)

# Do excess deaths seem to decrease by age?
excessDeathsByDateAgeRange <- relevantTownsDeathsData[, sum(excess2020DeathsMale+excess2020DeathsFemale), by = c("GE", "ageRange")]
names(excessDeathsByDateAgeRange)[3] <- "excessDeaths"
excessDeathsByDateAgeRange[, month := substr(as.character(GE), 1,1)]
excessDeathsByDateAgeRange[, day := substr(as.character(GE), 2,3)]
excessDeathsByDateAgeRange[, date := as.Date(paste0(month, "/", day, "/2020"), format = "%m/%d/%Y")]
excessDeathsByDateAgeRange <- excessDeathsByDateAgeRange[order(date),]

ggplot(excessDeathsByDateAgeRange, aes(date, excessDeaths, color = ageRange)) + 
  geom_line() + 
  theme_bw(base_size = 16) + 
  xlab("") + 
  ylab("Excess Deaths") + 
  geom_hline(yintercept = 0)

excessDeathsByDateAgeRange[GE %in% 221:328, sum(excessDeaths), by = ageRange]

excessDeaths <- relevantTownsDeathsData[, list(sum(excess2020DeathsFemale), sum(excess2020DeathsMale)), by = c("NOME_COMUNE", "ageRange", "covidAffectedPeriod")]
names(excessDeaths)[4:5] <- c("excessMale", "excessFemale")
excessDeaths <- excessDeaths[covidAffectedPeriod == T, ]

# Look at demograpgics
relevantTownsDemData[, age2020 := as.numeric(`Età`) + 2] # data are from 2018
relevantTownsDemData[, ageRange := cut(age2020, c(0, 20, 40, 50, 60, 70, 80,  200), labels = c("0-20", "21-40", "41-50", "51-60", "61-70","71-80", "81+"), include.lowest = T)]
relevantTownsDemData[, populationMale := sum(`Totale Maschi`), by = c("Denominazione", "ageRange")]
relevantTownsDemData[, populationFemale := sum(`Totale Femmine`), by = c("Denominazione", "ageRange")]

# Average age
relevantTownsDemData[! is.na(age2020), sum(age2020*(`Totale Femmine` + `Totale Maschi`))/sum((`Totale Femmine` + `Totale Maschi`))]
relevantTownsDemData[! is.na(age2020), sum(`Totale Femmine`)]
relevantTownsDemData[! is.na(age2020), sum(`Totale Femmine`)]
relevantTownsDemData[! is.na(age2020), sum(`Totale Maschi`)]

demographics <- unique(relevantTownsDemData[, c("Denominazione", "ageRange", "populationMale", "populationFemale")])

# Demographis table
relevantTownsDemData[, ]


cfrData <- merge(excessDeaths, demographics, by.x = c("ageRange", "NOME_COMUNE"), by.y = c("ageRange", "Denominazione"), all.y = T, all.x = T)
cfrData <- cfrData[!is.na(ageRange),]

cfrData[, excessFemale := ifelse(is.na(excessFemale), 0, excessFemale)]
cfrData[, excessMale := ifelse(is.na(excessMale), 0, excessMale)]
cfrData[, allInfectedMaleCFR := excessMale/populationMale]
cfrData[, allInfectedFemaleCFR := excessFemale/populationFemale]

cfrData[, sum(populationMale) + sum(populationFemale), by = ageRange]

cfrData[, allTownsAllInfectedMaleCFR := sum(excessMale)/sum(populationMale), by = ageRange]
cfrData[, allTownsAllInfectedFemaleCFR := sum(excessFemale)/sum(populationFemale), by = ageRange]

# Add correction for uncertainty on excess deaths
names(sdByAge)[2] <- "sdDeaths"
sdByAge <- rbind(sdByAge, list("0-20", 0.4))

cfrData <- merge(cfrData, sdByAge, by = "ageRange")
cfrData[, allTowns50InfectedMaleCFR := sum(excessMale)/sum(populationMale*.5)*100, by = ageRange]
cfrData[, allTowns50InfectedFemaleCFR := (sum(excessFemale) + unique(sdDeaths))/sum(populationFemale*.5)*100, by = ageRange]

cfrData[, allTowns50InfectedMaleCFRAdjusted := (sum(excessMale) + 2*unique(sdDeaths))/sum(populationMale*.5)*100, by = ageRange]
cfrData[, allTowns50InfectedFemaleCFRAdjusted := (sum(excessFemale) + 2*unique(sdDeaths))/sum(populationFemale*.5)*100, by = ageRange]

cfrData[, allTowns70InfectedFemaleCFR := sum(excessFemale)/sum(populationFemale*.7)*100, by = ageRange]
cfrData[, allTowns70InfectedMaleCFR := sum(excessMale)/sum(populationMale*.7)*100, by = ageRange]

cfrData[, allTowns90InfectedMaleCFR := sum(excessMale)/sum(populationMale*.9)*100, by = ageRange]
cfrData[, allTowns90InfectedFemaleCFR := sum(excessFemale)/sum(populationFemale*.9)*100, by = ageRange]

cfrData[, allTowns90InfectedMaleCFRAdjusted := max((sum(excessMale) - 2*unique(sdDeaths))/sum(populationMale*.9)*100, 0), by = ageRange] # Make sure not below zero!
cfrData[, allTowns90InfectedFemaleCFRAdjusted := max((sum(excessFemale) - 2*unique(sdDeaths))/sum(populationFemale*.9)*100, 0), by = ageRange]

dataPlotMale <- unique(cfrData[, c("ageRange", "allTowns70InfectedMaleCFR",  
                                               "allTowns50InfectedMaleCFR", 
                                               "allTowns90InfectedMaleCFR",
                                               "allTowns50InfectedMaleCFRAdjusted",
                                               "allTowns90InfectedMaleCFRAdjusted")])
dataPlotMale[, Gender := "Male"]

dataPlotFemale <- unique(cfrData[, c("ageRange", "allTowns70InfectedFemaleCFR",  
                                   "allTowns50InfectedFemaleCFR", 
                                   "allTowns90InfectedFemaleCFR",
                                   "allTowns50InfectedFemaleCFRAdjusted",
                                   "allTowns90InfectedFemaleCFRAdjusted")])
dataPlotFemale[, Gender := "Female"]

names(dataPlotMale) <- c("Age Range", "Infection Fatality Rate (%)", "Upper", "Lower", "UpperAdjusted", "LowerAdjusted", "Gender")
names(dataPlotFemale) <- c("Age Range", "Infection Fatality Rate (%)", "Upper", "Lower", "UpperAdjusted", "LowerAdjusted", "Gender")

dataPlot <- rbind(dataPlotMale, dataPlotFemale)

ggplot(dataPlot, aes(x=`Age Range`, y= `Infection Fatality Rate (%)`, fill = Gender)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2,
              position=position_dodge(.9)) +
  theme_bw(base_size = 14) + 
  scale_y_sqrt(breaks = c(0.05, 0.1, .5, 1, 2, 5, 10, 20))

ggplot(dataPlot, aes(x=`Age Range`, y= `Infection Fatality Rate (%)`, fill = Gender)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=LowerAdjusted, ymax=UpperAdjusted), width=.2,
                position=position_dodge(.9)) +
  theme_bw(base_size = 14) + 
  scale_y_sqrt(breaks = c(0.05, 0.1, .5, 1, 2, 5, 10, 20))

# Do without dividing by gender
cfrData <- unique(cfrData)

cfrData[, allTowns30InfectedCFR := max(0, (sum(excessMale)+sum(excessFemale))/(sum(populationMale*.3) + sum(populationFemale*.3))*100), by = ageRange]
cfrData[, allTowns30InfectedCFRAdjusted := max(0, (sum(excessMale)+sum(excessFemale) + 2*unique(sdDeaths))/(sum(populationMale*.3) + sum(populationFemale*.3))*100), by = ageRange]

cfrData[, allTowns60InfectedCFR := max(0, (sum(excessMale)+sum(excessFemale))/(sum(populationMale*.6) + sum(populationFemale*.6))*100), by = ageRange]

cfrData[, allTowns80InfectedCFR := max(0, (sum(excessMale)+sum(excessFemale))/(sum(populationMale*.8) + sum(populationFemale*.8))*100), by = ageRange]
cfrData[, allTowns80InfectedCFRAdjusted := max(0, (sum(excessMale)+sum(excessFemale) - 2*unique(sdDeaths))/(sum(populationMale*.8) + sum(populationFemale*.8))*100), by = ageRange]

dataPlotOverall <- unique(cfrData[, c("ageRange", "allTowns60InfectedCFR",
                                      "allTowns30InfectedCFR",
                                      "allTowns80InfectedCFR",
                                      "allTowns30InfectedCFRAdjusted",
                                      "allTowns80InfectedCFRAdjusted")])

names(dataPlotOverall) <- c("Age Range", "Infection Fatality Rate (%)", "Upper", "Lower", "UpperAdjusted", "LowerAdjusted")
dataPlotOverall[, `Infection Fatality Rate (%)` := round(`Infection Fatality Rate (%)`,2)]

ggplot(dataPlotOverall, aes(x=`Age Range`, y= `Infection Fatality Rate (%)`)) + 
  geom_text(aes(label=`Infection Fatality Rate (%)`), vjust= -.5, hjust = 1.5, size = 4) +
  geom_bar(stat="identity", color="red", fill = "red",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=LowerAdjusted, ymax=UpperAdjusted), width=.2, size = 1,
                position=position_dodge(.9)) +
  theme_bw(base_size = 16) + 
  scale_y_sqrt(breaks = c(0.01, 0.05, 0.1, .5, 1, 2, 5, 10), limits = c(0, 13))

# Plot of overall case fatality rate by percentage of population infected
functionOverallRate <- function(x) cfrData[, (sum(excessMale)+sum(excessFemale))/(x*(sum(populationMale) + sum(populationFemale)))*100, ]
functionOverallRateAge <- function(x) cfrData[, (sum(excessMale)+sum(excessFemale))/(x*(sum(populationMale) + sum(populationFemale)))*100, by = ageRange]

pointsFractionInfected <- seq(0.15, .8, 0.01)
overallIFR <- lapply(FUN = functionOverallRate, pointsFractionInfected)
overallIFRAge <- lapply(FUN = functionOverallRateAge, pointsFractionInfected)

plotIFR <- as.data.table(cbind(unlist(overallIFR), pointsFractionInfected))
names(plotIFR) <- c("Infection Fatality Rate (%)", "Population Fraction Infected")

ggplot(plotIFR, aes(`Population Fraction Infected`, `Infection Fatality Rate (%)`)) + 
  geom_line(size = 2) +
  theme_bw(base_size = 16) + 
  scale_y_continuous(breaks = c(0, .5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5), limits = c(-.25, 4)) +
  geom_hline(yintercept = 0)

for (j in 1:length(pointsFractionInfected)){
  overallIFRAge[[j]]$fractionInfected <- pointsFractionInfected[[j]]  
}

plotIFRAge <- do.call("rbind", overallIFRAge) 
names(plotIFRAge) <-  c("Age Range", "Infection Fatality Rate (%)", "Population Fraction Infected")

ggplot(plotIFRAge, aes(`Population Fraction Infected`, `Infection Fatality Rate (%)`, color = `Age Range`)) + 
  geom_line(size = 2) +
  theme_bw(base_size = 16) + 
  scale_y_sqrt(breaks = c(0.01, 0.05, 0.1, .5, 1, 2, 5, 10, 15, 20), limits = c(0, 20)) + 
  geom_hline(yintercept = 0)

#overall 
cfrData[, (sum(excessMale) + sum(excessFemale))/(sum(.6*populationMale) + sum(.6*populationFemale))*100]
cfrData[, (sum(excessMale) + sum(excessFemale) - 2*11.17)/(sum(.8*populationMale) + sum(.8*populationFemale))*100]
cfrData[, (sum(excessMale) + sum(excessFemale) + 2*11.17)/(sum(.30*populationMale) + sum(.30*populationFemale))*100]

# Compare age distribution
ggplot(cfrData[NOME_COMUNE == "Castiglione d'Adda", sum(populationMale) + sum(populationFemale), by = ageRange], 
       aes(ageRange, V1/(cfrData[NOME_COMUNE == "Castiglione d'Adda", sum(populationMale) + sum(populationFemale),]))) + geom_bar(stat = "Identity")
ggplot(cfrData[, sum(populationMale) + sum(populationFemale), by = ageRange], aes(ageRange, V1/(cfrData[, sum(populationMale) + sum(populationFemale),]))) + geom_bar(stat = "Identity")

sum(tail(cfrData[NOME_COMUNE == "Castiglione d'Adda", sum(populationMale) + sum(populationFemale), by = ageRange],2)$V1)/cfrData[NOME_COMUNE == "Castiglione d'Adda", sum(populationMale) + sum(populationFemale),]

sum(tail(cfrData[, sum(populationMale) + sum(populationFemale), by = ageRange],2)$V1)/cfrData[, sum(populationMale) + sum(populationFemale),]








