#################################################################
# Load and clean data
#################################################################

relevantTowns <- c("Codogno", "Castiglione d'Adda", "Casalpusterlengo", "Fombio", "Maleo", "Somaglia", "Bertonico", "Terranova dei Passerini", "Castelgerundo", "San Fiorano")

# Load demographic data
lodiDem <- fread("data/Lodi.csv")
relevantTownsDemData <- lodiDem[Denominazione %in% relevantTowns, ]

# Demographic Data
demsData <- fread(input = "data/Lodi_2015_2019.csv")
demsData <- demsData[Denominazione %in% unique(relevantTownsDeathsData$NOME_COMUNE), ]

# Display total population in the area and total population in towns with available deaths data
relevantTownsDemData[`Età` == "Totale", sum(`Totale Femmine`) + sum(`Totale Maschi`)]
relevantTownsDemData[`Età` == "Totale" & Denominazione %in% excludedTowns, sum(`Totale Femmine`) + sum(`Totale Maschi`)]

# This is only to reconstruct comune_giorno_relevant.csv from the original data (not uploaded to github for size limitations)
# deathsData <- fread("data/comune_giorno.csv") 
# relevantTownsDeathsData <- deathsData[NOME_COMUNE %in% relevantTowns, ]
# write.csv(relevantTownsDeathsData, file = "data/comune_giorno_relevant.csv")

# Load deaths data
relevantTownsDeathsData <- fread("data/comune_giorno_relevant.csv") 

# Define broader age categories
relevantTownsDeathsData[, ageRange := cut(CL_ETA, c(0, 4, 8, 10, 12, 14, 16, 22), labels = c("0-20", "21-40", "41-50", "51-60", "61-70","71-80", "81+"), include.lowest = T)]

# Remove towns for which 2020 deaths data is missing
relevantTownsDeathsData <- relevantTownsDeathsData[! (TOTALE_20 == 9999), ]

# We have deaths data for 7 towns
relevantTownsDemData <- relevantTownsDemData[Denominazione %in% relevantTownsDeathsData[, unique(NOME_COMUNE)], ] 
excludedTowns <- c("Bertonico", "Terranova dei Passerini", "Castelgerundo") # Towns without deaths data for 2020 as of April 17 2020

# Compute excess deaths by age and by sex
relevantTownsDeathsData[, dailyDeaths2020Male := sum(MASCHI_20), by = c("NOME_COMUNE", "ageRange", "GE")]
relevantTownsDeathsData[, dailyDeaths2020Female := sum(FEMMINE_20), by = c("NOME_COMUNE", "ageRange", "GE")]

relevantTownsDeathsData[, meanDailyDeathsPreviousYearsMale := (sum(MASCHI_15) + sum(MASCHI_16) + sum(MASCHI_17) + sum(MASCHI_18) + sum(MASCHI_19))/5, by = c("NOME_COMUNE", "ageRange", "GE")]
relevantTownsDeathsData[, meanDailyDeathsPreviousYearsFemale := (sum(FEMMINE_15) + sum(FEMMINE_16) + sum(FEMMINE_17) + sum(FEMMINE_18) + sum(FEMMINE_19))/5, by = c("NOME_COMUNE", "ageRange", "GE")]

relevantTownsDeathsData[, excess2020DeathsMale := unique(dailyDeaths2020Male - meanDailyDeathsPreviousYearsMale), by = c("NOME_COMUNE", "ageRange", "GE")]
relevantTownsDeathsData[, excess2020DeathsFemale := unique(dailyDeaths2020Female - meanDailyDeathsPreviousYearsFemale), by = c("NOME_COMUNE", "ageRange", "GE")]

# Remove dates after april 4 because no 2020 data
relevantTownsDeathsData <- relevantTownsDeathsData[GE <= 404,]
relevantTownsDeathsData[, covidAffectedPeriod := (GE %in% 221:404)]

# Look at total deaths by age cathegories to have an idea of variance
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths15 := sum(MASCHI_15 + FEMMINE_15), by = "ageRange"]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths16 := sum(MASCHI_16 + FEMMINE_16), by = "ageRange"]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths17 := sum(MASCHI_17 + FEMMINE_17), by = "ageRange"]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths18 := sum(MASCHI_18 + FEMMINE_18), by = "ageRange"]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths19 := sum(MASCHI_19 + FEMMINE_19), by = "ageRange"]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeaths20 := sum(MASCHI_20 + FEMMINE_20), by = "ageRange"]

relevantTownsDeathsData[covidAffectedPeriod == T, totDeathsTown15 := sum(MASCHI_15 + FEMMINE_15), by = c("ageRange", "NOME_COMUNE")]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeathsTown16 := sum(MASCHI_16 + FEMMINE_16), by = c("ageRange", "NOME_COMUNE")]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeathsTown17 := sum(MASCHI_17 + FEMMINE_17), by = c("ageRange", "NOME_COMUNE")]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeathsTown18 := sum(MASCHI_18 + FEMMINE_18), by = c("ageRange", "NOME_COMUNE")]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeathsTown19 := sum(MASCHI_19 + FEMMINE_19), by = c("ageRange", "NOME_COMUNE")]
relevantTownsDeathsData[covidAffectedPeriod == T, totDeathsTown20 := sum(MASCHI_20 + FEMMINE_20), by = c("ageRange", "NOME_COMUNE")]

allDeathsByYearTown <- unique(relevantTownsDeathsData[, c("NOME_COMUNE", "ageRange", "totDeathsTown15", "totDeathsTown16", "totDeathsTown17", "totDeathsTown18", "totDeathsTown19", "totDeathsTown20")])
allDeathsByYearTown <- allDeathsByYearTown[! is.na(totDeathsTown15)] 
allDeathsByYear <- unique(relevantTownsDeathsData[(! is.na(totDeaths15)) & (! is.na(totDeaths16)) & (! is.na(totDeaths17)) & (! is.na(totDeaths18)) & (! is.na(totDeaths19)) & (! is.na(totDeaths20)), c("ageRange", "totDeaths15","totDeaths16", "totDeaths17", "totDeaths18", "totDeaths19", "totDeaths20")])

# standard deviation total deaths in years before 2020
relevantTownsDeathsData[covidAffectedPeriod == T,  sd(c(sum(MASCHI_15 + FEMMINE_15),sum(MASCHI_16 + FEMMINE_16), sum(MASCHI_17 + FEMMINE_17), sum(MASCHI_18 + FEMMINE_18), sum(MASCHI_19 + FEMMINE_19))),]
allDeathsByYear <- melt(allDeathsByYear)
deathsByYearOverall <- allDeathsByYear[, sum(value), by = variable]
deathsByYearOverall[, variable := substr(variable, 10,11)]
ggplot(deathsByYearOverall, aes(variable, V1)) + geom_bar(stat = "identity") + xlab("") + ylab("Total deaths between Feb 21 and March 28") + theme_bw(base_size = 12)
sd(allDeathsByYear[! variable == "totDeaths20", sum(value), by = variable]$V1)
sdByAge <- allDeathsByYear[! variable == "totDeaths20", sd(value), by = c("ageRange")]

relevantTownsDeathsData[, meanDailyDeahtsBeforeAll := sum(meanDailyDeathsPreviousYearsMale) + sum(meanDailyDeathsPreviousYearsFemale), by = GE]
relevantTownsDeathsData[, deaths2020All := sum(dailyDeaths2020Male) + sum(dailyDeaths2020Female), by = GE]

plotDeaths <- unique(relevantTownsDeathsData[, c("GE", "meanDailyDeahtsBeforeAll", "deaths2020All")])
plotDeaths[, month := substr(as.character(GE), 1,1)]
plotDeaths[, day := substr(as.character(GE), 2,3)]
plotDeaths[, date := as.Date(paste0(month, "/", day, "/2020"), format = "%m/%d/%Y")]

# Look at demograpgics
relevantTownsDemData[, age2020 := as.numeric(`Età`) + 1] # data are from 2018
relevantTownsDemData[, ageRange := cut(age2020, c(0, 20, 40, 50, 60, 70, 80,  200), labels = c("0-20", "21-40", "41-50", "51-60", "61-70","71-80", "81+"), include.lowest = T)]
relevantTownsDemData[, populationMale := sum(`Totale Maschi`), by = c("Denominazione", "ageRange")]
relevantTownsDemData[, populationFemale := sum(`Totale Femmine`), by = c("Denominazione", "ageRange")]

# Average age
relevantTownsDemData[! is.na(age2020), sum(age2020*(`Totale Femmine` + `Totale Maschi`))/sum((`Totale Femmine` + `Totale Maschi`))]
relevantTownsDemData[! is.na(age2020), sum(`Totale Femmine`)]
relevantTownsDemData[! is.na(age2020), sum(`Totale Femmine`)]
relevantTownsDemData[! is.na(age2020), sum(`Totale Maschi`)]
