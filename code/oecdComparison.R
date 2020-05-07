## Estimate IFR for other nations using OECD Data

oecdDem <- get_dataset("HISTPOP", filter = NULL, start_time = NULL,
            end_time = NULL, pre_formatted = FALSE)
oecdDemCln <- as.data.table(oecdDem)
oecdDemCln <- oecdDemCln[obsTime == 2018, ]
oecdDemCln <- oecdDemCln[SEX == "T", ]
# One year difference compared to my data unfortunately (20-24 + 25-29 + 30-34 + 35-39 instead of 21-40 in ISTAT data)
oecdDemCln[AGE %in% c("0_4", "05_9", "10_14", "15_19"), ageRange := "0-20"]
oecdDemCln[AGE %in% c("20_24", "25_29", "30_34", "35_39"), ageRange := "21-40"]
oecdDemCln[AGE %in% c("40_44", "45_49"), ageRange := "41-50"]
oecdDemCln[AGE %in% c("50_54", "55_59"), ageRange := "51-60"]
oecdDemCln[AGE %in% c("60_64", "65_69"), ageRange := "61-70"]
oecdDemCln[AGE %in% c("70_74", "75_79"), ageRange := "71-80"]
oecdDemCln[AGE %in% c("80_84", "85_OVER"), ageRange := "81+"]

popByAgeRange <- oecdDemCln[! is.na(ageRange), sum(obsValue), by = c("LOCATION", "ageRange")]
popByAgeRange <- merge(popByAgeRange, oecdDemCln[AGE %in% c("TOTAL"), c("LOCATION", "obsValue")], by = "LOCATION")
popByAgeRange[, share := V1/obsValue]

# Add IFR estimates
ifrEstimates <- table2Data[16:22, 3:5]
ifrEstimates <- cbind(ifrEstimates, c("0-20", "21-40", "41-50", "51-60", "61-70", "71-80", "81+"))
names(ifrEstimates) <- c("lower","median","upper","ageRange")

popByAgeRange <- merge(popByAgeRange, ifrEstimates, by = "ageRange")
popByAgeRange[, overallIFRest := sum(median*share), by = LOCATION]
popByAgeRange[, overallIFRLower := sum(lower*share), by = LOCATION]
popByAgeRange[, overallIFRUpper := sum(upper*share), by = LOCATION]

popByAgeRange <- unique(popByAgeRange[, c("LOCATION", "overallIFRest", "overallIFRLower", "overallIFRUpper")])

ggplot(data=popByAgeRange, aes(x= reorder(LOCATION, - overallIFRest), y = overallIFRest)) +
  geom_bar(stat="identity", color = "black", fill = "red") + 
  geom_errorbar(aes(ymin=overallIFRLower, ymax=overallIFRUpper), width=.3,
                position=position_dodge(.9))+ 
  theme_bw(base_size = baseSize) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("Estimated overall IFR")
popByAgeRange[LOCATION == "USA", ]
popByAgeRange[LOCATION == "ITA", ]
popByAgeRange[LOCATION == "ITA", ]
unique(demographicData[, c("ageRange", "ageRangeShare")])

# Check baseline mortality in this region against rest of italy
italyMortality <- fread(file = "data/baselineMortalityItaly.csv")
italyMortality[, unique(TIPO_DATO15)]
italyMortalityCln <- italyMortality[TIPO_DATO15 %in% c("SURVIVORS", "DEATHS"), ]
italyMortalityCln <- italyMortalityCln[Gender == "total", ]
italyMortalityCln <- italyMortalityCln[Territory == "Italy", ]
italyMortalityCln[ETA1 %in% c("Y_UN4", "Y5-9", "Y10-14", "Y15-19"), ageRange := "0-20"]
italyMortalityCln[ETA1 %in% c("Y20-24", "Y25-29", "Y30-34", "Y35-39"), ageRange := "21-40"]
italyMortalityCln[ETA1 %in% c("Y40-44", "Y45-49"), ageRange := "41-50"]
italyMortalityCln[ETA1 %in% c("Y50-54", "Y55-59"), ageRange := "51-60"]
italyMortalityCln[ETA1 %in% c("Y60-64", "Y65-69"), ageRange := "61-70"]
italyMortalityCln[ETA1 %in% c("Y70-74", "Y75-79"), ageRange := "71-80"]
italyMortalityCln[ETA1 %in% c("Y80-84", "Y85-89", "Y90-94", "Y95-99", "Y100-104", "Y105-109", "Y110-114", "Y115-119"), ageRange := "81+"]  
deathProbItaly <- dcast(italyMortalityCln[, sum(Value), by = c("TIPO_DATO15", "ageRange")], ageRange ~ TIPO_DATO15)
deathProbItaly[, deathProb := DEATHS/(DEATHS + SURVIVORS)]
deathProbItaly <- cbind(deathProbItaly, table2Data[1:7, 5]*(52/6))

deathProbItaly <- deathProbItaly[, c(1,4,5)]
names(deathProbItaly)[3] <- "deathEstimated"

