library(readstata13)
dataWorld <- as.data.table(read.dta13("/Github/covid_IFR_Lombardy/data/worldometer.dta"))
dataIfr <- fread("/Github/covid_IFR_Lombardy/data/correlation_ifr_cfr.csv")
dataIfr <- dataIfr[country != "China", ]
dataIfr[, overallifrest := overallifrest/100]
dataIfr[, positiveRate := cases/tests]
ggplot(dataIfr[ ], aes(overallifrest,cfr)) + geom_point()


dataWorld <- merge(dataWorld, dataIfr, by = "country")
baseSize <- 20
ggplot(dataWorld, aes(overallifrest,cfr, label = location)) + geom_point() + 
  geom_smooth(method =  "lm",data =  dataWorld, color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .005) + 
  theme_bw(base_size = baseSize) +
  xlab("Estimated IFR") + 
  ylab("Reported CFR")

ggplot(dataWorld[tests_mln > 20000,], aes(overallifrest,cfr, label = location)) + geom_point() + 
  geom_smooth(method =  "lm",data =  dataWorld[tests_mln > 20000,], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .005) + 
  theme_bw(base_size = baseSize) +
  xlab("Estimated IFR") + 
  ylab("Reported CFR")

ggplot(dataWorld[cases_mln > 200,], aes(overallifrest,cfr, label = location)) + geom_point() + 
  geom_smooth(method =  "lm",data =  dataWorld[cases_mln > 200,], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .005) + 
  theme_bw(base_size = baseSize) +
  xlab("Estimated IFR") + 
  ylab("Reported CFR")

summary(lm(cfr~ overallifrest, dataWorld[tests_mln > 20000,]))
summary(lm(cfr~ overallifrest, dataWorld[tests_mln > 20000,]))

ggplot(dataWorld[tests_mln > 20000,], aes(overallifrest*100, cfr*100, label = location)) + geom_point() + 
  geom_smooth(method =  "lm",data =  dataWorld[tests_mln > 20000,], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .5) + 
  theme_bw(base_size = baseSize) +
  xlab("Estimated IFR (%)") + 
  ylab("Reported CFR (%)")

ggplot(dataWorld[tests_mln > 20000,], aes(positiveRate, cfr*100, label = location)) + geom_point() + 
  geom_smooth(method =  "lm",data =  dataWorld[tests_mln > 20000,], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .001) + 
  theme_bw(base_size = baseSize) +
  ylab("Estimated IFR (%)") + 
  xlab("Positive Rate")

ggplot(dataWorld, aes(log(overallifrest), log(cfr), label = location)) + geom_point() + 
  geom_smooth(method =  "lm",data =  dataWorld, color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .005) + 
  theme_bw(base_size = baseSize) +
  xlab("Estimated IFR") + 
  ylab("Reported CFR")

summ(lm(cfr~ overallifrest, dataWorld[tests_mln > 20000,]))
summ(lm(cfr~ overallifrest, dataWorld))

summary(lm(cfr ~ positiveRate, dataWorld[tests_mln > 20000,]))
summary(lm(cfr ~ overallifrest, dataWorld[tests_mln > 20000,]))
summary(lm(cfr ~ overallifrest + positiveRate, dataWorld[tests_mln > 20000,]))

summary(lm(log(cfr) ~ log(overallifrest), dataWorld[tests_mln > 20000,], weights = deaths.x))

summary(lm(cfr~ overallifrest + positiveRate, dataWorld))
summary(lm(log(cfr) ~ log(overallifrest) + positiveRate, dataWorld))
summary(lm(log(cfr) ~ log(overallifrest) + positiveRate, dataWorld, weights = deaths.x))
summary(lm(log(cfr) ~ log(overallifrest) + positiveRate, dataWorld[tests_mln > 20000,], weights = deaths.x))


summary(lm(log(cfr) ~ log(overallifrest), dataWorld))

summary(lm(cfr ~ overallifrest, dataWorld, weights = deaths.x))

summary(lm(cfr ~ overallifrest + positiveRate, dataWorld, weights = deaths.x))

summary(lm(log(cfr) ~ log(overallifrest), dataWorld, weights = deaths.x))
summary(lm(log(cfr) ~ log(overallifrest) + log(positiveRate), dataWorld, weights = deaths.x))



summary(lm(cfr ~ overallifrest, dataWorld))
summary(lm(cfr ~ positiveRate, dataWorld))

summ(lm(cfr ~ overallifrest, dataWorld[tests_mln > 20000,]))
summ(lm(log(cfr) ~  log(overallifrest) + positiveRate, dataWorld[country != "Singapore" & tests_mln > 20000, ]))
summ(lm(log(cfr) ~  log(overallifrest) + positiveRate, dataWorld[country != "Singapore" & tests_mln > 10000, ]))
summ(lm(log(cfr) ~  log(overallifrest) + positiveRate, dataWorld[country != "Singapore" & tests_mln > 20000, ]))
summ(lm(log(cfr) ~  log(overallifrest) + log(positiveRate), dataWorld[country != "Singapore" & tests_mln > 20000, ]))

dataWorld[country != "Singapore" & tests_mln > 20000, cfrRes := lm(log(cfr) ~  log(overallifrest), dataWorld[country != "Singapore" & tests_mln > 20000, ])$residuals]
dataWorld[country != "Singapore" & tests_mln > 20000, posRateRes:= lm(positiveRate ~  log(overallifrest), dataWorld[country != "Singapore" & tests_mln > 20000, ])$residuals]

summary(lm(cfrRes ~ posRateRes, dataWorld[tests_mln > 20000,]))
ggplot(dataWorld, aes(posRateRes, cfrRes, label = location)) + 
  geom_point() + 
  geom_smooth(method =  "lm",data =  dataWorld[tests_mln > 20000,], color = "gray", formula = 'y ~ x') + 
  geom_text(color= "Black", nudge_y = .001) + 
  theme_bw(base_size = baseSize) +
  ylab("Residual CFR (%)") + 
  xlab("Residual Positive Rate")

summary(lm(log(cfr) ~ log(overallifrest), dataWorld[tests_mln > 20000,]))
summary(lm(log(cfr) ~ log(overallifrest) + positiveRate, dataWorld[tests_mln > 20000,]))


ggplot(dataIfr[cases > 1000, ], aes(overallifrest,cfr)) + geom_point()
ggplot(dataIfr[cases > 1000, ], aes(overallifrest,cfr)) + geom_point()

dataIfr[ , ifrRes := lm(overallifrest ~ positiveRate, dataIfr)$residuals] 
dataIfr[ , cfrRes := lm(cfr ~ positiveRate, dataIfr)$residuals] 


ggplot(dataIfr, aes(ifrRes, cfrRes)) + geom_point()

ggplot(dataIfr, aes(log(overallifrest), log(cfr))) + geom_point()

summary(lm(cfrRes ~ ifrRes, dataIfr))


summary(lm(log(cfr) ~ log(overallifrest), dataIfr))
summary(lm(log(cfr) ~ log(overallifrest) + log(positiveRate), dataIfr))

summary(lm(cfr ~ overallifrest + positiveRate, dataIfr))

summary(lm(cfr ~ overallifrest, dataIfr[deaths > 1000, ]))
summary(lm(cfr ~ overallifrest, dataIfr[deaths > 100, ]))
summary(lm(cfr ~ overallifrest, dataIfr[cases > 1000, ]))
summary(lm(cfr ~ overallifrest, dataIfr[cases > 10000, ]))




