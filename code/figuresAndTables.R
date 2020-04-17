#########################
## Make figures 
#########################

baseSize <- 20

###############################################################
# Deaths by day in 2015-2019 and 2020
###############################################################

ggplot() + 
  geom_line(data = plotDeaths, aes(x = date, y = meanDailyDeahtsBeforeAll, colour = "2015-19 average deaths") , linetype = "dashed") + 
  geom_line(data = plotDeaths, aes(x = date, y = deaths2020All, colour = "2020 deaths")) + 
  scale_color_manual(values = c(
    '2015-19 average deaths' = 'blue',
    '2020 deaths' = 'black')) +
  theme_bw(base_size = baseSize) + 
  xlab("") + 
  ylab("") + 
  geom_hline(yintercept = 0) +  
  theme(legend.position = c(0.3, 0.8)) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = as.Date("2020-02-20"), color = "red") +
  theme(panel.grid.minor = element_blank())

ggsave(filename = "output/deathsByDate.pdf")
ggsave(filename = "../../Users/grinaldi/Dropbox/Apps/Overleaf/covid19 IFR/figures/deathsByDate.pdf")


###############################################################
# Bayesian estimates of infection fatality rates
###############################################################

IFRPlot <- rbindlist(lapply(postTown, function(x) as.data.table(x[,15:21])))
ageRanges <- unique(dataLikelihoodTown$ageRange)
names(IFRPlot) <- ageRanges
IFRPlot <- melt(IFRPlot)
IFRPlot[, value := value*100] # Change to percentage

ggplot(IFRPlot, aes(variable, value)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_sqrt(breaks = c(0,.01, .1, .2, .5, 1, 2, 5, 10), limits = c(0, 15)) +
  theme_bw(base_size = baseSize) +
  xlab("Age Range") + 
  ylab("Infection Fatality Rate (%)") +
  geom_hline(yintercept = 0) +
  theme(panel.grid.minor = element_blank())

ggsave(filename = "output/IFRbyAge.pdf")
ggsave(filename = "../../Users/grinaldi/Dropbox/Apps/Overleaf/covid19 IFR/figures/IFRbyAge.pdf")

###############################################################
# Fatality rates for different assumptions on infection rates
###############################################################

# Construct overall ifr 
ageRangeShare <- unique(demographics[, c("ageRange", "ageRangeShare")]) 
overallIFR <- merge(graphDataAll, ageRangeShare, by = "ageRange")
overallIFR <- overallIFR[, list(sum(`2.5%` * ageRangeShare), sum(`50%` * ageRangeShare), sum(`97.5%` * ageRangeShare)), by = prop]
overallIFR[, ageRange := "Overall", ]
names(overallIFR)[2:4] <- c("2.5%", "50%", "97.5%")
overallIFR <- overallIFR[, c("2.5%", "50%", "97.5%", "ageRange", "prop")]
graphDataAll <- rbind(graphDataAll, overallIFR)
graphDataAll[, `2.5%` := 100 * `2.5%`]  # change to percentage
graphDataAll[, `50%` := 100 * `50%`] 
graphDataAll[, `97.5%` := 100 * `97.5%`] 
graphDataAll[, prop := 100 * prop] 
# Custom palette
palCustom <- c( "#4575B4", "#91BFDB", "greenyellow", "khaki", "orange", rev(brewer.pal(n = 7, name = "RdYlBu"))[6:7] ,  "#000000")

ggplot(graphDataAll[prop > 10, ], aes(x = prop)) +
  geom_line(aes(y = `2.5%`, color = ageRange), linetype = 2) +
  geom_line(aes(y = `50%`, color = ageRange), size = 1) +
  geom_line(aes(y = `97.5%`, color = ageRange), linetype = 2) +
  scale_y_sqrt(breaks = c(0,.01, .1, .2, .5, 1, 2, 5, 10, 20), limits = c(0, 30)) +
  theme_bw(base_size = baseSize) +
  xlab("Proportion Infected (%)") + 
  ylab("Infection Fatality Rate (%)") +
  geom_hline(yintercept = 0) +
  theme(panel.grid.minor = element_blank()) + 
  geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`, fill = ageRange), alpha= 0.35)  +
  scale_fill_manual(values = palCustom, name = "Age Range") + 
  scale_color_manual(values = palCustom, name = "Age Range")
  
ggsave(filename = "output/IFRbyProp.pdf")
ggsave(filename = "../../Users/grinaldi/Dropbox/Apps/Overleaf/covid19 IFR/figures/IFRbyProp.pdf")

#############################################################
# Overall IFR from full model for various age groups
#############################################################

overallIFR <- merge(ageRangeShare, IFRPlot[, as.list(quantile(value, c(.025,.5,.975))), by = variable], by.x = "ageRange", by.y = "variable")
overallIFR <- overallIFR[, c(sum(ageRangeShare*`2.5%`), sum(ageRangeShare*`50%`) , sum(ageRangeShare*`97.5%`))]

under50IFR <- merge(ageRangeShare, IFRPlot[, as.list(quantile(value, c(.025,.5,.975))), by = variable], by.x = "ageRange", by.y = "variable")
under50IFR <- under50IFR[ageRange %in% c("0-20", "21-40", "41-50"), c(sum(ageRangeShare*`2.5%`), sum(ageRangeShare*`50%`) , sum(ageRangeShare*`97.5%`))/sum(ageRangeShare)]

at60IFR <- merge(ageRangeShare, IFRPlot[, as.list(quantile(value, c(.025,.5,.975))), by = variable], by.x = "ageRange", by.y = "variable")
at60IFR <- at60IFR[ageRange %in% c("51-60"), c(sum(ageRangeShare*`2.5%`), sum(ageRangeShare*`50%`) , sum(ageRangeShare*`97.5%`))/sum(ageRangeShare)]

under60IFR <- merge(ageRangeShare, IFRPlot[, as.list(quantile(value, c(.025,.5,.975))), by = variable], by.x = "ageRange", by.y = "variable")
under60IFR <- under60IFR[ageRange %in% c("0-20", "21-40", "41-50", "51-60"), c(sum(ageRangeShare*`2.5%`), sum(ageRangeShare*`50%`) , sum(ageRangeShare*`97.5%`))/sum(ageRangeShare)]

over60IFR <- merge(ageRangeShare, IFRPlot[, as.list(quantile(value, c(.025,.5,.975))), by = variable], by.x = "ageRange", by.y = "variable")
over60IFR <- over60IFR[! ageRange %in% c("0-20", "21-40", "41-50", "51-60"), c(sum(ageRangeShare*`2.5%`), sum(ageRangeShare*`50%`) , sum(ageRangeShare*`97.5%`))/sum(ageRangeShare)]

over80IFR <- merge(ageRangeShare, IFRPlot[, as.list(quantile(value, c(.025,.5,.975))), by = variable], by.x = "ageRange", by.y = "variable")
over80IFR <- over80IFR[ageRange %in% c("81+"), c(sum(ageRangeShare*`2.5%`), sum(ageRangeShare*`50%`) , sum(ageRangeShare*`97.5%`))/sum(ageRangeShare)]

graphDataAll[ageRange == "Overall" & prop == 1, ]

#########################
## Make Tables
#########################

##################################
# Table of demographics and deaths
##################################
tableDemsDeaths <- dataLikelihoodTown[, c(-1)] # Remove town name
tableDemsDeaths <-  tableDemsDeaths[,lapply(.SD, sum, na.rm=TRUE), by= ageRange]
overallDemsDeaths <- tableDemsDeaths[, c(-1)]
overallDemsDeaths <- overallDemsDeaths[,lapply(.SD, sum, na.rm=TRUE), ]
overallDemsDeaths[, ageRange := "Overall"]

tableDemsDeaths <- rbind(tableDemsDeaths, overallDemsDeaths)

stargazer(tableDemsDeaths, summary = FALSE, rownames = FALSE)




dataLikelihoodTown[, unique(Denominazione)]

