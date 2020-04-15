#########################
## Make figures 
#########################

###############################################################
# Deaths by day in 2015-2019 and 2020
###############################################################

ggplot() + 
  geom_line(data = plotDeaths, aes(x = date, y = meanDailyDeahtsBeforeAll, colour = "2015-19 average deaths") , linetype = "dashed") + 
  geom_line(data = plotDeaths, aes(x = date, y = deaths2020All, colour = "2020 deaths")) + 
  scale_color_manual(values = c(
    '2015-19 average deaths' = 'blue',
    '2020 deaths' = 'black')) +
  theme_bw(base_size = 16) + 
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

ggplot(IFRPlot, aes(variable, value)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_sqrt(breaks = c(0,.001, .01, .02, .05, .1, .2, .5, .10), limits = c(0, .25)) +
  theme_bw(base_size = 16) +
  xlab("Age Range") + 
  ylab("Infection Fatality Rate") +
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

# Custom palette
palCustom <- c( "#4575B4", "#91BFDB", "greenyellow", "khaki", "orange", rev(brewer.pal(n = 7, name = "RdYlBu"))[6:7] ,  "#000000")

ggplot(graphDataAll[prop > .1, ], aes(x = prop)) +
  geom_line(aes(y = `2.5%`, color = ageRange), linetype = 2) +
  geom_line(aes(y = `50%`, color = ageRange), size = 1) +
  geom_line(aes(y = `97.5%`, color = ageRange), linetype = 2) +
  scale_y_sqrt(breaks = c(0,.001, .01, .02, .05, .1, .2, .5, .10), limits = c(0, .3)) +
  theme_bw(base_size = 16) +
  xlab("Proportion Infected") + 
  ylab("Infection Fatality Rate") +
  geom_hline(yintercept = 0) +
  theme(panel.grid.minor = element_blank()) + 
  geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`, fill = ageRange), alpha= 0.35)  +
  scale_fill_manual(values = palCustom, name = "Age Range") + 
  scale_color_manual(values = palCustom, name = "Age Range")
  
ggsave(filename = "output/IFRbyProp.pdf")
ggsave(filename = "../../Users/grinaldi/Dropbox/Apps/Overleaf/covid19 IFR/figures/IFRbyProp.pdf")


