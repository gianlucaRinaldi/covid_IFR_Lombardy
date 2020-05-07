setwd("/Github/covid_IFR_Lombardy/")

require(data.table)
# Package to donwload OECD data
library(OECD)

# Load packages for estimation
library(rjags)
library(R2OpenBUGS)
library(coda)
library(MCMCvis)

# Visualization
require(ggplot2)
library(viridis)
library("ggsci")
library(RColorBrewer)
library(stargazer)
library("animation")
library(latex2exp)
