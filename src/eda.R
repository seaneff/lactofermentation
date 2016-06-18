#####################################################################
## set working directory and load libraries #########################
#####################################################################

setwd("~/Documents/workspace/lactofermentation")
library(ggplot2)
library(scales)
library(sqldf)

#####################################################################
## set plotting parameters ##########################################
#####################################################################

## save default plotting margins
mar_default <- par()$mar

##############################################
## load data #################################
##############################################

ferment <- read.csv("data/ferment.csv")
ferment_ingredient <- read.csv("data/ferment_ingredient.csv")
ferment_ph <- read.csv("data/ferment_ph.csv")

####################################
## format data #####################
####################################

## one row per ferment per day

ph_time <- sqldf("SELECT f.ferment_id,
f.ferment_name,
day,
appx_ph
FROM ferment AS f
JOIN ferment_ph AS ph
  ON f.ferment_id = ph.ferment_id")

#####################################################################
## generate figure: cange in pH over time #########################
#####################################################################

pdf("results/pH_over_time.pdf", height = 4, width = 6)
ggplot(ph_time, aes(x = day, y = appx_ph, group = ferment_name, col = ferment_name)) +
  geom_line() +
  xlab("Day") +
  ylab("Approximate pH") +
  ggtitle("Change in pH over Time:\nHome Lacto-fermentation Experiments") +
  scale_colour_discrete(name = "Ferment Name") +
  scale_x_continuous(breaks= pretty_breaks()) ## only label days as integers, since that's the unit of measurement
dev.off()

