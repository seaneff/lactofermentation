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

## salinity
ferment$salinity <- ferment$salt_grams/ferment$water_grams
ferment$salinity[which(ferment$salinity == Inf)] <- NA

## one row per ferment per day
ph_time <- sqldf("SELECT f.ferment_id,
f.ferment_name,
f.salinity as salinity,
ph.day,
ph.appx_ph
FROM ferment AS f
JOIN ferment_ph AS ph
  ON f.ferment_id = ph.ferment_id")

#####################################################################
## generate figure: ingredients #####################################
#####################################################################

ingredient_counts <- sort(table(ferment_ingredient$ingredient))

pdf("results/ingredients.pdf", height = 5, width = 6)
par(mar = mar_default + c(0, 6, 0, 0))
barplot(ingredient_counts,
        horiz = TRUE,
        las = 1,
        cex.names = 0.8,
        col = "palegreen3",
        xlim = c(0, max(ingredient_counts) + 2),
        xaxt = "n",
        xlab = "Number of Ferments with Ingredient",
        main = "Most Frequently Used Ingredients")
box()
## only integers on x-axis
axis(1, at = 0:(max(ingredient_counts) + 2))
dev.off()

#####################################################################
## generate figure: change in pH over time ##########################
#####################################################################

pdf("results/pH_over_time.pdf", height = 4, width = 6)
ggplot(ph_time, aes(x = day, y = appx_ph, group = ferment_name, col = ferment_name)) +
  geom_line() +
  xlab("Day") +
  ylab("Approximate pH") +
  ggtitle("Change in pH over Time:\nHome Lacto-fermentation Experiments") +
  scale_colour_discrete(name = "Ferment Name") +
  scale_x_continuous(breaks = pretty_breaks()) ## only label days as integers, since that's the unit of measurement
dev.off()

#####################################################################
## generate figure: change in pH over time by salinity level ########
#####################################################################

pdf("results/pH_over_time_by_salinity.pdf", height = 4, width = 6)
ggplot(ph_time[-which(is.na(ph_time$salinity)),], 
       aes(x = day, y = appx_ph, group = ferment_name, col = salinity)) +
  geom_line() +
  xlab("Day") +
  ylab("Approximate pH") +
  ggtitle("Change in pH over Time:\nHome Lacto-fermentation Experiments") +
  scale_colour_gradient(name = "Salinity", 
                        low = "royalblue", high = "springgreen",
                        limits = c(min(ph_time$salinity),
                                   max(ph_time$salinity)), 
                        labels = percent) +
  scale_x_continuous(breaks = pretty_breaks()) ## only label days as integers, since that's the unit of measurement
dev.off()


