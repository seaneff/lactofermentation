#####################################################################
## set working directory and load libraries #########################
#####################################################################

setwd("~/Documents/workspace/lactofermentation")
library(ggplot2)
library(scales)
library(sqldf)
library(lme4)
library(sjPlot) ## trying out a new package

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
ferment$salinity[which(complete.cases(ferment$mash_salinity_pct))] <- ferment$mash_salinity_pct[which(complete.cases(ferment$mash_salinity_pct))]

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
## plot full range
ggplot(ph_time, aes(x = day, y = appx_ph, group = ferment_name, col = ferment_name)) +
  geom_line(alpha = 0.8) +
  xlab("Day") +
  ylab("Approximate pH") +
  ggtitle("Change in pH over Time:\nHome Lacto-fermentation Experiments") +
  scale_colour_discrete(name = "Ferment Name") +
  scale_x_continuous(breaks = pretty_breaks()) ## only label days as integers, since that's the unit of measurement

## plot first 15 days
ggplot(ph_time, aes(x = day, y = appx_ph, group = ferment_name, col = ferment_name)) +
  geom_line(alpha = 0.8) +
  xlab("Day") +
  ylab("Approximate pH") +
  ggtitle("Change in pH over Time:\nHome Lacto-fermentation Experiments") +
  scale_colour_discrete(name = "Ferment Name") +
  scale_x_continuous(breaks = pretty_breaks(), ## only label days as integers, since that's the unit of measurement
                     limits = c(0, 15)) 
dev.off()

#####################################################################
## generate figure: change in pH over time by salinity level ########
#####################################################################

pdf("results/pH_over_time_by_salinity.pdf", height = 4, width = 6)
## plot full range
ggplot(ph_time[-which(is.na(ph_time$salinity)),], 
       aes(x = day, y = appx_ph, group = ferment_name, col = salinity)) +
  geom_line(alpha = 0.8) +
  xlab("Day") +
  ylab("Approximate pH") +
  ggtitle("Change in pH over Time:\nHome Lacto-fermentation Experiments") +
  scale_colour_gradient(name = "Salinity", 
                        low = "royalblue", high = "springgreen",
                        limits = c(min(ph_time$salinity),
                                   max(ph_time$salinity)), 
                        labels = percent) +
  scale_x_continuous(breaks = pretty_breaks()) ## only label days as integers, since that's the unit of measurement

## plot first 15 days
ggplot(ph_time[-which(is.na(ph_time$salinity)),], 
       aes(x = day, y = appx_ph, group = ferment_name, col = salinity)) +
  geom_line(alpha = 0.8) +
  xlab("Day") +
  ylab("Approximate pH") +
  ggtitle("Change in pH over Time:\nHome Lacto-fermentation Experiments") +
  scale_colour_gradient(name = "Salinity", 
                        low = "royalblue", high = "springgreen",
                        limits = c(min(ph_time$salinity),
                                   max(ph_time$salinity)), 
                        labels = percent) +
  scale_x_continuous(breaks = pretty_breaks(), ## only label days as integers, since that's the unit of measurement
                     limits = c(0, 15))
dev.off()

#####################################################################
## framework for time-series modeling ###############################
#####################################################################

(ts_model <- lmer(appx_ph ~ 1 + day + I(day^2) + 
                ( 1 + day + I(day^2) | ferment_name),
      data = ph_time,
      REML = FALSE))

summary(ts_model)
               
## look at random slope info
ranef(ts_model)[["ferment_name"]]

## plot random effects effects
pdf("results/initial_full_model_coef.pdf", height = 5, width = 10)
sjp.lmer(ts_model, type = "re", sort.est = "day")
dev.off()

ph_time$fit <- predict(ts_model)

pdf("results/model_fit.pdf", height = 5, width = 10)
ggplot(ph_time, aes(day, appx_ph, group = ferment_name, col = ferment_name)) + 
  geom_point(alpha = 0.3) +
  geom_line(aes(y = fit, col = ferment_name), size = 0.8) +
  scale_colour_discrete(name = "Ferment Name") + ## edit axis title
  ylab("pH") + 
  scale_x_continuous(labels = function (x) floor(x)) + 
  facet_wrap(~ ferment_name, scales = "free_x")
  geom_line(aes(y = fit, col = ferment_name), size = 0.8) +
  scale_colour_discrete(name = "Ferment Name") + ## edit axis title
  facet_wrap(~ ferment_name)
dev.off()
