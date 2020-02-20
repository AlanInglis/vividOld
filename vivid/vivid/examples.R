library(tidyverse)
library(ggnewscale)
library(mlr)
library(iml)
library(plotly)
library(reshape2)
library(colorspace)
library(igraph)
library(ggplot2)
library(GGally)
#library(network)
#library(sna)
library(RColorBrewer)
library(grDevices)
library(intergraph)

library(vivid)

# Get data ----------------------------------------------------------------
## Air quality data (used for regression)

aq <- data.frame(airquality)
aq <- na.omit(aq)

# mlr set up --------------------------------------------------------------

aqRgrTask  <- makeRegrTask(data = aq, target = "Ozone")
aqRegrLrn <- makeLearner("regr.randomForest")
aqMod <- train(aqRegrLrn, aqRgrTask)




# Network style plot  -----------------------------------------------------
set.seed(1701)
importanceNet(aqRgrTask, aqMod, method = "randomForestSRC_importance", thresholdValue = 0,
              label = T)
# With Thresholding
importanceNet(aqRgrTask, aqMod, method = "randomForestSRC_importance", thresholdValue = 0.09, cluster = F)


# Heatmap style plot ------------------------------------------------------
intHeatmap(aqRgrTask, aqMod, method = "randomForest_importance", interact = F)


# pdp plot ----------------------------------------------------------------
pdpPairs(aqRgrTask , aqMod)
