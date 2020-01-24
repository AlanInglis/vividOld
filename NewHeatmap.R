rm(list = ls())

library(tidyverse)
library(ggnewscale)
library(mlr)
library(iml)
library(plotly)
library(reshape2)
library(randomForest)
library(randomForestSRC)
library(ranger)

set.seed(1701)

# Get data ----------------------------------------------------------------

## Air quality data (regression)

aq <- data.frame(airquality)
aq <- na.omit(aq)

## Iris data for classification

ir <- data.frame(iris)
ir <- na.omit(ir)


## Friedman Data 
# Set Values
n <- 100
p <- 10
e <- rnorm(n)

# Create matrix of values
xValues <- matrix(runif(n*p, 0, 1), nrow=n)               # Create matrix 
colnames(xValues)<- paste0("x", 1:p)                      # Name columns
FRdf <- data.frame(xValues)                               # Create dataframe 

# Equation:
#y = 10sin(πx1x2) + 20(x3−0.5)^2 + 10x4 + 5x5 + ε

y = 10*sin(pi*FRdf$x1*FRdf$x2) + 20 * (FRdf$x3-0.5)^2 + 10 * FRdf$x4 + 5 * FRdf$x5 + e

# Adding y to df
FRdf$y <- y


# mlr set up --------------------------------------------------------------

## air quality:
aqRgrTask  <- makeRegrTask(data = aq, target = "Ozone")
aq.regr.lrn <- makeLearner("regr.randomForest")
aqMod <- train(aq.regr.lrn, aqRgrTask)

## iris:
irClasTask  <- makeClassifTask(data = ir, target = "Species")
ir.clas.lrn <- makeLearner("classif.randomForest", predict.type = 'prob')
irMod <- train(ir.clas.lrn, irClasTask)

## Friedman:
frRgrTask  <- makeRegrTask(data = FRdf, target = "y")
FR.regr.lrn <- makeLearner("regr.randomForest")
frMod <- train(FR.regr.lrn, frRgrTask)


## Heatmap Plotting Function -------------------------------------------------------

intHeatmap <- function(task, model, method = "randomForest",
                       interact = FALSE,...){
  # get data:
  data <- getTaskData(task)
  # Get Importance Measures -------------------------------------------------
  
  norm.task <- normalizeFeatures(task, method = "standardize")
  
  im_feat <- generateFilterValuesData(norm.task, method = method)
  Y_Imp <- im_feat$data$value



  mod  <- Predictor$new(model, data)
  res  <- NULL
  ovars <- getTaskFeatureNames(task)
  for (i in 1:length(ovars))
    res <- rbind(res, Interaction$new(mod, feature=ovars[i])$results)
  
  res[[".feature"]] <- reorder(res[[".feature"]], res[[".interaction"]])
  
  vars2 <- t(simplify2array(strsplit(as.character(res[[".feature"]]),":"))) # split/get feature names 
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[[".interaction"]]                              # set values   
  dinteraction <- (dinteraction+t(dinteraction))/2                          # avg over values to make symmetrical
  diag(dinteraction) <- Y_Imp
  
  nam <- getTaskFeatureNames(task)
# Now create a plot -------------------------------------------------------

  var_int2 = dinteraction %>% as_tibble %>% 
    mutate(var_num1 = 1:length(nam)) %>% 
    pivot_longer(cols = 1:length(nam),
                 values_to = 'Interaction\nStrength') %>% 
    mutate(var_num2 = rep(1:length(nam), length(nam)),
           alpha_imp = as.integer(var_num1 == var_num2),
           alpha_int = 1 - alpha_imp,
           `Variable\nImportance` = alpha_imp*`Interaction\nStrength`,
           `Interaction\nStrength` = alpha_int*`Interaction\nStrength`)

  
  

  # for plotly  -------------------------------------------------------------
  dinteraction1 <- data.frame(interaction=as.vector(dinteraction))          # Create df
  
  dinteraction1$x <- as.vector(row(dinteraction))
  dinteraction1$y <- as.vector(col(dinteraction))
  dinteraction1$varx <- ovars[dinteraction1$x]
  dinteraction1$vary <- ovars[dinteraction1$y]
  dinteraction1$varx <- factor(dinteraction1$varx, levels=ovars)
  dinteraction1$vary <- factor(dinteraction1$vary, levels=rev(ovars))      
  
  # Importance point size:
  if(length(Y_Imp) <= 5)
  {pointSize = 15
  } else if(length(Y_Imp) <= 10)
  {pointSize = 10
  }else if(length(Y_Imp <= 15))
  {pointSize = 7}
  
  labTitle = method
  
  impMat <- matrix(0,nrow=length(Y_Imp), ncol=length(Y_Imp))
  diag(impMat) <- Y_Imp
  importMatrix <- melt(impMat)
  
  p <- ggplot(data = var_int2, 
         mapping = aes(x = var_num1, y = var_num2)) + 
    scale_x_continuous(breaks = 1:length(nam), labels = nam, position = "top") + 
    scale_y_reverse(breaks = 1:length(nam), labels = nam) +
    geom_raster(aes(fill = `Interaction\nStrength`),
                alpha = var_int2$alpha_int) + 
    new_scale_fill() +
    geom_raster(aes(fill = `Variable\nImportance`),
                alpha = var_int2$alpha_imp) + 
    scale_fill_gradient(low = "red4",high = "red") +
    ggtitle(labTitle) +
    xlab('') + 
    ylab('') + 
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # This plot is only called for plotly as newGeom_raster is not supported
  Importance <- Y_Imp
  pp <- ggplot(data=dinteraction1, aes(x=varx,y=vary)) + 
    geom_tile(aes(fill = interaction), dinteraction1 %>% filter(dinteraction1$varx != dinteraction1$vary)) +
    geom_point(aes(x=Var1,y= rev(Var2),colour = Importance),size = pointSize, importMatrix %>% filter(importMatrix$Var1==importMatrix$Var2)) +
    scale_colour_gradient(low="red4", high="red") +
    labs(colour= labTitle) +
    labs(fill='Interaction \n\ Strength') +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
    scale_x_discrete(position = "top") + theme_bw()+
    xlab("")+ylab("")
  
  
  # Interactive plot using plotly
  ppp <- ggplotly(pp, tooltip = "all")

  if(interact == TRUE){
    return(ppp)
  }else{return(p)}
}
  
intHeatmap(aqRgrTask, aqMod, method = "randomForest_importance", interact = F)
intHeatmap(irClasTask, irMod, method = "ranger_permutation", interact = F)
intHeatmap(frRgrTask, frMod, method = "randomForestSRC_importance", interact = F)

?png
  


