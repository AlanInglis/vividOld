library(igraph)
library(iml)
library(mlr)
library(ggplot2)
library(GGally)
#library(network)
#library(sna)
library(randomForest)
library(randomForestSRC)
library(ranger)
library(RColorBrewer)
#library(grDevices)


set.seed(1701)

rm(list = ls())
# Get data  --------------------------------------------------------

## Air quality data
aq    <- data.frame(airquality)
aq    <- na.omit(aq)

## Iris data
ir <- data.frame(iris)
ir <- na.omit(ir)


## Friedman Data 
# Set Values
n <- 50
p <- 5
e <- rnorm(n)


# Create matrix of values
xValues <- matrix(runif(n*p, 0, 1), nrow=n)               # Create matrix 
colnames(xValues) <- paste0("x", 1:p)                      # Name columns
FRdf <- data.frame(xValues)                               # Create dataframe 


# Equation:
#y = 10sin(πx1x2) + 20(x3−0.5)^2 + 10x4 + 5x5 + ε

y = (10*sin(pi*FRdf$x1*FRdf$x2) + 20 * (FRdf$x3-0.5)^2 + 10 * FRdf$x4 + 5 * FRdf$x5 + e)


# Adding y to df
FRdf$y <- y


# mlr set up ---------------------------------------------------------

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


# Graph Function ----------------------------------------------------------


importanceNet <- function(task, model, method = "randomForest",Threshold = F, Cluster = F,...){
  
  
  # get data:
  data <- getTaskData(task)
  
  # Get importance:
  
  norm.task <- normalizeFeatures(task, method = "standardize")
  if(method == "randomForest"){
    impMethod = c("randomForest_importance")}
  else if(method == "ranger Permutation"){
    impMethod = c("ranger_permutation")}
  else if(method == "rfSRC Importance"){
    impMethod = c("randomForestSRC_importance")
  }
  else if(method >=4)
    (return("Invalid method chosen. Use ?intHeatmap for allowed methods"))
  im_feat <- generateFilterValuesData(norm.task, method = impMethod)
  Imp <- im_feat$data$value
  Imp <- (100)*(Imp-min(Imp))/(max(Imp)-min(Imp))  # Scale the Importance between 10-50 
  
  Mod <- Predictor$new(model, data = data) # iml Interaction Strength
  
  # Get names and interaction strength:
  res <- NULL
  ovars <- getTaskFeatureNames(task)
  for (i in 1:length(ovars))
    res <- rbind(res, Interaction$new(Mod, feature=ovars[i])$results)
  
  res[[".feature"]]<- reorder(res[[".feature"]], res[[".interaction"]])
  
  vars2 <- t(simplify2array(strsplit(as.character(res[[".feature"]]),":"))) # split/get feature names 
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[[".interaction"]]                              # set values   
  dinteraction <- (dinteraction+t(dinteraction))/2                          # avg over values to make symmetrical
  
  dinteraction1 <- data.frame(interaction=as.vector(dinteraction))   
  
  # setting var names:
  INT <- dinteraction1
  UINT <- unique(INT)
  UINT <- UINT[-1,]
  IMP <-  Imp
  nam <- getTaskFeatureNames(task)
  
  # # Set path direction of graph:
  g <- sample_pa(length(nam), m = length(nam))
  df <- igraph::as_data_frame(g)
  gDF <- dplyr::arrange(df, to)
  gDFL <- rbind(gDF$from,gDF$to)

  # Graph Parameters:
  
  net.bg <- make_graph(gDFL, length(nam)) 

  RImp <- round(Imp, 2)
  RImpFactor <- as.factor(RImp)
  UintScaled <- UINT *20
  Rint <- round(UintScaled, 2)
  colfunc <- colorRampPalette(c("black", "red"))
  colfuncNode <- colorRampPalette(c("blue", "red"))
  
  #highlight max interaction strength
  maxWeight <- max(UINT*100)
 
  
  if(Threshold == FALSE && Cluster == FALSE){
    p <- ggnet2(net.bg, mode = "circle", 
              size = RImp,
              node.color = "grey50",
              label = nam,
              edge.size = UintScaled,
              edge.label = Rint,
              edge.color = colfunc(length(Rint)))+
       theme(legend.text = element_text(size = 0))
    return(p) }else if(Threshold == TRUE && Cluster == FALSE){
      a <- sort(Rint, decreasing  = TRUE)
      cut.off <- a[1:5]
      net.sp  <- delete_edges(net.bg, E(net.bg)[Rint<cut.off])
      pp <- ggnet2(net.sp, mode = "circle",
                  size = RImp,
                  color = "grey50",
                  node.color = "grey50",
                  label = nam,
                  edge.size = Rint)+
        theme(legend.text = element_text(size = 0))
                  #edge.label = Rint)
                  #edge.color = colfunc(length(Rint)))
      return(pp)}
}

set.seed(1701)
importanceNet(aqRgrTask, aqMod, method = "randomForest", Threshold = F, Cluster = F)

importanceNet(irClasTask, irMod, method = "rfSRC Importance")

importanceNet(frRgrTask, frMod, method = "ranger Permutation", Threshold = F, Cluster = F) 




