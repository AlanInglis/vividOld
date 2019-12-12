library(igraph)
library(randomForest)
library(iml)
library(mlr)
set.seed(1701)


# Air Quality Data --------------------------------------------------------

# Get data and create RF
aq    <- data.frame(airquality)
aq    <- na.omit(aq)
aqRF  <- randomForest(Ozone ~ ., importance= TRUE, localImp = TRUE, data=aq, type=1)
varImpPlot(aqRF)

imp = FeatureImp$new(predictor, loss, compare = "ratio", n.repetitions = 5)

imp$plot()
imp$results
# Iris data ---------------------------------------------------------------

ir <- data.frame(iris)
ir <- na.omit(ir)
irRF <- randomForest(Species ~., localImp = TRUE, data = ir)
varImpPlot(irRF)

# Friedman Data -----------------------------------------------------------

# Set Values
n <- 100
p <- 20
e <- rnorm(n)


# Create matrix of values
xValues <- matrix(runif(n*p, 0, 1), nrow=n)               # Create matrix 
colnames(xValues)<- paste0("x", 1:p)                      # Name columns
FRdf <- data.frame(xValues)                               # Create dataframe 


# Equation:
#y = 10sin(πx1x2) + 20(x3−0.5)^2 + 10x4 + 5x5 + ε

y = (10*sin(pi*FRdf$x1*FRdf$x2) + 20 * (FRdf$x3-0.5)^2 + 10 * FRdf$x4 + 5 * FRdf$x5 + e)


# Adding y to df
FRdf$y <- y

# Create RF
FRrf <- randomForest(y ~ ., localImp = TRUE,  data=FRdf)


# Graph Function ----------------------------------------------------------

## GRAPH FUNCTION: 

## GRAPH FUNCTION: 

rgrTask  <- makeRegrTask(data=aq, target = "Ozone")
regr.lrn = makeLearner("regr.randomForest")
aqMod = train(regr.lrn,rgrTask)



importanceNet <- function(data, task, model, method = 1, Threshold = FALSE, Cluster = FALSE,...){
  
  # Get importance:
  
  norm.task <- normalizeFeatures(task, method = "standardize")
  if(method == 1){
    impMethod = c("randomForest_importance")}
  else if(method == 2){
    impMethod = c("ranger_permutation")}
  else if(method == 3){
    impMethod = c("randomForestSRC_importance")
  }
  else if(method >=4)
    (return("Invalid method chosen. Method must be between 1-3. See ?importanceNet for allowed methods"))
  
  im_feat <- generateFilterValuesData(norm.task, method = impMethod)
  Imp <- im_feat$data$value
  Imp = (50-10)*(Imp-min(Imp))/(max(Imp)-min(Imp))+10  # Scale the Importance
  
  Mod <- Predictor$new(model, data = data) # iml Interaction Strength
  
  # Get names and interaction strength:
  res <- NULL
  ovars <- getTaskFeatureNames(task)
  for (i in 1:length(ovars))
    res <- rbind(res, Interaction$new(Mod, feature=ovars[i])$results)
  
  res[[".feature"]]<- reorder(res[[".feature"]], res[[".interaction"]])
  
  vars2 <- t(simplify2array(strsplit(as.character(res[[".feature"]]),":"))) # split/get feature names 
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction)<- colnames(dinteraction) <- ovars                  # set names
  dinteraction[vars2] <- res[[".interaction"]]                              # set values   
  dinteraction <- (dinteraction+t(dinteraction))/2                          # avg over values to make symmetrical
  
  dinteraction1 <- data.frame(interaction=as.vector(dinteraction))   
  
  # setting var names:
  INT <- dinteraction1
  UINT <- unique(INT)
  UINT <- UINT[-1,]
  IMP <-  Imp
  nam <- getTaskFeatureNames(task)
  
  # Set path direction:
  g <- sample_pa(length(nam), m = length(nam))
  df <- igraph::as_data_frame(g)
  gDF <- dplyr::arrange(df, to)
  gDFL <- rbind(gDF$from,gDF$to)
  gdf <- c(gDFL)
  
  # Graph Parameters:
  net.bg <- make_graph(gdf, length(nam))
  V(net.bg)$size <- IMP
  V(net.bg)$frame.color <- "black"
  V(net.bg)[IMP < IMP+1]$color <- "green"
  V(net.bg)[IMP < 40]$color <- "orange"
  V(net.bg)[IMP < 30]$color <- "lightblue"
  V(net.bg)[IMP < 20]$color <- "red"
  V(net.bg)[IMP < 10]$color <- "wheat"
  V(net.bg)$label <- nam
  E(net.bg)$arrow.mode <- 0
  E(net.bg)$width <- UINT*100
  l <- layout_in_circle(net.bg)
  
  # Threshold and Cluster option:
  if(Threshold == FALSE && Cluster == FALSE){
    p <- plot(net.bg, layout=l, edge.color="gray30", title = "skdhjfb")
    return(p) 
  }else if(Threshold == TRUE && Cluster == FALSE){
    a <- sort(UINT, decreasing  = TRUE)
    cut.off <- a[1:5]
    net.sp  <- delete_edges(net.bg, E(net.bg)[UINT<cut.off])
    pp <- plot(net.sp, layout=l, edge.color="gray30")
    return(pp)
  }else if(Threshold == FALSE && Cluster == TRUE){
    clp <- cluster_optimal(net.bg)
    ppp <- plot(clp, net.bg,  edge.color="gray50")
    return(ppp)
  }else if(Threshold == TRUE && Cluster == TRUE){
    a <- sort(UINT, decreasing  = TRUE)
    cut.off <- a[1:5]
    net.sp  <- delete_edges(net.bg, E(net.bg)[UINT<cut.off])
    clp <- cluster_optimal(net.sp)
    pppp <- plot(clp, net.sp,  edge.color="gray50")
    return(pppp)
  }
  
}

importanceNet(aq, aqRgrTask, aqMod, method = 2, Threshold = FALSE, Cluster = FALSE)
importanceNet(FRdf, frRgrTask, frMod, method = 3, Threshold = FALSE, Cluster = FALSE)
importanceNet(ir, irClasTask, irMod, method = 2, Threshold = TRUE, Cluster = TRUE)

