library(igraph)
library(iml)
library(mlr)

library(randomForest)
library(randomForestSRC)
library(ranger)
library(RColorBrewer)
set.seed(1701)


# Get data  --------------------------------------------------------

## Air quality data
aq    <- data.frame(airquality)
aq    <- na.omit(aq)

## Iris data
ir <- data.frame(iris)
ir <- na.omit(ir)


## Friedman Data 
# Set Values
n <- 100
p <- 10
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


importanceNet <- function(data, task, model, method = "randomForest", 
                          Threshold = FALSE, Cluster = FALSE, edge.label = FALSE,...){
  
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
  Imp <- (50-10)*(Imp-min(Imp))/(max(Imp)-min(Imp))+10  # Scale the Importance between 10-50 
  
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
  
  # Set path direction of graph:
  g <- sample_pa(length(nam), m = length(nam))
  df <- igraph::as_data_frame(g)
  gDF <- dplyr::arrange(df, to)
  gDFL <- rbind(gDF$from,gDF$to)
  gdf <- c(gDFL)
  
  # Graph Parameters:
  net.bg <- make_graph(gdf, length(nam))
  V(net.bg)$size <- IMP
  V(net.bg)$frame.color <- "black"
  V(net.bg)[IMP < IMP+1]$color <- "#A6CEE3"#light blue
  V(net.bg)[IMP < 40]$color <- "#1F78B4"#dark blue
  V(net.bg)[IMP < 30]$color <- "#B2DF8A"#light green
  V(net.bg)[IMP < 20]$color <- "#33A02C"#dark green
  V(net.bg)[IMP < 10]$color <- "#FB9A99"# pink
  V(net.bg)$label <- nam
  V(net.bg)$label.cex <- 1.3
  V(net.bg)$label.color <- "black"
  E(net.bg)$arrow.mode <- 0
  E(net.bg)$label <- round(UINT, 3)
  E(net.bg)$width <- UINT*100
  
  l <- layout_in_circle(net.bg)
  
  #Colour legend 
  pal <- brewer.pal(12,"Paired")
  Group <- gl(5, 2, labels = c("40-50","30-40","20-30","10-20", "0-10"))
  
  # Add option to display edge label (i.e., interaction strength)
  if(edge.label == TRUE){
    E(net.bg)$label.cex <- 1
  }else{E(net.bg)$label.cex <- 0.001}
  
  
  ## Ajusting edge label text position
  # Start with the centers of the edges (on line)
  ELx = rep(0, ecount(net.bg))
  ELy = rep(0, ecount(net.bg))
  for(i in 1:ecount(net.bg)) {
    ELx[i] = (l[ends(net.bg,i)[1],1] + l[ends(net.bg,i)[2],1])/2
    ELy[i] = (l[ends(net.bg,i)[1],2] + l[ends(net.bg,i)[2],2])/2 }
  
  ## Adjust perpendicular to line
  d = 0.05
  for(i in 1:ecount(net.bg)) {
    if(abs(l[ends(net.bg,i)[1],2] - l[ends(net.bg,i)[2],2]) < 0.1) {
      ## This avoids problems with horizontal edges
      ELy[i] = ELy[i] + shift 
    } else {
      S = (l[ends(net.bg,i)[2],1] - l[ends(net.bg,i)[1],1]) / 
        (l[ends(net.bg,i)[1],2] - l[ends(net.bg,i)[2],2])
      shift = d / sqrt(1 + S^2)
      ELx[i] = ELx[i] + shift
      ELy[i] = ELy[i] + S*shift
    }
  }
 
  
  #highlight max interaction strength
  maxWeight <- max(UINT*100)
  E(net.bg)$color[E(net.bg)$width == maxWeight] <- 'red'
  E(net.bg)$color[E(net.bg)$width != maxWeight] <- 'gray70'
  
   
  # Threshold and Cluster option:
  if(Threshold == FALSE && Cluster == FALSE){
    p <- plot(net.bg, layout = l, vertex.label.family = "Helvetica", edge.label.family = "Helvetica",
              rescale = FALSE, xlim=range(l[,1]), ylim=range(l[,2]), 
              edge.label.x = ELx, edge.label.y = ELy)
    legend("topleft",bty = "n",
           legend = levels(Group),
           fill = pal, border = NA, title = "Importance Values")
    return(p) 
  }else if(Threshold == TRUE && Cluster == FALSE){
    a <- sort(UINT, decreasing  = TRUE)
    cut.off <- a[1:5]
    net.sp  <- delete_edges(net.bg, E(net.bg)[UINT<cut.off])
    pp <- plot(net.sp, layout = l, vertex.label.family = "Helvetica", edge.label.family = "Helvetica")
    legend("topleft",bty = "n",
           legend = levels(Group),
           fill = pal, border = NA, title = "Importance Values")
    return(pp)
  }else if(Threshold == FALSE && Cluster == TRUE){
    clp <- cluster_optimal(net.bg)
    ppp <- plot(clp, net.bg, vertex.label.family = "Helvetica", edge.label.family = "Helvetica")
    legend("topleft",bty = "n",
           legend = levels(Group),
           fill = pal, border = NA, title = "Importance Values")
    return(ppp)
  }else if(Threshold == TRUE && Cluster == TRUE){
    a <- sort(UINT, decreasing  = TRUE)
    cut.off <- a[1:5]
    net.sp  <- delete_edges(net.bg, E(net.bg)[UINT<cut.off])
    clp <- cluster_optimal(net.sp)
    pppp <- plot(clp, net.sp, vertex.label.family = "Helvetica", edge.label.family = "Helvetica")
    legend("topleft",bty = "n",
           legend = levels(Group),
           fill = pal, border = NA, title = "Importance Values")
    return(pppp)
    }
}

importanceNet(aq, aqRgrTask, aqMod, method = "randomForest", 
              Threshold = F, Cluster = F, edge.label= F)

importanceNet(ir, irClasTask, irMod, method = "rfSRC Importance", 
              Threshold = F, Cluster = F, edge.label = TRUE)

importanceNet(FRdf, frRgrTask, frMod, method = "ranger Permutation", 
              Threshold = F, Cluster = FALSE, edge.label = F) 



