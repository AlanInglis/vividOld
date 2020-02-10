
library(igraph)
library(iml)
library(mlr)
library(ggplot2)
library(GGally)
#library(network)
#library(sna)
library(RColorBrewer)
#library(grDevices)
#library(ggplot2)


# Graph Function ----------------------------------------------------------


importanceNet <- function(task, model, method = "randomForest_importance", thresholdValue = 0,
                          cluster = F,...){


  # get data:
  data <- getTaskData(task)

  # Get importance values:

  normTask <- normalizeFeatures(task, method = "standardize")
  impFeat <- generateFilterValuesData(normTask, method = method)
  Imp <- impFeat$data$value
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

  sortInt = t(dinteraction)[lower.tri(t(dinteraction), diag=FALSE)]  # get upper triangle of the matrix by row order
  sorted_Int <- sort(sortInt, index.return=TRUE)                     # Sort values whilst preserving the index
  Int <- sorted_Int$x
  nam <- getTaskFeatureNames(task)                                   # Get feature names


  # Set path direction of graph:
  g <- sample_pa(length(nam), m = length(nam))
  df <- igraph::as_data_frame(g)
  gDF <- dplyr::arrange(df, to)
  gDFL <- rbind(gDF$from,gDF$to)
  matched_gDFL <- gDFL[,sorted_Int$ix]

  # Create network graph:
  net.bg <- make_graph(matched_gDFL, length(nam))

  # Scale and round values:
  Int <- round(Int*10,2)
  E(net.bg)$weight <- Int
  Imp  <- round(Imp,2)


  # Set colour palette:
  colFunc <- colorRampPalette(c("floralwhite", "dodgerblue4"))

 # Plotting function:
  # if(cluster == FALSE){
  #   p <- ggnet2(net.bg, mode = "circle",
  #             size = Imp,
  #             node.color = "grey80",
  #             label = nam,
  #             edge.size = Rint,
  #             edge.label = Rint,
  #            edge.color = colfunc(length(Rint)))+
  #      theme(legend.text = element_text(size = 0))
  #   return(p) }
     #}else
       if(cluster == FALSE){
       a <- sort(Int, decreasing  = TRUE)
       idx <- which(a > thresholdValue)
       cut.off <- a[1:max(idx)]
       `%notin%` <- Negate(`%in%`)
       net.sp  <- delete_edges(net.bg, E(net.bg)[E(net.bg)$weight %notin% cut.off])
       weightDF <- get.data.frame(net.sp) # get df of graph attributes
       edgeWidth1 <- weightDF$weight  # select edge weight
       pp <- ggnet2(net.sp, mode = "circle",
                   size = Imp,
                   color = "red",
                   #label = nam,
                   edge.size = edgeWidth1,
                   edge.label = edgeWidth1,
                   edge.color = colFunc(length(edgeWidth1)))+
         theme(legend.text = element_text(size = 0))+
         geom_label(aes(label = nam),
                          nudge_y = 0.07,
                   )
       return(pp)}
     # }else if(Threshold == FALSE && Cluster == TRUE){
       # V(net.bg)$label <- nam
       # E(net.bg)$width <- Rint
       # E(net.bg)$arrow.mode <- 0
       # E(net.bg)$label <- Rint
       # E(net.bg)$label.cex <- 1
       # clp <- cluster_optimal(net.bg)
       #ppp <- plot(clp, net.bg, vertex.label.family = "Helvetica", edge.label.family = "Helvetica")
       # weightDF <- get.data.frame(net.bg) # get df of graph attributes
       # EdgeWidth2 <- weightDF$weight
       # clp <- cluster_optimal(net.bg)
       #
       # ppp <- ggnet2(clp)
       # , mode = "circle",
       #            size = Imp,
       #            color = "grey50",
       #            node.color = "grey50")
     #              label = nam,
     #              edge.size = Rint,
     #              edge.label = Rint,
     #    edge.color = colfunc(length(Rint)))+
     # theme(legend.text = element_text(size = 0))
     #  return(ppp)}
     # }else if(Threshold == TRUE && Cluster == TRUE){
     #   V(net.bg)$label <- nam
     #   E(net.bg)$width <- Rint
     #   E(net.bg)$arrow.mode <- 0
     #   a <- sort(UINT, decreasing  = TRUE)
     #   cut.off <- a[1:5]
     #   net.sp  <- delete_edges(net.bg, E(net.bg)[UINT<cut.off])
     #   clp <- cluster_optimal(net.sp)
     #   pppp <- plot(clp, net.sp, vertex.label.family = "Helvetica", edge.label.family = "Helvetica")}
}

# Get data ----------------------------------------------------------------
## Air quality data (used for regression)

aq <- data.frame(airquality)
aq <- na.omit(aq)

# mlr set up --------------------------------------------------------------

aqRgrTask  <- makeRegrTask(data = aq, target = "Ozone")
aqRegrLrn <- makeLearner("regr.randomForest")
aqMod <- train(aqRegrLrn, aqRgrTask)


importanceNet(aqRgrTask, aqMod, method = "randomForest_importance", thresholdValue = 0, cluster = F)

# With Thresholding
importanceNet(aqRgrTask, aqMod, method = "randomForest_importance", thresholdValue = 0.99, cluster = F)


