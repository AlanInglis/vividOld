#' importanceNet
#'
#' @description Create a Network style plot displaying Variable Importance and Variable Interaction
#'
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param method A list of variable importance methods to be set by the user. These can include any of the importance methods contained within the mlr package. The default is method = randomForest.
#' @param thresholdValue A value chosen by the user which will show all the edges with weights (i.e., the interacions) above that value. For example, if thresholdValue = 0.2, then only the the interacions greater than 0.2 will be displayed.
#' @param label If label = TRUE the numerical value for the interaction strength will be displayed.
#' @param minInt Minimum interaction strength to be displayed on the legend.
#' @param maxInt Maximum interaction strength to be displayed on the legend.
#' @param minImp Minimum importance value to be displayed on the legend.
#' @param maxImp Maximum importance value to be displayed on the legend.
#' @param cluster If cluster = TRUE, then the data is clustered in groups.
#' @param ... Not currently implemented
#'
#' @return A newtwork style plot displaying interaction strength between variables on the edges and variable importance on the nodes
#'
#' @importFrom mlr "getTaskData"
#' @importFrom mlr "normalizeFeatures"
#' @importFrom mlr "generateFilterValuesData"
#' @importFrom mlr "getTaskFeatureNames"
#' @importFrom iml "Predictor"
#' @importFrom iml "Interaction"
#' @import igraph
#' @importFrom igraph "sample_pa"
#' @importFrom igraph "as_data_frame"
#' @importFrom igraph "make_graph"
#' @import ggplot2
#' @importFrom GGally "ggnet2"
#' @importFrom grDevices "colorRampPalette"
#' @importFrom ggnewscale "new_scale_fill"
#' @importFrom stats "reorder"
#' @importFrom grDevices "blues9"
#'
#' @examples
#' # Load in the data:
#' aq <- data.frame(airquality)
#' aq <- na.omit(aq)
#'
#' # Run an mlr random forest model:
#' library(mlr)
#' library(randomForest)
#' aqRgrTask  <- makeRegrTask(data = aq, target = "Ozone")
#' aqRegrLrn <- makeLearner("regr.randomForest")
#' aqMod <- train(aqRegrLrn, aqRgrTask)
#'
#' # Create graph:
#' importanceNet(task = aqRgrTask, model = aqMod,
#'  method = "randomForest_importance",
#'  thresholdValue = 0, cluster = F)
#'
#' @export




# Graph Function ----------------------------------------------------------

importanceNet <- function(task, model, method = "randomForest_importance", thresholdValue = 0,
                          label = T, minInt = 0, maxInt = 1, minImp = 0, maxImp = impLegend,
                          cluster = F,...){


  # get data:
  data <- getTaskData(task)

  # Get importance values:

  normTask <- normalizeFeatures(task, method = "standardize")
  impFeat <- generateFilterValuesData(normTask, method = method)
  Imp <- impFeat$data$value
  Imp1 <- impFeat$data$value
  impWarn <- impFeat$data$value
  impWarn <- max(impWarn)
  impLegend <- impFeat$data$value
  impLegend <- round(impLegend, 2)
  impLegend <- max(impLegend)
  #Imp <- (100)*(Imp-min(Imp))/(max(Imp)-min(Imp))  # Scale the Importance between 0-100

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
  to <- NULL
  g <- sample_pa(length(nam), m = length(nam))
  df <- igraph::as_data_frame(g)
  gDF <- dplyr::arrange(df, to)
  gDFL <- rbind(gDF$from,gDF$to)
  matched_gDFL <- gDFL[,sorted_Int$ix]

  # Create network graph:
  net.bg <- make_graph(matched_gDFL, length(nam))

  # Scale and round values:
  Int <- round(Int,2)
  E(net.bg)$weight <- Int
  Imp <- (5-1)*((Imp-min(Imp))/(max(Imp)-min(Imp)))+1 # scale between 1-5
  Imp  <- round(Imp,2)


  # Set colour palette:
  colFunc <- colorRampPalette(c("floralwhite", "dodgerblue4"))

  # Set shape of plot:
  if(thresholdValue > 0){
    l <- layout.fruchterman.reingold(net.bg)
  }else{l <- "circle"}

  # Set up plot paramters:
  colfunction <- colorRampPalette(c("floralwhite", "firebrick1"))
  nodeCol <- colfunction(length(nam))
  a <- sort(Int, decreasing  = TRUE)
  idx <- which(a > thresholdValue)
  cut.off <- a[1:max(idx)]
  `%notin%` <- Negate(`%in%`)
  net.sp  <- delete_edges(net.bg, E(net.bg)[E(net.bg)$weight %notin% cut.off])
  weightDF <- get.data.frame(net.sp) # get df of graph attributes
  edgeWidth1 <- weightDF$weight  # select edge weight
  edgeWidth2 <- (5-1)*((edgeWidth1-min(edgeWidth1))/(max(edgeWidth1)-min(edgeWidth1)))+1 # scale between 1-5

  # Warning for max/min Int/Imp:
  if(maxInt > 1){
    warning("Legend value selected for Interaction Strength (i.e., maxInt) is larger than maximum interaction value")
  }
  if(maxImp > (impWarn+1)){
    warning("Legend value selected for Variable Importance (i.e., maxImp) is larger than maximum importance value")
  }
  if(minImp < 0){
    stop("minImp must not be less than zero")
  }
  if(minInt < 0){
    stop("minInt must not be less than zero")
  }
  if(minImp > (impWarn+1)){
    stop("minImp is lager than the maximum importance value")
  }
  if(minInt > 1){
    stop("minInt must not be greater than 1")
  }

  # Set the edge colours
  edgeColour <- (E(net.bg)$weight)
  cut_int <- cut(edgeColour, 9)
  npal <- blues9
  edgeCols <- npal[cut_int]


  # Plotting function -------------------------------------------------------
  if(label == TRUE){
    p <- ggnet2(net.sp, mode = l,
                size = 0,
                #color = "red",
                edge.size = edgeWidth2,
                edge.label = edgeWidth1,
                edge.color = edgeCols) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam),nudge_y = 0.07) +
      geom_point(aes(fill = Imp1), size = Imp*2, col = "black", shape = 21) +
      scale_fill_continuous(name = "Variable\nImportance",
                            limits=c(minImp, maxImp+10), breaks=seq(minImp, maxImp+10, by= round(maxImp/10,0)),
                            low = "floralwhite" ,high = "firebrick1") +
      new_scale_fill() +
      geom_point(aes(fill = Imp), size = -1, col = nodeCol) +
      scale_fill_continuous(name = "Interaction\nStrength",
                            limits=c(minInt, maxInt), breaks=seq(minInt, maxInt, by= 0.25),
                            low = "floralwhite" ,high = "dodgerblue4")
    myList <- list(impFeat, dinteraction,  p)
    return(myList)
  }else if(label == FALSE){
    p <- ggnet2(net.sp, mode = l,
                size = 0,
                #color = "red",
                edge.size = edgeWidth2,
                #edge.label = edgeWidth1,
                edge.color = edgeCols) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam),nudge_y = 0.07) +
      geom_point(aes(fill = Imp1), size = Imp*2, col = "black", shape = 21) +
      scale_fill_continuous(name = "Variable\nImportance",
                            limits=c(minImp, maxImp), breaks=seq(minImp, maxImp, by= round(maxImp/10,0)),
                            low = "floralwhite" ,high = "firebrick1") +
      new_scale_fill() +
      geom_point(aes(fill = Imp), size = -1, col = nodeCol) +
      scale_fill_continuous(name = "Interaction\nStrength",
                            limits=c(minInt, maxInt), breaks=seq(minInt, maxInt, by= 0.25),
                            low = "floralwhite" ,high = "dodgerblue4")
    myList <- list(impFeat, p)
    return(myList)}
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


