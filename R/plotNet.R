#' plotNet
#'
#'   @description Create a Network style plot displaying Variable
#'  and Variable Interaction.
#'
#' @param mat A matrix of values to be plotted. Either added by the user or created using the prepFunc() function.
#' @param thresholdValue A value chosen by the user which will show all the edges with weights (i.e., the interacions) above that value. For example, if thresholdValue = 0.2, then only the the interacions greater than 0.2 will be displayed.
#' @param label If label = TRUE the numerical value for the interaction strength will be displayed.
#' @param minInt Minimum interaction strength to be displayed on the legend.
#' @param maxInt Maximum interaction strength to be displayed on the legend.
#' @param minImp Minimum importance value to be displayed on the legend.
#' @param maxImp Maximum importance value to be displayed on the legend.
#' @param labelNudge A value, set by the user, to determine the y_postioning of the variables names. A higher value will postion the label farther above the nodes.
#' @param layout Determines the shape, or layout, of the plotted graph.
#' @param cluster If cluster = TRUE, then the data is clustered in groups.
#' @param clusterType = Network-based clustering. Any of the appropriate cluster types from the igraph package are allowed.
#' @param ... Not currently implemented.
#'
#' @return A newtwork style plot displaying interaction strength between variables on the edges and variable importance on the nodes.
#'
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
#' @importFrom reshape "melt"
#' @importFrom ggalt "geom_encircle"
#'
#' @examples
#' # Load in the data:
#' aq <- data.frame(airquality)
#' aq <- na.omit(aq)
#'
#' # Run an mlr ranger model:
#' library(mlr3)
#' library(mlr3learners)
#' library(ranger)
#' aq_Task = TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn = lrn("regr.ranger", importance = "permutation")
#' aq_Mod <- aq_lrn$train(aq_Task)
#'
#' # Create matrix
#' myMat <- vividMatrix(task = aq_Task,  model = aq_Mod)
#'
#' # Create plot:
#' plot(myMat, type = "network")


# Plotting Function -------------------------------------------------------
# -------------------------------------------------------------------------
plotNet <- function(dinteraction,
                    model,
                    thresholdValue = 0,
                    label, minInt = 0, maxInt = NULL, minImp = NULL, maxImp = NULL,
                    labelNudge = 0.05, layout = "circle",
                    cluster = F, clusterType = cluster_optimal, ...){


  # Get importance values
  Imp <- diag(dinteraction)
  Imp1 <-  Imp
  impWarn <- Imp
  impWarn <- max(impWarn)
  impLegend <- Imp
  impLegend <- round(impLegend, 2)
  impLegend <- max(impLegend)+0.5
  minimumImp <- min(Imp)

  # Sort interaction values
  sortInt = t(dinteraction)[lower.tri(t(dinteraction), diag=FALSE)]  # get upper triangle of the matrix by row order
  sorted_Int <- sort(sortInt, index.return=TRUE)                     # Sort values whilst preserving the index
  Int <- sorted_Int$x
  maximumInt <- max(Int)
  #maximumInt <- max(Int)+0.01
  nam <- colnames(dinteraction)                     # Get feature names

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
  #Int <- round(Int,5)
  #Int[Int<=1e-5] <-0.01
  E(net.bg)$weight <- Int
  Imp <- (5-1)*((Imp-min(Imp))/(max(Imp)-min(Imp)))+1 # scale between 1-5
  Imp  <- round(Imp,2)

  # Create dataframe to disply to user:
  impDf <- Imp


  # Set the edge colours
  colfunction <- colorRampPalette(c("floralwhite", "dodgerblue4"))
  edgeColour <- (E(net.bg)$weight)
  cut_int <- cut(edgeColour, 9)
  if(is.null(maxInt)){
  npal <- colfunction(9)
  }else{colVal <- 9 * (maxInt/0.1)
    npal <- colfunction(colVal)}
  edgeCols <- npal[cut_int]

  # Get edge weights
  weightDF <- get.data.frame(net.bg) # get df of graph attributes
  #weightDF[weightDF<=1e-5] <- 0.01
  edgeWidth1 <- weightDF$weight  # select edge weight
  edgeWidthScaled <- (5-1)*((edgeWidth1-min(edgeWidth1))/(max(edgeWidth1)-min(edgeWidth1)))+1 # scale between 1-5



  # THRESHOLDING ------------------------------------------------------------
  # -------------------------------------------------------------------------


  if(thresholdValue > 0){
    a <- sort(Int, decreasing  = TRUE)
    # Warning message if threshold value is set too high or too low
    if(thresholdValue > max(a)){
      stop("Selected threshold value is larger than maximum interaction strength")
    }else if(thresholdValue < 0){
      stop("Selected threshold value is less than minimum interaction strength")
    }
    idx <- which(a > thresholdValue)
    cut.off <- a[1:max(idx)]
    # Thresholded colours
    indexCol <- rev(edgeCols)
    edgeCols <- indexCol[idx]
    edgeCols <- rev(edgeCols)
    # Thresholded edge weights
    indexWeight <- rev(edgeWidthScaled)
    edgeW <- indexWeight[idx]
    edgeW <- rev(edgeW)
    # Thresholded edge labels
    indexLabel <- rev(edgeWidth1)
    edgeL <- indexLabel[idx]
    edgeL <- rev(edgeL)
    # Thresholded network
    `%notin%` <- Negate(`%in%`)
    net.sp  <- delete_edges(net.bg, E(net.bg)[E(net.bg)$weight %notin% cut.off])

    # Delete vertex that have no edges (if thresholding)
    Isolated <- which(igraph::degree(net.sp)==0)
    if(length(Isolated) == 0){
      net.sp <- net.sp
    }else{
      net.sp <- igraph::delete.vertices(net.sp, Isolated)
      Imp1 <- Imp1[-c(Isolated)]
      Imp <- Imp[-c(Isolated)]
      nam <- nam[-c(Isolated)]
    }


  }else{net.sp <- net.bg
  weightDF <- get.data.frame(net.sp) # get df of graph attributes
  #weightDF[weightDF<=1e-5] <- 0.01
  edgeL <- weightDF$weight  # select edge weight
  edgeW <- (5-1)*((edgeWidth1-min(edgeWidth1))/(max(edgeWidth1)-min(edgeWidth1)))+1 # scale between 1-5
  }







  l <- layout
  # min/max legend values:
  if(is.null(maxInt)){
    maxInt <- maximumInt
  }else{maxInt <- maxInt}

  if(is.null(maxImp)){
    maxImp <- impLegend
  }else{maxImp <- maxImp}

  if(is.null(minImp)){
    minImp <- minimumImp
  }else{minImp <- minImp}

  # Whether to show edge label
  if(label == T){
    edgeL <- edgeL
  }else{edgeL <- NULL}


  intMatrix <- round(dinteraction, 3)


  # PLOTTING ----------------------------------------------------------------
  #-----------------------------------------------------------------------

  if(cluster){

    l_1 <- layout_with_fr(net.sp)
    com <- clusterType(net.sp)
    V(net.sp)$color <- com$membership
    group <- V(net.sp)$color
    group <- factor(group)

    g <- set_graph_attr(net.sp, "layout", layout_with_fr(net.sp))
    colrs <- adjustcolor( c("yellow", "red", "blue", "black","purple",
                            "orange", "pink", "green", "red" , "blue"))
    colorC <- colrs[group]
    pp <- ggnet2(g,
                 mode = l_1,
                 size = 0,
                 edge.size = edgeW,
                 edge.label = edgeL,
                 edge.color = edgeCols) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam),nudge_y = labelNudge) +
      geom_point(aes(fill = Imp1), size = Imp*2, col = "transparent", shape = 21) +
      scale_fill_continuous(name = "Variable\nImportance",
                            limits=c(minImp, maxImp),
                            low = "floralwhite" ,high = "firebrick1") +
      new_scale_fill() +
      geom_point(aes(fill = 0), size = -1) +
      scale_fill_continuous( name = "Interaction\nStrength",
                             limits=c(minInt, maxInt),
                             low = "floralwhite" ,high = "dodgerblue4")


    groupV <- as.vector(group)
    fillCols <- c("yellow", "red", "blue", "black","purple",
                  "orange", "pink", "green", "red" , "blue")
    colCluster <- fillCols[group]
    colCluster <- as.vector(colCluster)
    pp <- pp + geom_encircle(aes(group = groupV),
                             alpha = 0.2,
                             fill = colCluster)

    return(pp)
  }else{
    p <- ggnet2(net.sp, mode = l,
                size = 0,
                edge.size = edgeW,
                edge.label = edgeL,
                edge.color = edgeCols) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam),nudge_y = labelNudge) +
      geom_point(aes(fill = Imp1), size = Imp*2, colour = "transparent", shape = 21) +
      scale_fill_continuous(name = "Variable\nImportance",
                            limits=c(minImp, maxImp),
                            low = "floralwhite" ,high = "firebrick1") +
      new_scale_fill() +
      geom_point(aes(fill = 0), size = -1) +
      scale_fill_continuous(name = "Interaction\nStrength",
                            limits=c(minInt, maxInt),
                            low = "floralwhite" ,high = "dodgerblue4")

    return(p)
  }
}

