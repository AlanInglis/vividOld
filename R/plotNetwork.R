#' plotNetwork
#'
#' @description Create a Network style plot displaying Variable
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
#' aq_Mod <- lrn$train(aq_Task)
#'
#'#' # Create matrix
#' myMat <- prepFunc(task = aq_Task, learner = aq_Lrn, model = aq_Mod)
#'
#' # Create graph:
#' plotNetwork(myMat, thresholdValue = 0, cluster = F)
#'
#' @export




# Graph Function ----------------------------------------------------------
# -------------------------------------------------------------------------

plotNetwork <- function(mat, thresholdValue = 0,
                          label = FALSE, minInt = 0, maxInt = NULL, minImp = 0, maxImp = NULL,
                          labelNudge = 0.05, layout = "circle",
                          cluster = F,...){


  netPrep <- mat
  plotNet(netPrep, model, thresholdValue, label, layout = layout,
          minInt, maxInt, minImp , maxImp)
}


# Plotting Function -------------------------------------------------------
# -------------------------------------------------------------------------
plotNet <- function(dinteraction,
                    model,
                    thresholdValue = 0,
                    label, minInt = 0, maxInt = NULL, minImp = 0, maxImp = NULL,
                    labelNudge = 0.05, layout = "circle",
                    cluster = F){

  Imp <- diag(dinteraction)
  Imp1 <-  Imp
  impWarn <- Imp
  impWarn <- max(impWarn)
  impLegend <- Imp
  impLegend <- round(impLegend, 2)
  impLegend <- max(impLegend)+0.5


  sortInt = t(dinteraction)[lower.tri(t(dinteraction), diag=FALSE)]  # get upper triangle of the matrix by row order
  sorted_Int <- sort(sortInt, index.return=TRUE)                     # Sort values whilst preserving the index
  Int <- sorted_Int$x
  maximumInt <- max(Int)+0.01
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
  Int <- round(Int,2)
  Int[Int<=1e-5] <-0.01
  E(net.bg)$weight <- Int
  Imp <- (5-1)*((Imp-min(Imp))/(max(Imp)-min(Imp)))+1 # scale between 1-5
  Imp  <- round(Imp,2)

  # Create dataframe to disply to user:
  impDf <- Imp



  # Set up thresholding:
  a <- sort(Int, decreasing  = TRUE)
  # Warning message if threshold value is set too high or too low
  if(thresholdValue > max(a)){
    stop("Selected threshold value is larger than maximum interaction strength")
  }else if(thresholdValue < 0){
    stop("Selected threshold value is less than minimum interaction strength")
  }
  idx <- which(a > thresholdValue)
  cut.off <- a[1:max(idx)]
  `%notin%` <- Negate(`%in%`)
  net.sp  <- delete_edges(net.bg, E(net.bg)[E(net.bg)$weight %notin% cut.off])
  weightDF <- get.data.frame(net.sp) # get df of graph attributes
  weightDF[weightDF<=1e-5] <- 0.01
  edgeWidth1 <- weightDF$weight  # select edge weight
  if(var(edgeWidth1)==0){
    edgeWidth2 <- edgeWidth1
  }else{
  edgeWidth2 <- (5-1)*((edgeWidth1-min(edgeWidth1))/(max(edgeWidth1)-min(edgeWidth1)))+1 # scale between 1-5
  }

  # Set the edge colours
  colfunction <- colorRampPalette(c("floralwhite", "dodgerblue4"))

  # Set shape of plot:
  if(thresholdValue > 0){
    l <- layout.reingold.tilford(net.bg, circular=T)
    edgeColour <- (E(net.sp)$weight)
    cut_int <- cut(edgeColour, 9)
    npal <- colfunction(9)
    edgeCols <- npal[cut_int]
  }else{l <- layout
    edgeColour <- (E(net.sp)$weight)
    cut_int <- cut(edgeColour, 9)
    npal <- colfunction(9)
    edgeCols <- npal[cut_int]
  }


  # min/max legend values:
  if(is.null(maxInt)){
    maxInt <- maximumInt
  }else{maxInt <- maxInt}
  if(is.null(maxImp)){
    maxImp <- impLegend
  }else{maxImp <- maxImp}
  intMatrix <- round(dinteraction, 3)

  # create plot -------------------------------------------------------
  if(!(label)){
    p <-ggnet2(net.sp, mode = l,
               size = 0,
               edge.size = edgeWidth2,
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
    myList <- list("Variable Importance:" = impDf, "Interaction Matrix:" = intMatrix, p)
    return(myList)
  }else{
    p <- ggnet2(net.sp, mode = l,
                size = 0,
                edge.size = edgeWidth2,
                edge.label = edgeWidth1,
                edge.color = edgeCols) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam),nudge_y = labelNudge) +
      geom_point(aes(fill = Imp1), size = Imp*2, colour = "transparent", shape = 21) +
      scale_fill_continuous(name = "Variable\nImportance",
                            low = "floralwhite" ,high = "firebrick1") +
      new_scale_fill() +
      geom_point(aes(fill = 0), size = -1) +
      scale_fill_continuous(name = "Interaction\nStrength",
                            limits=c(minInt, maximumInt),
                            low = "floralwhite" ,high = "dodgerblue4")
    myList <- list("Variable Importance:" = impDf, "Interaction Matrix:" = dinteraction, p)
    return(myList)
   }
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
