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
#' @param intPal A colorspace colour palette to display the interaction values.
#' @param impPal A colorspace colour palette to display the importance values.
#' @param labelNudge A value, set by the user, to determine the y_postioning of the variables names. A higher value will postion the label farther above the nodes.
#' @param layout Determines the shape, or layout, of the plotted graph.
#' @param cluster If cluster = TRUE, then the data is clustered in groups.
#' @param clusterType = Network-based clustering. Any of the appropriate cluster types from the igraph package are allowed.
#' @param clusterLayout = Determines the shape, or layout, of the clustered plotted graph.
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
#' @importFrom cowplot "get_legend"
#' @importFrom cowplot "plot_grid"
#' @importFrom colorspace "sequential_hcl"
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
#' aq_Task <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn <- lrn("regr.ranger", importance = "permutation")
#' aq_Mod <- aq_lrn$train(aq_Task)
#'
#' # Create matrix
#' myMat <- vividMatrix(task = aq_Task,  model = aq_Mod)
#'
#' # Create plot:
#' plot(myMat, type = "network")

#
# # Plotting Function -------------------------------------------------------
# # -------------------------------------------------------------------------
# plotNet <- function(dinteraction,
#                     model,
#                     thresholdValue = 0,
#                     label,
#                     minInt = 0, maxInt = NULL, minImp = NULL, maxImp = NULL,
#                     intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
#                     impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
#                     labelNudge = 0.05,
#                     layout = "circle",
#                     cluster = F,
#                     clusterType = cluster_optimal,
#                     clusterLayout = layout_with_fr,
#                     ...){
#
#
#   # Get importance values
#   Imp <- diag(dinteraction)
#   Imp1 <-  Imp
#   impWarn <- Imp
#   impy <- Imp
#   impWarn <- max(impWarn)
#   impLegend <- Imp
#   impLegend <- round(impLegend, 2)
#   impLegend <- max(impLegend)+0.005
#   minimumImp <- floor(min(Imp))
#
#   # Sort interaction values
#   sortInt = t(dinteraction)[lower.tri(t(dinteraction), diag=FALSE)]  # get upper triangle of the matrix by row order
#   sorted_Int <- sort(sortInt, index.return=TRUE)                     # Sort values whilst preserving the index
#   Int <- sorted_Int$x
#   maximumInt <- max(Int)
#   nam <- colnames(dinteraction)                                      # Get feature names
#
#   # Set path direction of graph:
#   to <- NULL
#   g <- sample_pa(length(nam), m = length(nam))
#   df <- igraph::as_data_frame(g)
#   gDF <- dplyr::arrange(df, to)
#   gDFL <- rbind(gDF$from,gDF$to)
#   matched_gDFL <- gDFL[,sorted_Int$ix]
#
#   # Create network graph:
#   net.bg <- make_graph(matched_gDFL, length(nam))
#
#   # Scale and round values:
#   E(net.bg)$weight <- Int
#   Imp <- (5-1)*((Imp-min(Imp))/(max(Imp)-min(Imp)))+1 # scale between 1-5
#   Imp  <- round(Imp,2)
#
#   # Set the edge colours
#   colfunction <- intPal
#   edgeColour <- (E(net.bg)$weight)
#   cut_int <- cut(edgeColour, 10)
#   if(is.null(maxInt)){
#     npal <- colfunction[1:length(edgeColour)]
#   }else{
#     npal <- colfunction[1:length(edgeColour)]}
#   edgeCols <- npal[cut_int]
#
#
#   print(npal)
#   print(edgeCols)
#
#
#
#   # Get edge weights
#   weightDF <- get.data.frame(net.bg) # get df of graph attributes
#   edgeWidth1 <- weightDF$weight  # select edge weight
#   edgeWidthScaled <- (5-1)*((edgeWidth1-min(edgeWidth1))/(max(edgeWidth1)-min(edgeWidth1)))+1 # scale between 1-5
#
#
#
#   # THRESHOLDING ------------------------------------------------------------
#   # -------------------------------------------------------------------------
#
#
#   if(thresholdValue > 0){
#     a <- sort(Int, decreasing  = TRUE)
#     # Warning message if threshold value is set too high or too low
#     if(thresholdValue > max(a)){
#       stop("Selected threshold value is larger than maximum interaction strength")
#     }else if(thresholdValue < 0){
#       stop("Selected threshold value is less than minimum interaction strength")
#     }
#     idx <- which(a > thresholdValue)
#     cut.off <- a[1:max(idx)]
#     # Thresholded colours
#     indexCol <- rev(edgeCols)
#     edgeCols <- indexCol[idx]
#     edgeCols <- rev(edgeCols)
#     # Thresholded edge weights
#     indexWeight <- rev(edgeWidthScaled)
#     edgeW <- indexWeight[idx]
#     edgeW <- rev(edgeW)
#     # Thresholded edge labels
#     indexLabel <- rev(edgeWidth1)
#     edgeL <- indexLabel[idx]
#     edgeL <- rev(edgeL)
#     # Thresholded network
#     `%notin%` <- Negate(`%in%`)
#     net.sp  <- delete_edges(net.bg, E(net.bg)[E(net.bg)$weight %notin% cut.off])
#
#     # Delete vertex that have no edges (if thresholding)
#     Isolated <- which(igraph::degree(net.sp)==0)
#     if(length(Isolated) == 0){
#       net.sp <- net.sp
#     }else{
#       net.sp <- igraph::delete.vertices(net.sp, Isolated)
#       Imp1 <- Imp1[-c(Isolated)]
#       Imp <- Imp[-c(Isolated)]
#       nam <- nam[-c(Isolated)]
#     }
#
#
#   }else{net.sp <- net.bg
#   weightDF <- get.data.frame(net.sp) # get df of graph attributes
#   edgeL <- weightDF$weight  # select edge weight
#   edgeW <- (5-1)*((edgeWidth1-min(edgeWidth1))/(max(edgeWidth1)-min(edgeWidth1)))+1 # scale between 1-5
#   }
#
#
#
#
#   l <- layout
#   # min/max legend values:
#   if(is.null(maxInt)){
#     maxInt <- maximumInt
#   }else{maxInt <- maxInt}
#
#   if(is.null(maxImp)){
#     maxImp <- impLegend
#   }else{maxImp <- maxImp}
#
#   if(is.null(minImp)){
#     minImp <- minimumImp
#   }else{minImp <- minImp}
#
#
#   # Whether to show edge label
#   if(label == T){
#     edgeL <- edgeL
#     edgeL <- round(edgeL, 3)
#   }else{edgeL <- NULL}
#
#
#   intMatrix <- round(dinteraction, 3)
#
#
#   # PLOTTING ----------------------------------------------------------------
#   #-----------------------------------------------------------------------
#
#   if(cluster){
#
#     l_1 <- clusterLayout(net.sp)
#     com <- clusterType(net.sp)
#     V(net.sp)$color <- com$membership
#     group <- V(net.sp)$color
#     group <- factor(group)
#
#
#     g <- set_graph_attr(net.sp, "layout", layout_in_circle(net.sp))
#     colrs <- adjustcolor( c("yellow", "red", "blue", "black","purple",
#                             "orange", "pink", "green", "red" , "blue", "yellow"))
#     colorC <- colrs[group]
#
#     pcl <- ggnet2(net.sp,
#                   mode = l_1,
#                   size = 0,
#                   edge.size = edgeW,
#                   edge.label = edgeL,
#                   edge.color = edgeCols) +
#       theme(legend.text = element_text(size = 10)) +
#       geom_label(aes(label = nam),nudge_y = labelNudge) +
#       geom_point(aes(fill = Imp1), size = Imp*2, col = "transparent", shape = 21) +
#       scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits=c(minImp, maxImp)) +
#       theme(legend.position = "none")
#
#
#     ppcl <- ggnet2(net.sp,
#                    mode = l_1,
#                    size = 0,
#                    edge.size = edgeW,
#                    edge.label = edgeL,
#                    edge.color = edgeCols) +
#       theme(legend.text = element_text(size = 10)) +
#       geom_label(aes(label = nam),nudge_y = labelNudge) +
#       geom_point(aes(fill = Imp1), size = Imp*2, col = "transparent", shape = 21) +
#       scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits=c(minImp, maxImp)) +
#       guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))
#
#
#     intDFcl <- as.data.frame(Int)
#     pppcl <- ggplot(intDFcl) + geom_tile(aes(x = 0, y = 0, fill = Int), size = -1) +
#       scale_fill_gradientn(name = "Interaction\nStrength", colors = intPal, limits=c(minInt, maxInt)) +
#       guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))
#
#
#     # Group clusters
#     groupV <- as.vector(group)
#     fillCols <- c("yellow", "red", "blue", "black","purple",
#                   "orange", "pink", "green", "red" , "blue")
#     colCluster <- fillCols[group]
#     colCluster <- as.vector(colCluster)
#     pcl <- pcl + geom_encircle(aes(group = groupV),
#                                spread=0.01,
#                                alpha = 0.2,
#                                expand = 0.03,
#                                fill = colCluster)
#
#
#     # Grab the legends using cowplot::get_legend()
#     pcl2_legend <- get_legend(ppcl)
#     pcl3_legend <- get_legend(pppcl)
#
#     # Combine the legends one on top of the other
#     legendsCl <- plot_grid(pcl2_legend, pcl3_legend, ncol = 1, nrow = 2)
#
#     # Combine the heatmap with the legends
#     endPlotCl <- plot_grid(pcl, legendsCl, ncol = 2, align = "h",
#                            scale = c(1, 0.8), rel_widths = c(0.9, 0.1))
#
#
#     return(endPlotCl)
#   }else{
#     p <- ggnet2(net.sp, mode = l,
#                 size = 0,
#                 edge.size = edgeW,
#                 edge.label = edgeL,
#                 edge.color = edgeCols) +
#       theme(legend.text = element_text(size = 10)) +
#       geom_label(aes(label = nam),nudge_y = labelNudge) +
#       geom_point(aes(fill = Imp1), size = Imp*2, colour = "transparent", shape = 21) +
#       scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits=c(minImp, maxImp)) +
#       theme(legend.position = "none")
#
#
#
#
#     pp <-  ggnet2(net.sp, mode = l,
#                   size = 0,
#                   edge.size = edgeW,
#                   edge.label = edgeL,
#                   edge.color = edgeCols) +
#       theme(legend.text = element_text(size = 10)) +
#       geom_label(aes(label = nam),nudge_y = labelNudge) +
#       geom_point(aes(fill = Imp1), size = Imp*2, colour = "transparent", shape = 21) +
#       scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits=c(minImp, maxImp)) +
#       guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))
#
#     intDF <- as.data.frame(Int)
#     ppp <- ggplot(intDF) + geom_tile(aes(x = 0, y = 0, fill = Int), size = -1) +
#       scale_fill_gradientn(name = "Interaction\nStrength", colors = intPal, limits=c(minInt, maxInt)) +
#       guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))
#
#
#     # Grab the legends using cowplot::get_legend()
#     p2_legend <- get_legend(pp)
#     p3_legend <- get_legend(ppp)
#
#     # Combine the legends one on top of the other
#     legends <- plot_grid(p2_legend, p3_legend, ncol = 1, nrow = 2)
#
#     # Combine the network with the legends
#     endPlot <- plot_grid(p, legends, ncol = 2, align = "h",
#                          scale = c(1, 0.8), rel_widths = c(0.9, 0.1))
#
#     return(endPlot)
#
#   }
# }

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

plotNet <- function(dinteraction,
                    model,
                    thresholdValue = 0,
                    label,
                    fitlimsInt = NULL,
                    fitlimsImp = NULL,
                    intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                    impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                    labelNudge = 0.05,
                    layout = "circle",
                    cluster = F,
                    clusterType = cluster_optimal,
                    clusterLayout =  layout_with_fr,
                    ...){


  # Get importance values
  Imp <- diag(dinteraction)
  Imp1 <-  Imp
  impWarn <- Imp
  impy <- Imp
  impWarn <- max(impWarn)
  impLegend <- Imp
  impLegend <- round(impLegend, 2)
  impLegend <- max(impLegend)+0.005
  minimumImp <- floor(min(Imp))

  # Sort interaction values
  sortInt = t(dinteraction)[lower.tri(t(dinteraction), diag=FALSE)]  # get upper triangle of the matrix by row order
  sorted_Int <- sort(sortInt, index.return=TRUE)                     # Sort values whilst preserving the index
  Int <- sorted_Int$x
  maximumInt <- max(Int)
  nam <- colnames(dinteraction)                                      # Get feature names



  # Limits ------------------------------------------------------------------

  # max min Int vals
  #intValues <- lower.tri(dinteraction)
  minInteraction <- min(as.dist(dinteraction))
  maximumInt <- max(as.dist(dinteraction))+0.01
  maximumInt <- ceiling(maximumInt*100)/100
  # max min Imp vals
  vImportance <- diag(dinteraction)
  maxImportance <- max(vImportance)
  minImportance <-  min(vImportance)

  if(is.null(fitlimsInt)){
    limitsInt <- c(minInteraction, maximumInt)
  }else {
    limitsInt <- fitlimsInt
  }

  if(is.null(fitlimsImp)){
    limitsImp <- c(minImportance, maxImportance)
  }else {
    limitsImp <- fitlimsImp
  }



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
  E(net.bg)$weight <- Int
  Imp <- (5-1)*((Imp-min(Imp))/(max(Imp)-min(Imp)))+1 # scale between 1-5
  Imp  <- round(Imp,2)

  # Set the edge colours
  # colfunction <- intPal
  # edgeColour <- (E(net.bg)$weight)
  # cut_int <- cut(edgeColour, 10)
  #if(is.null(maxInt)){
  if(is.null(fitlimsInt)){
    colfunction <- intPal #col palette
    edgeColour <- (E(net.bg)$weight) # edge weights
    cut_int <- cut(edgeColour, 10) # cutt
    npal <- colfunction[1:length(edgeColour)]
    edgeCols <- npal[cut_int]
    print(edgeCols)
   }else{

     #colfunction <- intPal #col palette
     edgeColour <- (E(net.bg)$weight) # edge weights
     ## Use n equally spaced breaks to assign each value to n-1 equal sized bins
     ii <- cut(edgeColour, breaks = seq(min(fitlimsInt), max(fitlimsInt), len = 10),
               include.lowest = TRUE)
     print(ii)
     print("STOP")
     ## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
     #colfunction <- colfunction(99)[ii] #col palette
     #colfunction <- colorRampPalette(c("lightblue", "blue"))(20)[ii]
     colfunction <- intPal[ii]
     print(colfunction)

     #npal <- colfunction[1:length(edgeColour)]
     #print(npal)
     edgeCols <-colfunction #npal#[cut_int]

     # mI <- max(fitlimsInt)
     # mI <- mI - maximumInt
     # nC <- 11 + mI
     #
     # mIn <- min(fitlimsInt)
     # mIn <- mIn - minInteraction
     # nCn <- 11 + mIn
     #
     # nCol <- nC + nCn
     #
     # intPal_2 <-  rev(sequential_hcl(palette = "Blues 3", n = nCol))
     # colfunction <- intPal
     # #intPal_2  <- intPal[(1 + nC):(length(colfunction) + nC)]
     # #colfunction <- intPal_2
     #edgeColour <- (E(net.bg)$weight)
    # cut_int <- cut(edgeColour, 10)
     # npal <- colfunction[1:length(edgeColour)]
     # edgeCols <- npal[cut_int]
   }

 #edgeCols <- npal[cut_int]
 # print(colfunction)
 # print(cut_int)
 # print(npal)
 # print(edgeCols)
  # edgeCols <- npal[cut_int]
  # print(npal)
  # print(edgeCols)

  # Get edge weights
  weightDF <- get.data.frame(net.bg) # get df of graph attributes
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
  edgeL <- weightDF$weight  # select edge weight
  edgeW <- (5-1)*((edgeWidth1-min(edgeWidth1))/(max(edgeWidth1)-min(edgeWidth1)))+1 # scale between 1-5
  }




  l <- layout
  # min/max legend values:
  # if(is.null(maxInt)){
  #   maxInt <- maximumInt
  # }else{maxInt <- maxInt}
  #
  # if(is.null(maxImp)){
  #   maxImp <- impLegend
  # }else{maxImp <- maxImp}
  #
  # if(is.null(minImp)){
  #   minImp <- minimumImp
  # }else{minImp <- minImp}


  # Whether to show edge label
  if(label == T){
    edgeL <- edgeL
    edgeL <- round(edgeL, 3)
  }else{edgeL <- NULL}


  intMatrix <- round(dinteraction, 3)


  # PLOTTING ----------------------------------------------------------------
  #-----------------------------------------------------------------------

  if(cluster){

    l_1 <- clusterLayout(net.sp)
    #l_1 <- layout_in_circle(net.sp)
    com <- clusterType(net.sp)
    V(net.sp)$color <- com$membership
    group <- V(net.sp)$color
    group <- factor(group)


    #g <- set_graph_attr(net.sp, "layout", layout_in_circle(net.sp))
    colrs <- adjustcolor( c("yellow", "red", "blue", "black","purple",
                            "orange", "pink", "green",
                            "yellow", "red", "blue", "black","purple",
                            "orange", "pink", "green"))
    colorC <- colrs[group]


    pcl <- ggnet2(net.sp,
                  mode = l_1,
                  size = 0,
                  edge.size = edgeW,
                  edge.label = edgeL,
                  edge.color = edgeCols) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam),nudge_y = labelNudge) +
      geom_point(aes(fill = Imp1), size = Imp*2, col = "transparent", shape = 21) +
      scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits = limitsImp) +
      theme(legend.position = "none")


    ppcl <- ggnet2(net.sp,
                   mode = l_1,
                   size = 0,
                   edge.size = edgeW,
                   edge.label = edgeL,
                   edge.color = edgeCols,
                   palette = intPal) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam),nudge_y = labelNudge) +
      geom_point(aes(fill = Imp1), size = Imp*2, col = "transparent", shape = 21) +
      scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits = limitsImp) +
      guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))


    intDFcl <- as.data.frame(Int)
    pppcl <- ggplot(intDFcl) + geom_tile(aes(x = 0, y = 0, fill = Int), size = -1) +
      scale_fill_gradientn(name = "Interaction\nStrength", colors = intPal, limits = limitsInt) +
      guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))


    # Group clusters
    groupV <- as.vector(group)
    fillCols <- c("yellow", "red", "blue", "black","purple",
                  "orange", "pink", "green",
                  "yellow", "red", "blue", "black","purple",
                  "orange", "pink", "green")
    colCluster <- fillCols[group]
    colCluster <- as.vector(colCluster)
    pcl <- pcl + geom_encircle(aes(group = groupV),
                               spread=0.01,
                               alpha = 0.2,
                               expand = 0.03,
                               fill = colCluster)


    # Grab the legends using cowplot::get_legend()
    pcl2_legend <- get_legend(ppcl)
    pcl3_legend <- get_legend(pppcl)

    # Combine the legends one on top of the other
    legendsCl <- plot_grid(pcl2_legend, pcl3_legend, ncol = 1, nrow = 2)

    # Combine the heatmap with the legends
    endPlotCl <- plot_grid(pcl, legendsCl, ncol = 2, align = "h",
                           scale = c(1, 0.8), rel_widths = c(0.9, 0.1))


    return(endPlotCl)
  }else{
    p <- ggnet2(net.sp,
                mode = l,
                size = 0,
                edge.size = edgeW,
                edge.label = edgeL,
                edge.color = edgeCols)+
                #palette = intPal) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam),nudge_y = labelNudge) +
      geom_point(aes(fill = Imp1), size = Imp*2, colour = "transparent", shape = 21) +
      scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits = limitsImp) +
      theme(legend.position = "none")




    pp <-  ggnet2(net.sp,
                  mode = l,
                  size = 0,
                  edge.size = edgeW,
                  edge.label = edgeL,
                  edge.color = edgeCols) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam),nudge_y = labelNudge) +
      geom_point(aes(fill = Imp1), size = Imp*2, colour = "transparent", shape = 21) +
      scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits = limitsImp) +
      guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))

    intDF <- as.data.frame(Int)

    ppp <- ggplot(intDF) + geom_tile(aes(x = 0, y = 0, fill = Int), size = -1) +
      scale_fill_gradientn(name = "Interaction\nStrength", colors = intPal, limits = limitsInt) +
      guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))


    # Grab the legends using cowplot::get_legend()
    p2_legend <- get_legend(pp)
    p3_legend <- get_legend(ppp)

    # Combine the legends one on top of the other
    legends <- plot_grid(p2_legend, p3_legend, ncol = 1, nrow = 2)

    # Combine the network with the legends
    endPlot <- plot_grid(p, legends, ncol = 2, align = "h",
                         scale = c(1, 0.8), rel_widths = c(0.9, 0.1))

    return(endPlot)

  }
}


