#' vivid: Plot the interaction/importance matrix created from \code{vividMatrix}
#'
#' @description This function is used to create vivid-based plots.
#'
#'
#' @param x An object of class \code{vivid} created via vividMatrix.
#' @param type Type of plot required.
#' @param plotly If TRUE then an interactive plotly heatMap plot is displayed.
#' @param intLow Colour, set by the user, to display low interaction strengths for plotly.
#' @param intHigh Colour, set by the user, to display high interaction strengths for plotly.
#' @param impLow Colour, set by the user, to display low importance values for plotly.
#' @param impHigh Colour, set by the user, to display high importance values for plotly.
#' @param intPal A colorspace colour palette to display the interaction values.
#' @param impPal A colorspace colour palette to display the importance values.
#' @param angle The angle to display the x-axis labels.
#' @param top Returns the first part of the interaction matrix and resulting plot. Similar to head() function.
#' For use with \code{"heatMap"} and \code{"allInteractions"}.
#' @param minInt Minimum interaction strength to be displayed on the legend.
#' @param maxInt Maximum interaction strength to be displayed on the legend.
#' @param minImp Minimum importance value to be displayed on the legend.
#' @param maxImp Maximum importance value to be displayed on the legend.
#' @param thresholdValue A value chosen by the user which will show all the edges with weights (i.e., the interacions) above that value.
#' For example, if thresholdValue = 0.2, then only the the interacions greater than 0.2 will be displayed.
#' For use with \code{"network"}.
#' @param label If label = TRUE the numerical value for the interaction strength will be displayed. For use with \code{"network"}.
#' @param labelNudge A value, set by the user, to determine the y_postioning of the variables names.
#' A higher value will postion the label farther above the nodes. For use with \code{"network"}.
#' @param layout Determines the shape, or layout, of the plotted network graph.
#' @param cluster If cluster = TRUE, then the data is clustered in groups.
#' @param clusterType = Network-based clustering. Any of the appropriate cluster types from the igraph package are allowed.
#' @param clusterLayout = Determines the shape, or layout, of the clustered plotted graph.
#' @param plotType The type of interaction/importance plot to display, either "lollipop", "barplot", or "circleBar".
#' For use with \code{"allInteractions"}, and \code{"importance"}
#' @param ... Not currently implemented.
#'
#' @return A plot chosen from the type argument.
#'
#' @importFrom ggplot2 "ggplot"
#' @importFrom utils "globalVariables"
#' @import DendSer
#'
#'@examples
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
#'#' # Create matrix
#' myMat <- vividMatrix(task = aq_Task,  model = aq_Mod)
#'
#' # Create plot:
#' plot(myMat, type = "heatMap")
#'
#' @export

plot.vivid <- function(x,
                       # for heatmap
                       plotly = FALSE, intLow = "floralwhite", intHigh = "dodgerblue4",
                       impLow = "floralwhite", impHigh = "firebrick1", top = NULL, reorder=TRUE,
                       intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                       impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                       fitlimsInt = NULL,
                       fitlimsImp = NULL,
                       minImp = NULL, maxImp = NULL, minInt = 0, maxInt = NULL, angle = NULL,
                       #for network
                       thresholdValue = 0,
                       label = FALSE,
                       labelNudge = 0.05, layout = "circle",
                       cluster = F,
                       clusterType = cluster_optimal,
                       clusterLayout = layout_with_fr,
                       # for all interactions
                       fitlims = NULL,
                       plotType = "lollipop",
                       type = c("heatMap",
                                "network",
                                "allInteractions",
                                "importance"),...){


  # Get the specified type
  type <- match.arg(type)

  if ('heatMap'%in%type) {

  dint <- x
  if (is.null(top)) {
    top <- length(colnames(dint))
  }
  else {
    top <- max(top)
  }

  dint <- dint[1:top,1:top]

  if(plotly){
    plotlyPlot(dint, intLow=intLow, intHigh=intHigh, impLow=impLow, impHigh=impHigh,...)

  }else{plotHeat(dint,
                 intPal,
                 impPal,
                 fitlimsInt = fitlimsInt,
                 fitlimsImp = fitlimsImp,
                 angle = angle,...)
  }

}else if ('network'%in%type) {
  # netPrep <- x
  # plotNet(netPrep, model, reorder = T, thresholdValue, label, layout = layout,
  #         minInt = minInt, maxInt = maxInt, minImp = minImp , maxImp = maxImp, labelNudge = labelNudge, cluster = cluster, clusterType = clusterType,...)

     netPrep <- x

     plotNet(netPrep,
             model,
             thresholdValue,
             label,
             layout = layout,
             fitlimsInt = fitlimsInt,
             fitlimsImp = fitlimsImp,
             intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
             impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
             labelNudge = labelNudge,
             cluster = cluster,
             clusterType = clusterType,
             clusterLayout = clusterLayout,...)



}else if('allInteractions'%in%type){

  dinteraction <- x
  if (is.null(plotType)) {
    plotType = "lollipop"
  }else{plotType = plotType}
    plotAllInteractions(dinteraction,
                        pal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                        fitlims = fitlims,
                        plotType = plotType,
                        top = top,...)

}else if('importance'%in%type){

    mat <- x
    plotImportance(mat,
                   plotType = plotType,
                   pal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                   fitlims = fitlims,
                   minImp = minImp, maxImp = maxImp,
                   top = top,
                   label, ...)
  }
}






