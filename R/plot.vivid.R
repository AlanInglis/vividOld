#' vivid: Plot the interaction/importance matrix created from \code{vividMatrix}
#'
#' @description This function is used to create vivid-based plots.
#'
#'
#' @param x An object of class \code{vivid} created via vividMatrix.
#' @param type Type of plot required.
#' @param plotly If TRUE then an interactive plotly heatMap plot is displayed.
#' @param intLow Colour, set by the user, to display low interaction strengths.
#' @param intHigh Colour, set by the user, to display high interaction strengths.
#' @param impLow Colour, set by the user, to display low importance values.
#' @param impHigh Colour, set by the user, to display high importance values.
#' @param top Returns the first part of the interaction matrix and resulting plot. Similar to head() function.
#' For use with \code{"heatMap"} and \code{"allInteractions"}.
#' @param reorder If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances.
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
#' aq_Task = TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn = lrn("regr.ranger", importance = "permutation")
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
                       impLow = "white", impHigh = "firebrick1", top = NULL, reorder=TRUE,
                       minImp = NULL, maxImp = NULL, minInt = 0, maxInt = NULL,
                       #for network
                       thresholdValue = 0,
                       label = FALSE,
                       labelNudge = 0.05, layout = "circle",
                       cluster = F,
                       # for all interactions
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

  if (reorder){
    vimp <- diag(dint)
    vimp <- (vimp-min(vimp))/max(vimp) # scale to 0-1 for consistency with interactions
    vimp <- sqrt(outer(vimp, vimp)) # make a matrix

    maxinteraction <- max(as.dist(dint))
    maxvimp <- max(as.dist(vimp))
    intVals <- lower.tri(dint)
    minInteraction <- min(intVals)

    # give equal weight to both interaction and varimp
    o <- dser( -as.dist(vimp/maxvimp+ dint/maxinteraction), cost=costLPL)
    dint <- dint[o,o]
  }


  intValues <- lower.tri(dint)
  minInteraction <- min(intValues)
  vImportance <- diag(dint)
  maxImportance <- max(vImportance)
  minImportance <-  min(vImportance)

  maximumInt <- max(as.dist(dint))+0.01
  maximumInt <- ceiling(maximumInt*100)/100

  if(is.null(minImp)){
    minImp <- minImportance - 2
  }else{minImp <- minImp}

  if(is.null(maxImp)){
    maxImp <- maxImportance + 2
  }else{maxImp <- maxImp}

  if(is.null(maxInt)){
    maxInt <- maximumInt
  }else{maxInt <- maxInt}

  if(is.null(maxImp)){
    maxImp <- maximumImp
  }else{maxImp <- maxImp}

  dint <- dint[1:top,1:top]

  ## Warning messages:
  if(minInt > minInteraction){
    message(" Warning: Minimum chosen interaction value is larger than
            some of the interaction values. These values may not be displayed correctly.
            Adjust minInt to rectify.")
  }
  if(minImp > minImportance){
    message(" Warning: Minimum chosen importance value is larger than
            some of the importance values. These values may not be displayed correctly.
            Adjust minImp to rectify.")
  }

  if(plotly){
    plotlyPlot(dint, intLow=intLow, intHigh=intHigh, impLow=impLow, impHigh=impHigh,...)

  }else{plotHeat(dint, intLow=intLow, intHigh=intHigh, impLow=impLow, impHigh=impHigh,
                 minImp=minImp, maxImp=maxImp, minInt=minInt, maxInt=maxInt,...)
  }

}else if ('network'%in%type) {
  if (reorder){
    netPrep <- x
    vimp <- diag(netPrep)
    vimp <- (vimp-min(vimp))/max(vimp) # scale to 0-1 for consistency with interactions
    vimp <- sqrt(outer(vimp, vimp)) # make a matrix

    maxinteraction <- max(as.dist(netPrep))
    maxvimp <- max(as.dist(vimp))
    intVals <- lower.tri(netPrep)
    minInteraction <- min(intVals)
    minImportance <-  min(vimp)

    # give equal weight to both interaction and varimp
    o <- dser( -as.dist(vimp/maxvimp+ netPrep/maxinteraction), cost=costLPL)
    netPrep <- netPrep[o,o]
  }else{netPrep <- x}
    plotNet(netPrep, model, reorder = T, thresholdValue, label, layout = layout,
            minInt, maxInt, minImp , maxImp, cluster = cluster,...)
}else if('allInteractions'%in%type){

  dinteraction <- x
  if (is.null(plotType)) {
    plotType = "lollipop"
  }else{plotType = plotType}
    plotAllInteractions(dinteraction, plotType = plotType, top = top,...)

}else if('importance'%in%type){

    mat <- x
    plotImportance(mat, plotType = plotType, minImp = minImp, maxImp = maxImp, label, ...)
  }
}
