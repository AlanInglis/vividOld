#' importancePlot
#'
#' @description This function is used to plot just the variable importance.
#'
#' @param mat A matrix of values to be plotted. Either added by the user or created using the prepFunc() function.
#' @param type The type of plot to display, either "lollipop" (default), or "barplot".
#' @param pal A vector of colors to show predictions, for use with scale_fill_gradientn
#' @param fitlims Specifies the fit range for the color map for importance.
#' @param top A value set by the user to only display the top x amount variables.
#' @param label Only compatible with plotType = "barplot". If TRUE then the importance value is displayed at the end of each bar.
#' @param ... Not currently implemented
#'
#'
#' @importFrom ggplot2 "ggplot"
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
#' plot(myMat, type = "importance")



# Function ----------------------------------------------------------------

importancePlot <- function(mat,
                           plotType = "lollipop",
                           pal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                           fitlims = NULL,
                           top = NULL,
                           label, ...){


  plotImportance(mat,
                 plotType = plotType,
                 pal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                 fitlims = fitlims,
                 top = top,
                 label,...)
}





plotImportance <- function(mat,
                           plotType = "lollipop",
                           pal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                           fitlims = NULL,
                           top = NULL,
                           label,...){

  # # Get Importance Measures -------------------------------------------------
  mat <- mat
  yImp <- diag(mat)

  yImpRound <- round(yImp, 2)
  if(label == T){
    yImpRound <- yImpRound
  }else{yImpRound <- " "}

  maximumImp <- max(yImp)
  minimumImp <- min(yImp)
  midImp <- median(yImp)
  nam <- names(yImp)

  yDF <- reorder(nam, yImp)
  yDF <- data.frame(yDF)


  if (is.null(top)) {
    top <- 0
  }else{top <- top}

  # Show only top X results
  if(top > 0){
    yDF <-  yDF[tail(order(yDF$yDF), top), ]
    yDF <- as.data.frame(yDF)
    yImp <- sort(yImp, decreasing = T)
    yImp <- head(yImp, top)
    yImp <- sort(yImp)
    if(label == T){
      yImpRound <- round(yImp, 2)
    }else{yImpRound <- " "}
  }



  # set limits
  if(is.null(fitlims)){
    limitsImp <- c(minimumImp, maximumImp)
  }else {
    limitsImp <- fitlims
  }

  # Barplot
  if(plotType == "barplot"){

    # Warning message:
    # if(maxImp < maximumImp){
    #   message("Warning: Maximum chosen importance value is less than
    #         some of the importance values. Some values may not be displayed correctly")
    # }
    # if(minImp > minimumImp){
    #   message("Warning: Minimum chosen importance value is greater than
    #         some of the importance values. Some values may not be displayed correctly")
    # }

    p <- ggplot(yDF, aes(x = yDF, y = yImp)) +
      geom_col(aes(fill = yImp)) +
      scale_fill_gradientn(colors = pal, limits = limitsImp) +
      ggtitle(label = "Variable Importance") +
      geom_text(aes(label = yImpRound), vjust = 1.6, color = "black", size = 3.5)+
      theme_minimal() +
      xlab('Features') +
      ylab("Importance Value") +
      theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
      coord_flip() +
      guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))
    p <- p + labs(fill = "Variable\nImportance")
    return(p)
  }else if(plotType == "lollipop"){
      # lollipop plot
      ppp <- ggplot( yDF, aes(x=yDF, y=yImp)) +
        geom_linerange(ymin=0, aes(ymax=yImp)) + geom_point()+
        xlab('Features') +
        ylab("Importance Value") +
        theme_bw() +
        coord_flip()
      return(ppp)}
}
