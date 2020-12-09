#' allInteractionsPlot
#'
#' @description Plots all 2-way interactions
#'
#'
#' @param mat A matrix of values to be plotted. Either added by the user or created using the prepFunc() function.
#' @param type The type of plot to display, either "lollipop", "barplot", or "circleBar".
#' @param top A value set by the user to only display the top x amount variables.
#' @param ... Not currently implemented
#'
#' @importFrom ggplot2 "ggplot"
#' @importFrom stats "reshape"
#' @import progress
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
#' plot(myMat, type = "allInteractions", plotType = "barplot")
#'



# Function ----------------------------------------------------------------

allInteractionsPlot <- function(mat, plotType = NULL, top = NULL, ...){

  # if(type != "lollipop" || type != "barplot" || type != "circleBar"){
  #   stop("Invalid plotting type. See ?allInt for available plotting types")
  # }

  dinteraction <- mat
  if (is.null(plotType)) {
    plotType = "lollipop"
  }else{plotType = plotType}

  plotAllInteractions(dinteraction, plotType = plotType, top = top)
}

plotAllInteractions <- function(mat, plotType = NULL, top = NULL, ...){
  dinteraction <- mat
  diag(dinteraction) <- 0

  df <- transform(data.frame(dinteraction), y=row.names(dinteraction))

  # Reshape long
  long_df <- reshape(df, varying = colnames(dinteraction), times = colnames(dinteraction),
                     timevar="x", v.names="value", direction="long")

  # Order values
  long_df <- with(long_df, long_df[order(value),])
  long_df$xy <- with(long_df, paste(x, y, sep=":"))

  # Select correct data
  df <- long_df[,c(3,5)]

  # Replace low vales with zero
  df[df < 0.04 ] <- 0

  # Remove zeros
  df<-df[!apply(df[,1:2] == 0, 1, FUN = any, na.rm = TRUE),]


  if (is.null(top)) {
    top <- 0
  }else{top <- top}

  if (is.null(plotType)) {
    plotType = "lollipop"
  }else{plotType = plotType}

  # Show only top X results
  if(top > 0){
    df <- tail(df,top)
  }


  # -------------------------------------------------------------------------



if(plotType != "barplot" && plotType != "lollipop"){
  stop("plotType must be of type barplot or lollipop")
}

  # Plotting
  if(plotType == "barplot"){
    # barplot
    pp <-  ggplot(df, aes(x = reorder(xy, value), y = value)) +
      geom_col(aes(fill = value)) +
      scale_fill_gradient(name = "Interaction\nStrength",
                          low = "floralwhite",
                          high = "dodgerblue4") +
      theme_minimal() +
      xlab('Features ') +
      ylab("Interaction Strength") +
      theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
      coord_flip() +
      guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))
    return(pp)
  }else if(plotType == "lollipop"){
    # lollipop plot
    p <- ggplot(df, aes(x = reorder(xy, value), y = value)) +
      geom_linerange(ymin=0, aes(ymax=value)) + geom_point()+
      xlab('Features') +
      ylab("Interaction Strength") +
      coord_flip() +
      theme_bw()
    return(p)
  }
}
