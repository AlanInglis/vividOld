## This Script displays just variable importance

#' importancePlot
#'
#' @description Plots variable importance
#'
#' @param mat A matrix of values to be plotted. Either added by the user or created using the prepFunc() function.
#' @param type The type of plot to display, either "lollipop" (default), "barplot", or "circleBar".
#' @param minImp Minimum importance value to be displayed on the legend.
#' @param maxImp Maximum importance value to be displayed on the legend.
#' @param ... Not currently implemented
#'
#'
#' @importFrom ggplot2 "ggplot"
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
#' # Create matrix
#' myMat <- prepFunc(task = aq_Task, learner = aq_lrn, model = aq_Mod)
#'
#'
#' # Create plot:
#' importancePlot(myMat)
#'
#' @export



# Function ----------------------------------------------------------------



importancePlot <- function(mat, type = "lollipop", minImp = NULL, maxImp = NULL, ...){


# # Get Importance Measures -------------------------------------------------


yImp <- diag(mat)

yImpRound <- round(yImp, 2)
maximumImp <- max(yImp)
minimumImp <- min(yImp)
midImp <- median(yImp)
nam <- names(yImp)

yDF <- reorder(nam, yImp)
yDF <- data.frame(yDF)

if(is.null(maxImp)){
  maxImp <- maximumImp
}else{maxImp <- maxImp}

if(is.null(minImp)){
  minImp <- minimumImp
}else{minImp <- minImp}

# Barplot
if(type == "barplot"){

  # Warning message:
  if(maxImp < maximumImp){
    message("Warning: Maximum chosen importance value is less than
            some of the importance values. Some values may not be displayed correctly")
  }
  if(minImp > minimumImp){
    message("Warning: Minimum chosen importance value is greater than
            some of the importance values. Some values may not be displayed correctly")
  }

p <- ggplot(yDF, aes(x = yDF, y = yImp)) +
  geom_col(aes(fill = yImp)) +
  scale_fill_gradient2(low = "dodgerblue4",
                       mid = "white",
                       high = "firebrick1",
                       midpoint = 0,
                        limits = c(minImp, maxImp)) +
  ggtitle(label = "Variable Importance") +
  geom_text(aes(label = yImpRound), vjust = 1.6, color = "black", size = 3.5)+
  theme_minimal() +
  xlab('Features') +
  ylab("Importance Value") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  coord_flip()
p <- p + labs(fill = "Variable\nImportance")
return(p)
}else if(type == "circleBar"){
# Circular barplot

  pp <- ggplot(yDF, aes(x=yDF, y=yImp)) +
    geom_col(aes(fill = yImp)) +
    scale_fill_gradient2(low = "white",
                         high = "firebrick1") +
    ylim(-1,max(yImp)) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = c(0.9,0.5), #"none",
      plot.margin=unit(c(0,-2,0,0), "cm")) + # remove the margin and centre plot
    coord_polar(start = 0) + # This makes the coordinate polar instead of cartesian.
    geom_text(aes(label = nam), vjust = 1.6, color = "black", size = 3.5) +
    geom_text(aes(label = yImpRound),vjust = 4, color = "black", size = 3)
  pp <- pp + labs(fill = "Variable\nImportance") #+ ggtitle(label = "Variable Importance")
return(pp)}else if(type == "lollipop"){

  # lollipop plot
  ppp <- ggplot( yDF, aes(x=yDF, y=yImp)) +
    geom_linerange(ymin=0, aes(ymax=yImp)) + geom_point()+
    xlab('Features') +
    ylab("Importance Value") +
    theme_bw() +
    coord_flip()
return(ppp)}
}








