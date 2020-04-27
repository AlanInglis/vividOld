## This Script displays 2-way variable interaction

#' allInt
#'
#' @description Plots a Heatmap-tyle display showingVariable Importance and Variable Interaction
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param type The type of plot to display, either "lollipop", "barplot", or "circleBar"
#' @param top A value set by the user to only display the top x amount variables
#' @param ... Not currently implemented
#'
#'
#' @importFrom mlr "getTaskData"
#' @importFrom mlr "normalizeFeatures"
#' @importFrom mlr "generateFilterValuesData"
#' @importFrom mlr "getTaskFeatureNames"
#' @importFrom iml "Predictor"
#' @importFrom iml "Interaction"
#' @importFrom ggplot2 "ggplot"
#'
#'@examples
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
#' # Create plot:
#' allInt(aqRgrTask, aqMod)
#'
#' @export


# Function ----------------------------------------------------------------

allInt <- function(task, model, type = "lollipop", top = 0, ...){

  data <- getTaskData(task)
  nam <- getTaskFeatureNames(task)

  mod <- Predictor$new(model, data = data)
  res <- NULL
  ovars <- nam
  for (i in 1:length(nam))
    res <- rbind(res, Interaction$new(mod, feature=ovars[i])$results)

  res[[".feature"]]<- reorder(res[[".feature"]], res[[".interaction"]])

  if(top > 0){
    res <- head(res,top)
  }

  # lollipop plot
  p <- ggplot(data=res,aes(x=.feature, y=.interaction)) +
           geom_linerange(ymin=0, aes(ymax=.interaction)) + geom_point()+
    xlab('Features') +
    ylab("Interaction Strength") +
           coord_flip() +
           theme_bw()
  # barplot
  pp <-  ggplot(res, aes(x = .feature, y = .interaction)) +
    geom_col(aes(fill = .interaction)) +
    scale_fill_gradient2(low = "floralwhite",
                         high = "dodgerblue4") +
    theme_minimal() +
    xlab('Features') +
    ylab("Interaction Strength") +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
    coord_flip()

  # Circle barplot
  ppp <- ggplot(res, aes(x = .feature, y = .interaction)) +
    geom_col(aes(fill = .interaction)) +
    scale_fill_gradient2(low = "floralwhite",
                         high = "dodgerblue4") +
    # Limits of the plot. The negative value controls the size of the inner circle,
    # the positive is to add size over each bar
    ylim(-0.5,0.3) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.margin = unit(rep(-2,4), "cm")) +    # This remove unnecessary margin around plot
    coord_polar(start = 0) + # This makes the coordinate polar instead of cartesian.
    geom_text(aes(label = .feature), vjust = 2, color = "black", size = 3.5)
    #geom_text(aes(label = intRound),vjust = 0, color = "black", size = 3)

  if(type == "barplot"){
  return(pp)
  }else if(type == "lollipop"){
    return(p)
  }else if(type == "circleBar"){
      return(ppp)
  }
}
