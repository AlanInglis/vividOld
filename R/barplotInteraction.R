## This Script displays just variable interaction strength

#' interactionPlot
#'
#' @description Plots a selected variable against all other variables in a model
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param type The type of plot to display, either "lollipop" (default), "barplot", or "circleBar"
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
#' interactionPlot(aqRgrTask, aqMod)
#'
#' @export



# Function ----------------------------------------------------------------
interactionPlot <- function(task, model, type = "lollipop"){

  # Get data
  data <- getTaskData(task)
  nam <- getTaskFeatureNames(task)

  # Get values
  preMod  <- Predictor$new(model, data = data)

  intValues <- Interaction$new(preMod)
  intVal <- intValues$results$.interaction
  intRound <- round(intVal, 3)
  intDF <- reorder(nam, intVal)
  intDF <- data.frame(intDF)


  # Barplot
  p <-  ggplot(intDF, aes(x = intDF, y = intVal)) +
    geom_col(aes(fill = intVal)) +
    scale_fill_gradient2(low = "floralwhite",
                         high = "dodgerblue4") +
    ggtitle(label = "Overall Interaction Strength") +
    geom_text(aes(label = intRound), vjust = 1.6, color = "black", size = 3.5) +
    theme_minimal() +
    xlab('Variable') +
    ylab("Interaction\nStrength") +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5))+
    coord_flip()
  p <- p + labs(fill = "Interaction\nStrength")

  # Circular barplot
    pp <- ggplot(intDF, aes(x = intDF, y = intVal)) +
      geom_col(aes(fill = intVal)) +
      scale_fill_gradient2(low = "floralwhite",
                           high = "dodgerblue4") +
      ylim(-0.5,max(intVal)) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = unit(rep(-2,4), "cm")) +
      coord_polar(start = 0) +
      geom_text(aes(label = nam), vjust = -2, color = "black", size = 3.5) +
      geom_text(aes(label = intRound),vjust = 0, color = "black", size = 3)
    pp <- pp + labs(fill = "Interaction\nStrength")


  # Lollipop
  ppp  <- plot(Interaction$new(preMod)) +
    theme_bw()

  if(type == "barplot"){
    return(p)
  }else if(type == "circleBar"){
    return(pp)
  }else if(type == "lollipop"){
    (return(ppp))
  }
}

