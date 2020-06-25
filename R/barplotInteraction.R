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
#' @importFrom iml "Predictor"
#' @importFrom iml "Interaction"
#' @importFrom ggplot2 "ggplot"
#'
#'@examples
#' # Load in the data:
#' aq <- data.frame(airquality)
#' aq <- na.omit(aq)
#'
#' # Run an mlr ranger model:
#' library(mlr3)
#' aq_Task = TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn = lrn("regr.ranger", importance = "permutation")
#' aq_Mod <- lrn$train(aq_Task)
#'
#' # Create plot:
#' interactionPlot(aq_Task, aq_Mod)
#'
#' @export



# Function ----------------------------------------------------------------
interactionPlot <- function(task, model, type = "lollipop"){

  # Get data
  # get data:
  data <-  task$data()
  data <- as.data.frame(data)
  target <- task$target_names
  nam <- task$feature_names

  # Get values
  preMod  <- Predictor$new(model, data = data, y = target)

  intValues <- Interaction$new(preMod)
  intVal <- intValues$results$.interaction
  intRound <- round(intVal, 3)
  intDF <- reorder(nam, intVal)
  intDF <- data.frame(intDF)


  # Barplot
  if (type == "barplot"){
  p <-  ggplot(intDF, aes(x = intDF, y = intVal)) +
    geom_col(aes(fill = intVal)) +
    scale_fill_gradient2(low = "floralwhite",
                         high = "dodgerblue4") +
    ggtitle(label = "Overall Interaction Strength") +
    geom_text(aes(label = intRound), vjust = 1.6, color = "black", size = 3.5) +
    theme_minimal() +
    xlab('Feature') +
    ylab("Interaction\nStrength") +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5))+
    coord_flip()
  p <- p + labs(fill = "Interaction\nStrength")
  return(p)
  }else if(type == "circleBar"){
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
    return(pp)
   }else if(type == "lollipop"){# Lollipop
  ppp  <- plot(Interaction$new(preMod)) +
    theme_bw()
  return(ppp)}
}

