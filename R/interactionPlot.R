#' interactionPlot
#'
#' @description Plots a selected variable against all other variables in a model
#'
#' @param model A machine learning model created from mlr3 task and learner.
#' @param type The type of plot to display, either "lollipop" (default) or "barplot".
#' @param interactionType Are measures based on Friedman's H statistic ("H") or on "ice" curves?
#' @param pal A vector of colors to show values, for use with scale_fill_gradientn
#' @param fitlims Specifies the fit range for the color map for interaction strength.
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param normalize Should the variances explained be normalized? Default is FALSE.
#' @param n_max Maximum number of data rows to consider.
#' @param seed An integer random seed used for subsampling.
#' @param sqrt In order to reproduce Friedman's H statistic, resulting values are root transformed. Set to FALSE if squared values should be returned.
#' @param title A title to be placed on the plot
#' @param label Only compatible with plotType = "barplot". If TRUE then the interaction value is displayed at the end of each bar.

#'
#' @importFrom flashlight "flashlight"
#' @importFrom flashlight "light_interaction"
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
#' aq_Task <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn <- lrn("regr.ranger", importance = "permutation")
#' aq_Mod <- aq_lrn$train(aq_Task)
#'
#' # Create plot:
#' interactionPlot(aq_Task, aq_Mod)
#'
#' @export



# Function ----------------------------------------------------------------
interactionPlot <- function(model,
                            data,
                            interactionType = 'ice',
                            pal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                            fitlims = NULL,
                            gridSize = 10,
                            normalize = FALSE,
                            n_max = 1000,
                            seed = NULL,
                            sqrt = FALSE,
                            type = "lollipop",
                            title = NULL,
                            label = FALSE){

  # Get data
  # get data:
  data <-  data
  data <- as.data.frame(data)
  nam <- model$state$train_task$feature_names

  # reordering mlr3 data order to match OG dataset
  # originalOrder <- task$backend$colnames
  # OG <- originalOrder[1:(length(originalOrder)-1)]
  # OG <- setdiff(OG, task$target_names)
  # data <- data[OG]

  if (!is.null(seed)) {
    set.seed(seed)
  }else{seed = NULL}

  response <- model$state$train_task$target_names
  fl <- flashlight(model = model, data = data, y = response, label = "")
  res <- light_interaction(fl, pairwise = F, type = interactionType, grid_size = gridSize,
                           normalize = normalize, n_max = n_max,
                           seed = seed, sqrt = sqrt)$data

  # reorder for plot
  intVal <- res$value
  intRound <- round(intVal, 3)
  intName <- res$variable
  intDF <- reorder(intName, intVal)
  intDF <- data.frame(intDF)

  minimumInt <- min(intDF)
  maximumInt <- max(intDF)

  if(is.null(title)){
    title = "Overall Interaction Strength"
  }else{title = title}

  if(label == T){
    intRound <- intRound
  }else{intRound <- " "}

  if(is.null(fitlims)){
    limitsInt <- c(minimumInt, maximumInt)
  }else {
    limitsInt <- fitlims
  }


  # Barplot
  if (type == "barplot"){
  p <-  ggplot(intDF, aes(x = intDF, y = intDF)) +
    geom_col(aes(fill = intVal)) +
      scale_fill_gradientn(colors = pal, limits = limitsInt) +
    # scale_fill_gradient2(low = "floralwhite",
    #                      high = "dodgerblue4") +
    # ggtitle(label = title) +
    geom_text(aes(label = intRound), vjust = 1.6, color = "black", size = 3.5) +
    theme_minimal() +
    xlab('Feature') +
    ylab("Interaction\nStrength") +
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))+
    coord_flip() +
    guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))
  p <- p + labs(fill = "Interaction\nStrength")
  return(p)
  }else if(type == "lollipop"){
  ppp <- ggplot(intDF, aes(x=intDF, y=intDF)) +
    geom_linerange(ymin=0, aes(ymax=intDF)) + geom_point()+
    xlab('Features') +
    ylab("Importance Value") +
    theme_bw() +
    coord_flip()
  return(ppp)}
}

