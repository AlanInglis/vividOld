#' intHeatmap
#'
#' @description Plots a Heatmap-tyle display showingVariable Importance and Variable Interaction
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param iml If TRUE then agnostic variable importance measures are generated.
#' @param plotly If TRUE then an interactive plot is displayed.
#' @param intLow Colour, set by the user, to display low interaction strengths.
#' @param intHigh Colour, set by the user, to display high interaction strengths.
#' @param impLow Colour, set by the user, to display low importance values.
#' @param impHigh Colour, set by the user, to display high importance values.
#' @param top Returns the first part of the interaction matrix and resulting plot. Similar to head() function.
#' @param reorder If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances
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
#' @importFrom ggnewscale "new_scale_fill"
#' @importFrom plotly "ggplotly"
#' @importFrom tibble "as_tibble"
#' @importFrom dplyr "mutate"
#' @importFrom tidyr "pivot_longer"
#' @importFrom reshape "melt"
#' @importFrom stats "reorder"
#' @importFrom stats "as.dist"
#' @importFrom utils "globalVariables"
#' @import DendSer
#' @importFrom utils "globalVariables"
#' @importFrom DendSer "dser"
#' @import progress
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
#' intHeatmap(aqRgrTask, aqMod, method = "randomForest_importance", interact = F)
#'
#' @export



# Heatmap Plotting Function -------------------------------------------------------

intHeatmap <- function(task, model, iml = FALSE,
                       plotly = FALSE, intLow = "floralwhite", intHigh = "dodgerblue4",
                       impLow = "white", impHigh = "firebrick1", top = NULL, reorder=TRUE,...)
  {
  message(" Calculating variable importance...")
  #dint <- prepPlotly(task, model, method = method)
  dint <- prepHeatmap(task, model, iml)

  #top <- max(top)

  if (is.null(top)) {
    top <- length(getTaskFeatureNames(task))
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
    # give equal weight to both interaction and varimp
    o <- dser( -as.dist(vimp/maxvimp+ dint/maxinteraction), cost=costLPL)
    dint <- dint[o,o]
  }
  dint <- dint[1:top,1:top]

  if(plotly){
    plotlyPlot(dint, intLow=intLow, intHigh=intHigh, impLow=impLow, impHigh=impHigh,...)

  }else{plotHeatmap(dint, intLow=intLow, intHigh=intHigh, impLow=impLow, impHigh=impHigh,...)
  }
}




# PLOT FUNCTION -----------------------------------------------------------
# -------------------------------------------------------------------------


plotHeatmap <- function(dinteraction,
                        plotly = FALSE, intLow = "floralwhite", intHigh = "dodgerblue4",
                        impLow = "white", impHigh = "firebrick1", top = NULL ,title="",...){


  maximumInt <- max(as.dist(dinteraction))+0.01
  maximumInt <- ceiling(maximumInt*100)/100

  yimpMax <- max(diag(dinteraction))+1
  labelNames <- colnames(dinteraction)

  #set values below zero to = zero:
  dinteraction[dinteraction<0] <- 0
  nvar <- nrow(dinteraction)
  index <- 1:nvar

  # Set up plot -------------------------------------------------------

  var_int = dinteraction %>% as_tibble %>%
    mutate(var_num1 = index) %>%
    pivot_longer(cols = index,
                 values_to = 'Interaction\nStrength') %>%
    mutate(var_num2 = rep(index, nvar),
           alpha_imp = as.integer(var_num1 == var_num2),
           alpha_int = 1 - alpha_imp,
           `Variable\nImportance` = alpha_imp*`Interaction\nStrength`,
           `Interaction\nStrength` = alpha_int*`Interaction\nStrength`)


  # Create Plot: ------------------------------------------------------------

  p <- ggplot(data = var_int,
              mapping = aes(x = var_num1, y = var_num2)) +
    scale_x_continuous(breaks = index, labels = labelNames, position = "top") +
    scale_y_reverse(breaks = index, labels = labelNames) +
    geom_raster(aes(fill = `Interaction\nStrength`),
                alpha = var_int$alpha_int) +
    scale_fill_gradient(low = intLow, high = intHigh, limits=c(0, maximumInt)) +
    new_scale_fill() +
    geom_raster(aes(fill = `Variable\nImportance`),
                alpha = var_int$alpha_imp) +
    scale_fill_gradient(low = impLow ,high = impHigh, limits=c(0, yimpMax)) +
    ggtitle(title) +
    xlab('') +
    ylab('') +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  p
}


# PLOT PLOTLY FUNCTION ---------------------------------------------------------
# ------------------------------------------------------------------------------

plotlyPlot <- function(dinteraction,
                       plotly = FALSE, intLow = "floralwhite", intHigh = "dodgerblue4",
                       impLow = "white", impHigh = "firebrick1", top = NULL, title="",...){

  maximumInt <- max(as.dist(dinteraction))+0.01
  maximumInt <- ceiling(maximumInt*100)/100


  yimpMax <- max(diag(dinteraction))+1
  labelNames <- colnames(dinteraction)
  #set values below zero to = zero:
  dinteraction[dinteraction<0] <- 0
  nvar <- nrow(dinteraction)
  index <- 1:nvar

  # Set up plot -------------------------------------------------------

  var_int = dinteraction %>% as_tibble %>%
    mutate(var_num1 = index) %>%
    pivot_longer(cols = index,
                 values_to = 'Interaction\nStrength') %>%
    mutate(var_num2 = rep(index, nvar),
           alpha_imp = as.integer(var_num1 == var_num2),
           alpha_int = 1 - alpha_imp,
           `Variable\nImportance` = alpha_imp*`Interaction\nStrength`,
           `Interaction\nStrength` = alpha_int*`Interaction\nStrength`)

  # Interactive Plot --------------------------------------------------------
  # This plot is only called for plotly
  pp <- ggplot(data = var_int,
               mapping = aes(x = var_num1, y = var_num2)) +
    scale_x_continuous(breaks = index, labels = labelNames, position = "top") +
    scale_y_reverse(breaks = index, labels = labelNames) +
    geom_tile(aes(fill = `Interaction\nStrength`),alpha = var_int$alpha_int) +
    scale_fill_gradient(low = intLow, high = intHigh, limits=c(0, maximumInt)) +
    geom_point(aes(colour = `Variable\nImportance`), size = 10,
               alpha = var_int$alpha_imp) +
    scale_colour_gradient(low = impLow ,high = impHigh, limits=c(0, yimpMax)) +
    xlab('') +
    ylab('') +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  #Interactive plot using plotly
  ppp <- ggplotly(pp)
  ppp

}

# PREP FUNCTION -----------------------------------------------------------

prepHeatmap <- function(task, model, iml){
  data <- getTaskData(task)

  # Get Importance Measures -------------------------------------------------

  if(iml){
    mod  <- Predictor$new(model, data)
    imp <- FeatureImp$new(mod, loss = "mse")
    yImp <- imp$results$importance
    ovars <- imp$results$feature
  }else{
    imp <- getFeatureImportance(model)
    imp <- imp$res
    suppressMessages({
      imp <- melt(imp)
    })
    yImp<-  imp$value
  }

  mod  <- Predictor$new(model, data)
  ovars <- getTaskFeatureNames(task)


  # Create progress bar
  pb <- progress_bar$new(
    format = "  Calculating variable interactions...[:bar]:percent. Estimated completion time::eta ",
    total = length(ovars),
    clear = FALSE)

  res  <- NULL
  for (i in 1:length(ovars)){
    res <- rbind(res, Interaction$new(mod,  grid.size = 10, feature=ovars[i])$results)
    pb$tick()
  }
  res[[".feature"]] <- reorder(res[[".feature"]], res[[".interaction"]])

  vars2 <- t(simplify2array(strsplit(as.character(res[[".feature"]]),":"))) # split/get feature names
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[[".interaction"]]                              # set values
  dinteraction <- (dinteraction+t(dinteraction))/2   # avg over values to make symmetrical

  diag(dinteraction)<- yImp
  dinteraction
}

