#' plotHeat
#'
#' @description Plots a Heatmap-tyle display showingVariable Importance and Variable Interaction
#'
#' @param mat A matrix of values to be plotted. Either added by the user or created using the prepFunc() function.
#' @param plotly If TRUE then an interactive plot is displayed.
#' @param intLow Colour, set by the user, to display low interaction strengths.
#' @param intHigh Colour, set by the user, to display high interaction strengths.
#' @param impLow Colour, set by the user, to display low importance values.
#' @param impHigh Colour, set by the user, to display high importance values.
#' @param top Returns the first part of the interaction matrix and resulting plot. Similar to head() function.
#' @param reorder If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances
#' @param minInt Minimum interaction strength to be displayed on the legend.
#' @param maxInt Maximum interaction strength to be displayed on the legend.
#' @param minImp Minimum importance value to be displayed on the legend.
#' @param maxImp Maximum importance value to be displayed on the legend.
#' @param ... Not currently implemented.
#'
#' @importFrom ggplot2 "ggplot"
#' @importFrom ggnewscale "new_scale_fill"
#' @importFrom plotly "ggplotly"
#' @importFrom dplyr "as_tibble"
#' @importFrom dplyr "mutate"
#' @importFrom tidyr "pivot_longer"
#' @importFrom reshape "melt"
#' @importFrom stats "reorder"
#' @importFrom stats "as.dist"
#' @importFrom utils "globalVariables"
#' @import DendSer
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
#' plot(myMat, type = "heatMap")


# PLOT FUNCTION -----------------------------------------------------------
# -------------------------------------------------------------------------


plotHeat <- function(dinteraction,
                     plotly = FALSE, intLow = "floralwhite", intHigh = "dodgerblue4",
                     impLow = "white", impHigh = "firebrick1", top = NULL , title="",
                     minImp = NULL, maxImp = NULL, minInt = 0, maxInt = NULL,...){



  maximumInt <- max(as.dist(dinteraction))+0.01
  maximumInt <- ceiling(maximumInt*100)/100

  maximumImp <- max(diag(dinteraction))+1
  labelNames <- colnames(dinteraction)

  #set values below zero to = zero:
  dinteraction[dinteraction < 1.0e-5] <- 0
  nvar <- nrow(dinteraction)
  index <- 1:nvar

  # min/max legend values:
  if(is.null(maxInt)){
    maxInt <- maximumInt
  }else{maxInt <- maxInt}

  if(is.null(maxImp)){
    maxImp <- maximumImp
  }else{maxImp <- maxImp}

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
    scale_fill_gradient(low = intLow, high = intHigh, limits=c(minInt, maxInt)) +
    new_scale_fill() +
    geom_raster(aes(fill = `Variable\nImportance`),
                alpha = var_int$alpha_imp) +
    scale_fill_gradient(low = impLow ,high = impHigh, limits=c(minImp, maxImp)) +
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
                       impLow = "white", impHigh = "firebrick1", top = NULL, title="",
                       minImp = NULL, maxImp = NULL, minInt = 0, maxInt = NULL,...){

  maximumInt <- max(as.dist(dinteraction))+0.01
  maximumInt <- ceiling(maximumInt*100)/100


  maximumImp <- max(diag(dinteraction))+1
  labelNames <- colnames(dinteraction)
  #set values below zero to = zero:
  dinteraction[dinteraction<0] <- 0
  nvar <- nrow(dinteraction)
  index <- 1:nvar

  if(is.null(minImp)){
        minImp <- 0
      }else{minImp <- minImp}

      if(is.null(maxInt)){
        maxInt <- maximumInt
      }else{maxInt <- maxInt}

      if(is.null(maxImp)){
        maxImp <- maximumImp
      }else{maxImp <- maxImp}

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
    scale_fill_gradient(low = intLow, high = intHigh, limits=c(0, maxInt)) +
    geom_point(aes(colour = `Variable\nImportance`), size = 10,
               alpha = var_int$alpha_imp) +
    scale_colour_gradient(low = impLow ,high = impHigh, limits=c(0, maxImp)) +
    xlab('') +
    ylab('') +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  #Interactive plot using plotly
  ppp <- ggplotly(pp)
  ppp

}
