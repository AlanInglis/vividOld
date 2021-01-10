#' plotHeat
#'
#' @description Plots a Heatmap-tyle display showingVariable Importance and Variable Interaction
#'
#' @param mat A matrix of values to be plotted. Either added by the user or created using the prepFunc() function.
#' @param intPal A colorspace colour palette to display the interaction values.
#' @param impPal A colorspace colour palette to display the importance values.
#' @param plotly If TRUE then an interactive plot is displayed.
#' @param top Returns the first part of the interaction matrix and resulting plot. Similar to head() function.
#' @param minInt Minimum interaction strength to be displayed on the legend.
#' @param maxInt Maximum interaction strength to be displayed on the legend.
#' @param minImp Minimum importance value to be displayed on the legend.
#' @param maxImp Maximum importance value to be displayed on the legend.
#' @param title Adds title to the plot.
#' @param angle The angle to display the x-axis labels.
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
#' @importFrom cowplot "get_legend"
#' @importFrom cowplot "plot_grid"
#' @importFrom colorspace "sequential_hcl"
#'
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
#' plot(myMat, type = "heatMap")


# PLOT FUNCTION -----------------------------------------------------------
# -------------------------------------------------------------------------


plotHeat <- function(dinteraction,
                     plotly = FALSE,
                     top = NULL ,
                     title="",
                     intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                     impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                     minImp = NULL, maxImp = NULL, minInt = NULL, maxInt = NULL,
                     angle = NULL,
                      ...){


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


  labelNames <- colnames(dinteraction)

  #set values below zero to = zero:
  dinteraction[dinteraction < 1.0e-5] <- 0
  nvar <- nrow(dinteraction)
  index <- 1:nvar

  # set x-axis text angle
  if(is.null(angle)){
    angle <- 0
  }else{angle = angle}

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
    geom_tile(aes(fill = `Interaction\nStrength`), alpha = var_int$alpha_int) +
    scale_fill_gradientn(colors = intPal, limits=c(minInt, maxInt)) +
    labs(title = title) +
    new_scale_fill() +
    geom_tile(aes(fill = `Variable\nImportance`), alpha = var_int$alpha_imp) +
    scale_fill_gradientn(colors = impPal, limits=c(minImp, maxImp)) +
    xlab('') +
    ylab('') +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(axis.text  = element_text(size = 10)) +
    theme(axis.text.x  = element_text(angle = angle, hjust = 0)) +
    theme(legend.position = "none")

  pp <- ggplot(data = var_int,
              mapping = aes(x = var_num1, y = var_num2)) +
    guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5)) +
    scale_x_continuous(breaks = index, labels = labelNames, position = "top") +
    scale_y_reverse(breaks = index, labels = labelNames) +
    geom_tile(aes(fill = `Interaction\nStrength`),
                alpha = var_int$alpha_int) +
    scale_fill_gradientn(colors = intPal, limits=c(minInt, maxInt)) +
    labs(title = title)


  ppp <- ggplot(data = var_int,
               mapping = aes(x = var_num1, y = var_num2)) +
    guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5)) +
     geom_tile(aes(fill = `Variable\nImportance`),
                 alpha = var_int$alpha_imp) +
    scale_fill_gradientn(colors = impPal, limits=c(minImp, maxImp)) +
  xlab('') +
  ylab('') +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

  # Grab the legends using cowplot::get_legend()
  p2_legend <- get_legend(pp)
  p3_legend <- get_legend(ppp)

  # Combine the legends one on top of the other
  legends <- plot_grid(p2_legend, p3_legend, ncol = 1, nrow = 2)

  # Combine the heatmap with the legends
  endPlot <- plot_grid(p, legends, ncol = 2, align = "h",
                       scale = c(1, 0.8), rel_widths = c(0.9, 0.1))
  endPlot

}


# PLOT PLOTLY FUNCTION ---------------------------------------------------------
# ------------------------------------------------------------------------------

plotlyPlot <- function(dinteraction,
                       plotly = FALSE, intLow = "floralwhite", intHigh = "dodgerblue4",
                       impLow = "floralwhite", impHigh = "firebrick1", top = NULL, title="",
                       minImp = NULL, maxImp = NULL, minInt = 0, maxInt = NULL,...){

  maximumInt <- max(as.dist(dinteraction))+0.01
  maximumInt <- ceiling(maximumInt*100)/100


  maximumImp <- max(diag(dinteraction))+1
  ImpVal <- diag(dinteraction)
  minimumImp <- min(ImpVal)
  labelNames <- colnames(dinteraction)
  #set values below zero to = zero:
  dinteraction[dinteraction<0] <- 0
  nvar <- nrow(dinteraction)
  index <- 1:nvar

  if(is.null(minImp)){
        minImp <- minimumImp
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
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45))


  #Interactive plot using plotly
  ppp <- ggplotly(pp)
  ppp

}
