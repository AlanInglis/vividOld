#' intHeatmap
#'
#' @description Create a Heatmap style plot displaying Variable Importance and Variable Interaction
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param method A list of variable importance methods to be set by the user. These can include any of the importance methods contained within the mlr package. The default is method = randomForest.
#' @param plotly If plotly = TRUE then it displays an interactive plotly plot.
#' @param intLow Colour, set by the user, to display low interaction strengths.
#' @param intHigh Colour, set by the user, to display high interaction strengths.
#' @param impLow Colour, set by the user, to display low importance values.
#' @param impHigh Colour, set by the user, to display high importance values.
#' @param top Returns the first part of the interaction matrix and resulting plot. Similar to head() function.
#' @param ... Not currently implemented
#'
#' @return A heatmap style plot dispaying interaction strength on the off-diagonal and variable importance on the diagonal.
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
<<<<<<< HEAD
#' @importFrom stats "as.dist"
#' @importFrom utils "globalVariables"
#' @import DendSer
=======
#' @importFrom utils "globalVariables"
#' @importFrom DendSer "dser"
>>>>>>> 15cab52c1fdfaaf5150e72da8cbcbf7201a3ba23
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



## Heatmap Plotting Function -------------------------------------------------------

intHeatmap <- function(task, model, method = "randomForest",
                       plotly = FALSE, intLow = "floralwhite", intHigh = "dodgerblue4",
                       impLow = "white", impHigh = "firebrick1", top = 0, ...){

  #asdf

  # get data:
  data <- getTaskData(task)

  # Get Importance Measures -------------------------------------------------

  normTask <- normalizeFeatures(task, method = "standardize")

  impFeat <- generateFilterValuesData(normTask, method = method)
  yImp <- impFeat$data$value
  yimpMax <- max(yImp)+1


  nam <- getTaskFeatureNames(task)

  mod  <- Predictor$new(model, data)
  res  <- NULL
  ovars <- getTaskFeatureNames(task)
  for (i in 1:length(ovars))
    res <- rbind(res, Interaction$new(mod, feature=ovars[i])$results)

  res[[".feature"]] <- reorder(res[[".feature"]], res[[".interaction"]])

  vars2 <- t(simplify2array(strsplit(as.character(res[[".feature"]]),":"))) # split/get feature names
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[[".interaction"]]                              # set values
  dinteraction <- (dinteraction+t(dinteraction))/2                          # avg over values to make symmetrical

  #scale interaction values:
  #dinteraction <- ((dinteraction-min(dinteraction))/(max(dinteraction)-min(dinteraction)))
  maximumInt <- max(dinteraction)+0.01
  maximumInt <- round(maximumInt, 2)
<<<<<<< HEAD

  # Sort matrix so max value is top left
  o <- dser(as.dist(-dinteraction), cost = costLPL)
=======
  # Sort matrix so max value is top left
  o <- dser((-dinteraction))

>>>>>>> 15cab52c1fdfaaf5150e72da8cbcbf7201a3ba23
  #q <- which(colSums(dinteraction == max(dinteraction))>0,arr.ind = T)
  #o <- c(q, seq(ncol(dinteraction))[-q])
  dinteraction <- dinteraction[o,o]

  # Reorder the variable importance on diag to match the new order of plot:
  labelNames <- colnames(dinteraction)
  yImpName <- list(nam)
  matV <- matrix(yImp, dimnames = yImpName)
  matV <- t(matV)

  matV <- matV[, labelNames, drop = FALSE]
  yImp <- matV[1,]

  diag(dinteraction) <- yImp
  dinteraction <- round(dinteraction,2)
  labTitle = method

  #set values below zero to = zero:
  dinteraction[dinteraction<0] <- 0
<<<<<<< HEAD
=======

  # Only show the top X variables:
  if(top > 0){
    dinteraction <- (dinteraction[1:top,1:top])
    labelNames <- colnames(dinteraction[,1:top])
    breakTop <- rep(0, top)
  }else if(top == 0){
    dinteraction <- dinteraction
    labelNames <- colnames(dinteraction)
    breakTop <- rep(0, length(nam))
  }
>>>>>>> 15cab52c1fdfaaf5150e72da8cbcbf7201a3ba23

  # Only show the top X variables:
  if(top > 0){
    dinteraction <- (dinteraction[1:top,1:top])
    labelNames <- colnames(dinteraction[,1:top])
    breakTop <- rep(0, top)
  }else if(top == 0){
    dinteraction <- dinteraction
    labelNames <- colnames(dinteraction)
    breakTop <- rep(0, length(nam))
  }

<<<<<<< HEAD

=======
>>>>>>> 15cab52c1fdfaaf5150e72da8cbcbf7201a3ba23
  # Set up plot -------------------------------------------------------

  var_int2 = dinteraction %>% as_tibble %>%
    mutate(var_num1 = 1:length(breakTop)) %>%
    pivot_longer(cols = 1:length(breakTop),
                 values_to = 'Interaction\nStrength') %>%
    mutate(var_num2 = rep(1:length(breakTop), length(breakTop)),
           alpha_imp = as.integer(var_num1 == var_num2),
           alpha_int = 1 - alpha_imp,
           `Variable\nImportance` = alpha_imp*`Interaction\nStrength`,
           `Interaction\nStrength` = alpha_int*`Interaction\nStrength`)


  # Create Plot: ------------------------------------------------------------


  p <- ggplot(data = var_int2,
              mapping = aes(x = var_num1, y = var_num2)) +
    scale_x_continuous(breaks = 1:length(breakTop), labels = labelNames, position = "top") +
    scale_y_reverse(breaks = 1:length(breakTop), labels = labelNames) +
    geom_raster(aes(fill = `Interaction\nStrength`),
                alpha = var_int2$alpha_int) +
    scale_fill_gradient(low = intLow, high = intHigh, limits=c(0, maximumInt)) +
    new_scale_fill() +
    geom_raster(aes(fill = `Variable\nImportance`),
                alpha = var_int2$alpha_imp) +
    scale_fill_gradient(low = impLow ,high = impHigh, limits=c(0, yimpMax)) +
    ggtitle(labTitle) +
    xlab('') +
    ylab('') +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())


  # for plotly  -------------------------------------------------------------
  dinteraction1 <- data.frame(interaction=as.vector(dinteraction))          # Create df

  dinteraction1$x <- as.vector(row(dinteraction))
  dinteraction1$y <- as.vector(col(dinteraction))
  dinteraction1$varx <- ovars[dinteraction1$x]
  dinteraction1$vary <- ovars[dinteraction1$y]
  dinteraction1$varx <- factor(dinteraction1$varx, levels=ovars)
  dinteraction1$vary <- factor(dinteraction1$vary, levels=rev(ovars))

  # Importance point size:
  if(length(yImp) <= 5)
  {pointSize = 15
  } else if(length(yImp) <= 10)
  {pointSize = 10
  }else if(length(yImp <= 15))
  {pointSize = 7}

  impMat <- matrix(0,nrow=length(yImp), ncol=length(yImp))
  diag(impMat) <- yImp
  importMatrix <- melt(impMat)


  # Interactive Plot --------------------------------------------------------
  # This plot is only called for plotly as newGeom_raster is not supported
  # Importance <- yImp
  # pp <- ggplot(data=dinteraction1, aes(x=varx,y=vary)) +
  #   geom_tile(aes(fill = interaction), dinteraction1 %>% filter(dinteraction1$varx != dinteraction1$vary)) +
  #   geom_point(aes(x=Var1,y= rev(Var2),colour = Importance),size = pointSize, importMatrix %>% filter(importMatrix$Var1==importMatrix$Var2)) +
  #   scale_fill_gradient2(low="white", high="red") +
  #   labs(colour= labTitle) +
  #   labs(fill='Interaction \n\ Strength') +
  #   theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  #   scale_x_discrete(position = "top") + theme_bw()+
  #   xlab("")+ylab("")


  # Interactive plot using plotly
  #ppp <- ggplotly(pp, tooltip = "all")

  # if(plotly == TRUE){
  #   return(ppp)
  # }else{return(p)}
  myList <- list("Interaction Matrix:" = dinteraction, p)
  return(myList)
}
