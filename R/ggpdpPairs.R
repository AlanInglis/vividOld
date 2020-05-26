#' ggpdpPairs
#'
#' @description Creates a plot of the partial dependence of each of the variables in ggpairs plot style matrix
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param method "pdp" (default) or "ale"
#' @param vars Variables to plot. Defaults to all predictors
#' @param colLow Color to be used for low values
#' @param colHigh Color to be used for low values
#' @param fitlims If supplied, should be a numeric vector of length 2, specifying the fit range.
#' @param gridsize for the pdp/ale plots, defaults to 10
#' @param class For a classification model, show the probability of this class. Defaults to 1.
#' @param ... Not currently implemented
#'
#' @return A ggpairs style plot displaying the partial dependence.
#' @importFrom mlr "getTaskData"
#' @importFrom mlr "getTaskTargets"
#' @importFrom iml "FeatureEffect"
#' @importFrom iml "Predictor"
#' @importFrom GGally "ggpairs"
#' @importFrom GGally "wrap"
#' @import progress
#'
#' @examples
#'
#' # Run an mlr random forest model:
#' library(mlr)
#' library(randomForest)
#' library(MASS)
#' Boston1 <- Boston[,c(4:6,8,13:14)]
#' Boston1$chas <- factor(Boston1$chas)
#' task  <- makeRegrTask(data = Boston1, target = "medv")
#' learner <- makeLearner("regr.randomForest")
#' fit <- train(learner, task)
#' ggpdpPairs(task , fit)
#'
#' Boston2 <- Boston1
#' Boston2$medv <- ggplot2::cut_interval(Boston2$medv, 3)
#' levels(Boston2$medv) <- c("lo","mid", "hi")
#' task  <- makeClassifTask(data = Boston2, target = "medv")
#' learner <- makeLearner("classif.randomForest",predict.type = "prob")
#' fit <- train(learner, task)
#' ggpdpPairs(task , fit, class="hi")
#'
#' @export

ggpdpPairs <- function(task, model, method="pdp",vars=NULL, colLow = "#132B43", colHigh = "#56B1F7",
                       fitlims = NULL,gridsize = 10,class=1,cardinality = 20, ...){

   prob <- model$learner$type == "classif"
  data <- getTaskData(task)

  # make iml model
  if (prob){
    pred.data <- Predictor$new(model, data = data, class=class)
  }
  else{
    pred.data <- Predictor$new(model, data = data)
  }

  # Get data for individual variables
  xdata <- pred.data$data$get.x()
  if (!is.null(vars) & all(vars %in% names(xdata)))
    xdata <- xdata[,vars]
  xvar1 <- expand.grid(1:ncol(xdata), 1:ncol(xdata))[,2:1]
  xvar <- xvar1$Var1
  xvar <- as.matrix(xvar)
  xvarn <- cbind(names(xdata)[xvar[,1]])
  xvarn <- xvarn[1:length(unique(xvarn))]
  xvarn <- as.matrix(xvarn)

  # Create progress bar
  pb <- progress_bar$new(
    format = "  Calculating pdp + ice...[:bar]:percent. Estimated completion time::eta ",
    total = nrow(xvarn),
    clear = FALSE)

  # loop through vars and create a list of pdps
  pdplist1 <- vector("list", length=nrow(xvarn))
  for (i in 1:nrow(xvarn)){
    pdplist1[[i]] <-FeatureEffect$new(pred.data, xvarn[i,], method = "pdp+ice", grid.size=10)
    pb$tick()
  names(pdplist1)  <- paste(xvarn[,1])
  }

  # Get data for pairs of variables
  xdata <- pred.data$data$get.x()
  if (!is.null(vars) & all(vars %in% names(xdata)))
    xdata <- xdata[,vars]
  xyvar <- expand.grid(1:ncol(xdata), 1:ncol(xdata))[,2:1]
  xyvar <- as.matrix(xyvar[xyvar[,1]<xyvar[,2],])
  xyvarn <- cbind(names(xdata)[xyvar[,1]], names(xdata)[xyvar[,2]])


  # Create progress bar
  pb1 <- progress_bar$new(
    format = "  Calculating partial dependence...[:bar]:percent. Estimated completion time::eta ",
    total = nrow(xyvarn),
    clear = FALSE)

  # loop through vars and create a list of pdps for each pair
  pdplist <- vector("list", length=nrow(xyvarn))
  for (i in 1:nrow(xyvarn)){
    pdplist[[i]] <-FeatureEffect$new(pred.data, xyvarn[i,], method = method, grid.size=gridsize)
    pb1$tick()
  names(pdplist)  <- paste(xyvarn[,1], xyvarn[,2], sep="pp")
 }

   # Set limits for pairs
  if (is.null(fitlims)){
    r <- sapply(pdplist, function(x) range(x$results[,3]))
    r <- range(r)
    limits <- range(labeling::rpretty(r[1],r[2]))
  } else
    limits <- fitlims

  # Plot prep for pairs
  ggpdp <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist[[paste(vars[2], vars[1], sep="pp")]]
    # pdp <-FeatureEffect$new(pred.data, vars, method = method, grid.size=gridsize)
    plot(pdp, rug=FALSE) + scale_fill_gradient(name="\u0177",low=colLow, high=colHigh,limits=limits)
  }

  # Plot prep for diag pdps
  ggpdpDiag <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist1[[paste(vars[1])]]
    plot(pdp, rug=FALSE) }

  # plot prep for class.
  ggpdpc <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist[[paste(vars[2], vars[1], sep="pp")]]
    # pdp <-FeatureEffect$new(pred.data, vars, method = method, grid.size=gridsize)
    plot(pdp, rug=FALSE) + ylim(limits)
  }


  w <- which(sapply(xdata, is.numeric))
  if (length(w) >= 2) w <- w[1:2]
  else {
    w <- which(sapply(xdata, is.factor))
    if (length(w) >= 2) w <- w[1:2]
    else w <- NULL
  }

  # get y-data
  yData <- pred.data$data$y
  yData <- as.numeric(unlist(yData))
  # get predictions
  Pred <- pred.data$predict(data)
  Pred <- Pred$.prediction

  p <- ggpairs(xdata,
               mapping=ggplot2::aes(colour = yData),
               upper=list(continuous = ggpdp, combo=ggpdpc, discrete=ggpdp),
               diag = list(continuous = ggpdpDiag),
               lower=list(continuous=wrap("points", size=.2)), legend=w,
               cardinality_threshold = 12) +
    theme_bw() + theme(panel.border=element_blank(), axis.line=element_line(),
                       strip.text = element_text(face="bold", colour="red", size = 5))

  suppressMessages(print(p))
  invisible(p)

}

