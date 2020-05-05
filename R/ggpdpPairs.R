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

  xdata <- pred.data$data$get.x()
  if (!is.null(vars) & all(vars %in% names(xdata)))
    xdata <- xdata[,vars]
  xyvar <- expand.grid(1:ncol(xdata), 1:ncol(xdata))[,2:1]
  xyvar <- as.matrix(xyvar[xyvar[,1]<xyvar[,2],])
  xyvarn <- cbind(names(xdata)[xyvar[,1]], names(xdata)[xyvar[,2]])

  pdplist <- vector("list", length=nrow(xyvarn))
  for (i in 1:nrow(xyvarn))
    pdplist[[i]] <-FeatureEffect$new(pred.data, xyvarn[i,], method = method, grid.size=gridsize)
  names(pdplist)  <- paste(xyvarn[,1], xyvarn[,2], sep="pp")

  if (is.null(fitlims)){
    r <- sapply(pdplist, function(x) range(x$results[,3]))
    r <- range(r)
    limits <- range(labeling::rpretty(r[1],r[2]))
  } else
    limits <- fitlims

  ggpdp <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist[[paste(vars[2], vars[1], sep="pp")]]
    # pdp <-FeatureEffect$new(pred.data, vars, method = method, grid.size=gridsize)
    plot(pdp, rug=FALSE) + scale_fill_gradient(name="yhat",low=colLow, high=colHigh,limits=limits)
  }

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
  p <- ggpairs(xdata,
               upper=list(
                 continuous = ggpdp, combo=ggpdpc, discrete=ggpdp),
                lower=list(continuous=wrap("points", size=.2)), legend=w,
               cardinality_threshold = cardinality)
  suppressMessages(print(p))
  invisible(p)

}
