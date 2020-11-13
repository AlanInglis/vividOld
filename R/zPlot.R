#' zPlot
#'
#'   @description Create a zenplot displaying the 2-way partial dependence of each of the variables.
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model from mlr3.
#' @param method "pdp" (default) or "ale"
#' @param vars Variables to plot. Defaults to all predictors.
#' @param colLow Colour to be used for low values.
#' @param colMid Colour to be used for mid values.
#' @param colHigh Colour to be used for low values.
#' @param fitlims If supplied, should be a numeric vector of length 2, specifying the fit range.
#' @param gridsize for the pdp/ale plots, defaults to 10.
#' @param class For a classification model, show the probability of this class. Defaults to 1.
#' @param ... Not currently implemented.
#'
#' @return A zenplot displaying the 2-way partial dependence.
#' @importFrom iml "FeatureEffect"
#' @importFrom iml "Predictor"
#' @importFrom zenplots "zenplot"
#' @importFrom stats "median"
#' @import progress
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
#' aq_Task = TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn = lrn("regr.ranger", importance = "permutation")
#' aq_Mod <- aq_lrn$train(aq_Task)
#'
#' # Create plot:
#' zPlot(task = aq_Task, model = aq_Mod)
#'
#' @export

# Plotting Function -------------------------------------------------------
# -------------------------------------------------------------------------
zPlot <- function(task, model, method = "pdp",
                  vars = NULL,
                  colLow = "#D7191C", colMid = "#FFFFBF", colHigh = "#2B83BA",
                  fitlims = NULL, gridsize = 10, class = 1){


  # Get data
  prob <- model$task_type == "classif"
  data <-  task$data()
  data <- as.data.frame(data)
  target <- task$target_names


  # make iml model
  if (prob){
    pred.data <- Predictor$new(model, data = data, class = class, y = target)
  }
  else{
    pred.data <- Predictor$new(model, data = data, y = target)
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
    format = "  Calculating partial dependence...[:bar]:percent. Est::eta ",
    total = nrow(xyvarn),
    clear = FALSE)

  # loop through vars and create a list of pdps for each pair
  pdplist <- vector("list", length=nrow(xyvarn))
  for (i in 1:nrow(xyvarn)){
    pdplist[[i]] <-FeatureEffect$new(pred.data, xyvarn[i,], method = method, grid.size=gridsize)
    pb1$tick()
    names(pdplist)  <- paste(xyvarn[,1], xyvarn[,2], sep="pp")
  }

  # Set limits for plot
  if (is.null(fitlims)){
    r <- sapply(pdplist, function(x) range(x$results[,3]))
    r <- range(r)
    limits <- range(labeling::rpretty(r[1],r[2]))
  } else
    limits <- fitlims

  Pred <- pred.data$predict(data)
  colnames(Pred) <- "prd"
  Pred <- Pred$prd
  midLimit <- floor(median(Pred))

  # Zenplot graphing function
  ggplot2d <- function(zargs, vals) {

    # zen prep
    unL <- unlist(zargs$x, recursive=F)
    ppi <- plot_indices(zargs)
    x <- unL[ppi[1]]
    y <- unL[ppi[2]]


    for (idx in 1:length(zargs$x[[1]]$x1)) {
      pdplist[[1]]$results$.value[[idx]] <- vals[[ppi[[1]]]][[idx]]
    }

    # create plot
    p <- plot(pdplist[[1]], rug=FALSE ) +
      scale_fill_gradient2(name="\u0177",low = "#D7191C",
                           mid = "#FFFFBF",
                           high = "#2B83BA",
                           midpoint = midLimit, limits=limits)+
      guides(fill=FALSE) +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    ggplot_gtable(ggplot_build(p))
  }

  # get rid of unnecessary columns. Result is list of data frames to
  # pass to zenplot to plot PDPs
  allPdpResults <- lapply(pdplist, `[[`,"results")
  newPdpList <- lapply(allPdpResults, function(x) { x[".type"] <- NULL; x })
  newPdpList <- lapply(newPdpList, function(x) { x[".id"] <- NULL; x })
  newPdpList <- lapply(newPdpList, function(x) { x[".value"] <- NULL; x })


  # Create a list of the predicted values
  newPdpValueList <- lapply(1:length(allPdpResults), function(x,i) { x[[i]]$.value }, x = allPdpResults)
  # duplicate list
  rep_pdp_new <- lapply(seq_along(newPdpValueList), function(i) replicate(2,newPdpValueList[[i]]))
  rep_pdp_new <- lapply(seq_along(rep_pdp_new), function(i) c(list(rep_pdp_new[[i]][,1]), list(rep_pdp_new[[i]][,2])))
  rep_pdp_new <- unlist(rep_pdp_new,recursive=FALSE)

  # plot
   zenplot(newPdpList, labs = list(group = NULL), lim ='groupwise', pkg = "grid", plot1d = "layout",
          plot2d = function(zargs) ggplot2d(zargs, rep_pdp_new))

}










