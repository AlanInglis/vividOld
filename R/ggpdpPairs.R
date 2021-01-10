#' ggpdpPairs
#'
#' @description Creates a plot of the partial dependence of each of the variables in ggpairs plot style matrix
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param method "pdp" (default) or "ale"
#' @param parallel If TRUE then the method is executed in parallel.
#' @param vars Specify which variables and their order to plot. Defaults to all predictors.
#' @param pal A vector of colors to show predictions, for use with scale_fill_gradientn
#' @param fitlims If supplied, should be a numeric vector of length 2, specifying the fit range.
#' @param gridsize for the pdp/ale plots, defaults to 10.
#' @param class For a classification model, show the probability of this class. Defaults to 1.
#' @param nIce Number of ice curves to be plotted, defaults to 30
#' @param ... Not currently implemented.
#'
#' @return A ggpairs style plot displaying the partial dependence.
#' @importFrom iml "FeatureEffect"
#' @importFrom iml "Predictor"
#' @importFrom GGally "ggpairs"
#' @importFrom GGally "wrap"
#' @importFrom GGally "eval_data_col"
#' @importFrom future "plan"
#'
#' @import progress
#'
#' @examples
#'
#' # Run an mlr3 ranger model:
#' library(mlr3)
#' library(mlr3learners)
#' library(MASS)
#' Boston1 <- Boston[,c(4:6,8,13:14)]
#' Boston1$chas <- factor(Boston1$chas)
#' task <- TaskRegr$new(id = "Boston1", backend = Boston1, target = "medv")
#' learner <- lrn("regr.ranger", importance = "permutation")
#' fit <- learner$train(task)
#' ggpdpPairs(task , fit)
#'
#' Boston2 <- Boston1
#' Boston2$medv <- ggplot2::cut_interval(Boston2$medv, 3)
#' levels(Boston2$medv) <- c("lo","mid", "hi")
#' task <- TaskClassif$new(id = "Boston2", backend = Boston2, target = "medv")
#' learner <- lrn("classif.ranger", importance = "impurity")
#' fit <- learner$train(task)
#' ggpdpPairs(task , fit, class="hi")
#'
#' @export

ggpdpPairs <- function(task, model, method = "pdp",
                       parallel = FALSE, vars = NULL, pal=rev(RColorBrewer::brewer.pal(11,"RdYlBu")),
                       fitlims = NULL, gridsize = 10, class = 1,
                       nIce=30,...){

  # Set up registered cluster for parallel
  if(parallel){
    plan(future::cluster)
  }

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

  # Get data for individual variables
  xdata <- pred.data$data$get.x()
  if (!is.null(vars) & all(vars %in% names(xdata)))
    xdata <- xdata[,vars]


  xvarn <- names(xdata)

  nIce <- min(nIce, nrow(xdata)) # ch, make this an argument for the function
  sice <- c(NA, sample(nrow(xdata), nIce)) # ch

  # Create progress bar
  pb <- progress_bar$new(
    format = "  Calculating ICE curves...[:bar]:percent. Est::eta ",
    total = length(xvarn),
    clear = FALSE)

  # loop through vars and create a list of pdps
  pdplist1 <- vector("list", length=length(xvarn))
  for (i in 1:length(xvarn)){
    pdplist1[[i]] <-FeatureEffect$new(pred.data, xvarn[i], method = "pdp+ice", grid.size=gridsize)
    pb$tick()
    names(pdplist1)  <- xvarn
  }

  # Get data for pairs of variables

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
    pdplist[[i]] <-FeatureEffect$new(pred.data, rev(xyvarn[i,]), method = method, grid.size=gridsize) # added rev, ch
    pb1$tick()
    names(pdplist)  <- paste(xyvarn[,2], xyvarn[,1], sep="pp") # switch 1 and 2, ch
  }

  # get predictions
  Pred <- pred.data$predict(data)
  colnames(Pred) <- "prd"
  Pred <- Pred$prd

  # Set limits for pairs
  if (is.null(fitlims)){
    r <- sapply(pdplist, function(x) range(x$results[[".value"]]))
    r1 <- sapply(pdplist1, function(x)  range(subset(x$results, .id %in% sice)[[".value"]]))
    r <- range(c(r,r1,Pred))
    limits <- range(labeling::rpretty(r[1],r[2]))
  } else
    limits <- fitlims


  ggpdp <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist[[paste(vars[1], vars[2], sep="pp")]]  # switch 1 and 2, ch
    plot(pdp, rug=F) +scale_fill_gradientn(name = "\u0177",colors = pal, limits = limits)

  }


  # Plot prep for diag pdps

  ggpdpDiag <- function(data, mapping, ...) {
    var<- quo_name(mapping$x) # ch
    pdp <- pdplist1[[var]]  # ch
    pdpr <- pdp$results # ch
    pdpr <- subset(pdpr, .id %in% sice) # ch
    aggr <- pdpr[pdpr$.type != "ice", ] # ch


    ggplot(data=pdpr, aes(x=.data[[var]], y=.value, color = .value))+
      geom_line(aes(group=.id, color=.value)) +
      scale_color_gradientn(name = "\u0177",colors = pal, limits = limits)+
      geom_line(data = aggr, size = 1, color = "black", lineend = "round", group=1)
  }

  # plot prep for class.
  ggpdpc <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist[[paste(vars[1], vars[2], sep="pp")]] # switch 1 and 2, ch
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
  ggTitle <- model$id

  lowerPlotc <-  function(data, mapping) {

    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)

    # Assemble data frame
    df <- data.frame(x = x, y = y)


    # Prepare plot
    ggplot(df, aes(x = x, y = y, color = Pred)) +
      geom_point(shape = 16, size = 1, show.legend = FALSE) +
      scale_colour_gradientn(name = "\u0177",colors = pal, limits = limits)
  }

  lowerPlotm <-  function(data, mapping) {

    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)

    # Assemble data frame
    df <- data.frame(x = x, y = y)
    jitterx <- if (is.factor(df$x)) .25 else 0
    jittery <- if (is.factor(df$y)) .25 else 0

    ggplot(df, aes(x = x, y = y, color = Pred)) +
      geom_jitter(shape = 16, size = 1, show.legend = FALSE, width=jitterx, height=jittery) +
      scale_colour_gradientn(name = "\u0177",colors = pal, limits = limits)
  }

  p <- ggpairs(xdata, title = ggTitle,
               mapping=ggplot2::aes(colour = Pred),
               upper=list(continuous = ggpdp, combo = ggpdpc, discrete = ggpdp),
               diag = list(continuous = ggpdpDiag, discrete=ggpdpDiag), # added discrete, ch
               lower = list(continuous = lowerPlotc, combo=lowerPlotm, discrete=lowerPlotm),
               # lower=list(continuous=wrap("points", size = 0.5)),
               legend=w,
               cardinality_threshold = NULL) +
    theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
                       axis.line=element_line(),
                       axis.ticks = element_blank(),
                       axis.text.x = element_text(angle = 45, hjust = 1, size = 0),
                       axis.text.y = element_text(size = 0),
                       strip.text = element_text(face ="bold", colour ="red", size = 5))


  p




  if(parallel){
    # Closing works by setting them to default
    plan("default")
  }
  suppressMessages(print(p))
  invisible(p)

}
