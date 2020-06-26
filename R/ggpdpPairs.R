#' ggpdpPairs
#'
#' @description Creates a plot of the partial dependence of each of the variables in ggpairs plot style matrix
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param method "pdp" (default) or "ale"
#' @param corrVal If TRUE, then display the correlation coefficient on top of scatterplot.
#' @param corr If TRUE then a correlation matrix is displayed in the lower triangle.
#' @param corrMethod a character string indicating which correlation coefficient (or covariance) is to be computed.
#'  One of "pearson" (default), "kendall", or "spearman".
#' @param parallel If TRUE then the method is executed in parallel.
#' @param vars Variables to plot. Defaults to all predictors
#' @param colLow Color to be used for low values
#' @param colHigh Color to be used for low values
#' @param fitlims If supplied, should be a numeric vector of length 2, specifying the fit range.
#' @param gridsize for the pdp/ale plots, defaults to 10
#' @param class For a classification model, show the probability of this class. Defaults to 1.
#' @param ... Not currently implemented
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
#' learner = lrn("regr.ranger", importance = "permutation")
#' fit <- lrn$train(task)
#' ggpdpPairs(task , fit)
#'
#' Boston2 <- Boston1
#' Boston2$medv <- ggplot2::cut_interval(Boston2$medv, 3)
#' levels(Boston2$medv) <- c("lo","mid", "hi")
#' task = TaskClassif$new(id = "Boston2", backend = Boston2, target = "medv")
#' learner = lrn("classif.ranger", importance = "impurity", predit_type = "prob")
#' fit <- learner$train(task)
#' ggpdpPairs(task , fit, class="hi")
#'
#' @export

ggpdpPairs <- function(task, model, method="pdp", corrVal = FALSE, corr = FALSE, corrMethod = "p",
                       parallel = FALSE, vars=NULL, colLow = "#132B43", colHigh = "#56B1F7",
                       fitlims = NULL,gridsize = 10,class=1,cardinality = 20, ...){

  # Set up registered cluster for parallel
  if(parallel){
    plan(future::cluster)
  }

  #prob <- model$learner$type == "classif"
  prob <- model$task_type == "classif"
  data <-  task$data()
  data <- as.data.frame(data)
  target <- task$target_names
  #data <- getTaskData(task)

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
  xvar1 <- expand.grid(1:ncol(xdata), 1:ncol(xdata))[,2:1]
  xvar <- xvar1$Var1
  xvar <- as.matrix(xvar)
  xvarn <- cbind(names(xdata)[xvar[,1]])
  xvarn <- xvarn[1:length(unique(xvarn))]
  xvarn <- as.matrix(xvarn)

  # Create progress bar
  pb <- progress_bar$new(
    format = "  Calculating pdp + ice...[:bar]:percent. Est::eta ",
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

  # get predictions
  Pred <- pred.data$predict(data)
  colnames(Pred) <- "prd"
  Pred <- Pred$prd
  # Plot prep for diag pdps
  ovars <- task$feature_names
  ggpdpDiag <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist1[[paste(vars[1])]]
    aggr <- pdp$results[pdp$results$.type != "ice", ]
    plot(pdp, rug=FALSE) +
      geom_line(aes(y = .value, group = .id, color = .value)) +
      geom_line(data = aggr, size = 2, color = "gold")
  }

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
  ggTitle <- model$id
  #ggTitle <- model$learner$id

  # Display warning if both corr & corrVal = TRUE
  if(corr == TRUE && corrVal == TRUE){
    message(" warning: Trying to plot 2 different displays at once.
            please select either corr = TRUE or corrVal = TRUE.
            Defaulting to corrVal = TRUE")
  }

  if(corrVal == TRUE){
    GGscatterPlot <- function(data, mapping,method = corrMethod)
    {
      #Get correlation coefficient
      x <- eval_data_col(data, mapping$x)
      y <- eval_data_col(data, mapping$y)

      corr <- cor(x, y, method = method)

      # Assemble data frame
      df <- data.frame(x = x, y = y)

      # Set colour for label
      colFn <- colorRampPalette(c("dodgerblue", "floralwhite", "firebrick1"), interpolate ='spline')
      fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]

      # Prepare plot
      ggplot(df, aes(x = x, y = y, color = Pred)) +
        geom_point(shape = 16, size = 1, show.legend = FALSE) +
        geom_smooth(method=loess, fill="red", color="red", ...) +
        geom_label(
          data = data.frame(
            xlabel = min(x, na.rm = TRUE),
            ylabel = max(y, na.rm = TRUE),
            lab = round(corr, digits = 3)),
          mapping = ggplot2::aes(x = xlabel,
                                 y = ylabel,
                                 label = lab),
          fill = fill,
          hjust = 0, vjust = 1,
          size = 3, fontface = "bold",
          inherit.aes = FALSE) +
        theme_minimal()
    }

    p <- ggpairs(xdata, title = ggTitle,
                 upper = list(continuous = ggpdp, combo = ggpdpc, discrete = ggpdp),
                 diag  = list(continuous = ggpdpDiag),
                 lower = list(continuous = GGscatterPlot),
                 legend=w,
                 cardinality_threshold = cardinality) +
      theme_bw() +
      theme(panel.border=element_blank(), axis.line=element_line(),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            strip.text = element_text(face ="bold", colour ="red", size = 5))
    p
  }else if(corr == TRUE){
    my_fn <- function(data, mapping, method = corrMethod, use="pairwise", ...){

      # grab data
      x <- eval_data_col(data, mapping$x)
      y <- eval_data_col(data, mapping$y)

      # calculate correlation: for significance stars and set some values
      corr <- cor.test(x, y, method=method)
      est <- corr$estimate
      sz = 8
      ndp <- 2 # number of decimal points
      lb.size <- sz*abs(est) # set label size


      # get significance stars
      stars <- c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 0.05, 1))]
      lbl <- paste0(round(est, ndp), stars)

      # calculate correlation
      corr <- cor(x, y, method = corrMethod, use=use)

      # calculate colour based on correlation value
      # Here I have set a correlation of minus one to blue,
      # zero to white, and one to red
      colFn <- colorRampPalette(c("dodgerblue", "floralwhite", "firebrick1"), interpolate ='spline')
      fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]

      ggplot(data = data, mapping = mapping, ...) +
        theme_void() +
        annotate("text",
                 x=mean(x, na.rm=TRUE),
                 y=mean(y, na.rm=TRUE),
                 label=lbl,
                 size=lb.size,
                 ...) +
        theme(panel.background = element_rect(fill = fill,  # to fill background of panel with colour
                                              colour = NA), # to remove border of panel
              panel.grid.major = element_blank())

    }

    p <- ggpairs(xdata, title = ggTitle,
                 mapping=ggplot2::aes(label = lbl),
                 upper=list(continuous = ggpdp, combo = ggpdpc, discrete = ggpdp),
                 diag = list(continuous = ggpdpDiag),
                 lower=list(continuous = my_fn),
                 legend=w,
                 cardinality_threshold = cardinality) +
      theme(panel.border=element_blank(), axis.line=element_line(),
            strip.text = element_text(face ="bold", colour ="red", size = 5))
    p
  }else{
    p <- ggpairs(xdata, title = ggTitle,
                 mapping=ggplot2::aes(colour = Pred),
                 upper=list(continuous = ggpdp, combo = ggpdpc, discrete = ggpdp),
                 diag = list(continuous = ggpdpDiag),
                 lower=list(continuous=wrap("points", size = 0.5)),
                 legend=w,
                 cardinality_threshold = cardinality) +
      theme_bw() + theme(panel.border=element_blank(), axis.line=element_line(),
                         axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                         strip.text = element_text(face ="bold", colour ="red", size = 5))

    p
  }

  if(parallel){
    # Closing works by setting them to default
    plan("default")
  }
  suppressMessages(print(p))
  invisible(p)

}

