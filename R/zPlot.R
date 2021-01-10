#' Create a zenplot displaying partial dependence values.
#'
#' @description Constructs a zigzag expanded navigation plot (zenplot) displaying partial dependence values.
#'
#' @param task Task created from the mlr3 package, either regression or classification.
#' @param model A machine learning model created from mlr3 task and learner.
#' @param zpath A zenpath created from calcZpath. see \code{\link[zenplots]{zenpath}} from the
#' \code{\link[zenplots]{zenplots}} package for more details.
#' @param method "pdp" (default) or "ale"
#' @param noCols  number of columns of 2d plots (>= 1) or one of "letter", "square", "A4", "golden" or "legal"
#' in which case a similar layout is constructed. See ?zenplot
#' @param zenMethod String indicating the layout of the zigzag plot. The available methods are:
#' "tidy": more tidied-up double.zigzag (slightly more compact placement of plots towards the end).
#' "double.zigzag": zigzag plot in the form of a flipped “S”. Along this path, the plots are placed in the form of an “S” which is rotated counterclockwise by 90 degrees.
#' "single.zigzag": zigzag plot in the form of a flipped “S”.
#' "rectangular": plots that fill the page from left to right and top to bottom. This is useful (and most compact) for plots that do not share an axis.
#' @param pal A vector of colors to show predictions, for use with scale_fill_gradientn
#' @param fitlims If supplied, should be a numeric vector of length 2, specifying the fit range.
#' @param gridsize for the pdp/ale plots, defaults to 10.
#' @param class For a classification model, show the probability of this class. Defaults to 1.
#'
#' @return A zenplot of partial dependence values.
#'
#' @importFrom zenplots "zenplot"
#' @importFrom zenplots "indexData"
#' @importFrom zenplots "groupData"
#' @importFrom iml "FeatureEffect"
#' @importFrom iml "Predictor"
#'
#' @examples
#' # Load in the data:
#' aq <- na.omit(airquality)*1.0
#'
#' # Run an mlr3 ranger model:
#' library(mlr3)
#' library(mlr3learners)
#' library(ranger)
#' ozonet  <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' ozonel  <- lrn("regr.ranger", importance = "permutation")
#' ozonef  <- ozonel$train(ozonet)
#'
#' # Create matrix
#' viv <- vividMatrix(ozonet, ozonef)
#'
#'# Calculate Zpath:
#' zpath<-calcZpath(viv,.8)
#' zpath
#'
#' # Create graph:
#' pdpZenplot(ozonet, ozonef, zpath=zpath)
#'
#' @export



pdpZenplot <- function(task, model, zpath=NULL, method = "pdp",
                       noCols = c("letter", "square", "A4", "golden", "legal"),
                       zenMethod = c("tidy", "double.zigzag", "single.zigzag", "rectangular"),
                       pal=rev(RColorBrewer::brewer.pal(11,"RdYlBu")),
                       fitlims = NULL, gridsize = 10, class = 1,...){

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

  if (is.null(zpath)) {
    zpath <- 1:ncol(xdata)
    zdata <- xdata
    zpairs <- t(sapply(1:(length(zpath)-1), function(i){
      z <- zpath[i:(i+1)]
      if (i %% 2 == 0) rev(z) else z
    }))
  }
  else if (is.character(zpath)){
    zpath <- match(zpath, names(xdata))
    if (any(is.na(zpath))) stop("'zpath' should contain predictor names.")
    zdata <- indexData(xdata, zpath)
    zpairs <- t(sapply(1:(length(zpath)-1), function(i){
      z <- zpath[i:(i+1)]
      if (i %% 2 == 0) rev(z) else z
    }))
  }
  else if (is.list(zpath)){
    zpath0 <- unlist(zpath)
    zpath0 <- match(zpath0, names(xdata))
    if (any(is.na(zpath0))) stop("'zpath' should contain predictor names.")
    zpath <- lapply(zpath, function(z) match(z, names(xdata)))
    zpairs <- t(sapply(1:(length(zpath0)-1), function(i){
      z <- zpath0[i:(i+1)]
      if (i %% 2 == 0) rev(z) else z
    }))
    fixind <- cumsum(sapply(zpath, length))
    fixind <- fixind[-length(fixind)]
    for (i in fixind) zpairs[i,]<- NA
    zdata <- groupData(xdata, indices = zpath)
  }


  # Create progress bar
  pb1 <- progress_bar$new(
    format = "  Calculating partial dependence...[:bar]:percent. Est::eta ",
    total = nrow(zpairs),
    clear = FALSE)

  # loop through vars and create a list of pdps for each pair
  pdplist <- vector("list", nrow(zpairs))

  for (i in 1:nrow(zpairs)){
    ind <- zpairs[i,]

    if (!is.na(ind[1]))
      p <-FeatureEffect$new(pred.data, ind, method = "pdp", grid.size=gridsize)
    else p <- NULL
    pdplist[[i]] <- list(index=ind, pdp=p)
    pb1$tick()
  }



  # get predictions
  Pred <- pred.data$predict(data)
  colnames(Pred) <- "prd"
  Pred <- Pred$prd


  # Set limits for pairs
  if (is.null(fitlims)){
    pdplist0 <- lapply(pdplist, function(x) x$pdp)
    pdplist0 <-pdplist0[!sapply(pdplist0, is.null)]
    r <- sapply(pdplist0, function(x) range(x$results[,3]))
    r <- range(c(r,Pred))
    limits <- range(labeling::rpretty(r[1],r[2]))
  } else
    limits <- fitlims


  # Zenplot graphing function
  z2index <- 0
  ggplot2d <- function(zargs) {

    z2index <<- z2index+1

    pdp <- pdplist[[z2index]]$pdp
    if (!is.null(pdp)) {
      p <- plot(pdp, rug=FALSE ) +
        scale_fill_gradientn(name = "\u0177",colors = pal, limits = limits)+
        guides(fill=FALSE, color=FALSE) +
        theme_bw() +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.border = element_rect(colour = "gray", fill=NA, size = 1.5))

      }
    else p <- ggplot() + theme(panel.background = element_blank())

    ggplot_gtable(ggplot_build(p))
  }
  suppressMessages({
    zenplot(zdata, pkg="grid", labs=list(group=NULL),
            plot2d = function(zargs) ggplot2d(zargs), ...)
  })

}
