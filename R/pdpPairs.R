#' pdpPairs
#'
#' @description Creates a plot of the partial dependence of each of the variables in pairs plot style matrix
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param cols Colour of partial dependece plot.
#' @param class For a classification model, show the probability of this class. Defaults to 1.
#' @param ... Not currently implemented
#'
#' @return A pairs style plot displaying the partial dependence.
#' @importFrom mlr "getTaskData"
#' @importFrom mlr "getTaskTargets"
#' @importFrom iml "FeatureEffect"
#' @importFrom iml "Predictor"
#' @importFrom iml "Interaction"
#' @import colorspace
#' @importFrom stats "filter"
#' @importFrom graphics "pairs"
#' @importFrom graphics "par"
#' @importFrom graphics "rect"
#' @importFrom graphics "text"
#'
#' @examples
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
#' pdpPairs(aqRgrTask , aqMod)
#'
#' @export


pdpPairs <- function(task, model, cols= rev(sequential_hcl(20,"Blues3")),class=1,...){
  prob <- model$learner$type == "classif"
  data <- getTaskData(task)
  # make iml model
  if (prob)
    pred.data <- Predictor$new(model, data = data, class=class)
  else
  pred.data <- Predictor$new(model, data = data)

  # Colour function
  colorfn <- function(vec,  expand=.03){

    r <- range(vec, na.rm = TRUE)

    if (diff(r) == 0){
      r <- c(r[1]-.5, r[1]+.5)
    }
    else {
      fudge <- diff(r)*expand
      r[1] <- r[1]- fudge
      r[2] <- r[2]+ fudge
      r <- seq(r[1], r[2],length.out=length(cols)+1)
    }


    fn <- function(x){
      index <- as.numeric(cut(x,breaks=r, include.lowest=TRUE))

      cols[index]
    }
    structure(fn,breaks=r, vec=vec)
  }

  if (prob)
    colfn <- colorfn(c(0,1), expand=0)
  else
  colfn <- colorfn(getTaskTargets(task))


  legendn <- function(colorY){
    legendlen <- .5
    boxw <- 1/ncol(xdata)
    legendw <- boxw/6

    r <- attr(colorY, "breaks")
    rs <- (r-r[1])/(r[length(r)] - r[1])
    rs <- legendlen/2 + rs*legendlen
    z1<- rs[-length(rs)]
    z2<- rs[-1]
    rectcols <- colorY(r[-1])

    # rp <- pretty(r)
    rp <- pretty(attr(colorY, "vec"), min.n=4)
    rp <- rp[rp >= r[1] & rp <= r[length(r)]]
    rps <- (rp-r[1])/(r[length(r)] - r[1])
    rps <- legendlen/2 + rps*legendlen

    xpd <- par("xpd")

    usr <- par("usr")
    # clip(usr[1], usr[2], usr[3], usr[4])
    par(xpd=NA)
    # rect(.9,z1[1],.97,z2[length(z2)], border="grey30")
    # rect(.9,z1,.97,z2,col=rectcols, lty=0)
    # text(.975,rps,rp, adj=c(0,.5),cex=.5)
    lmax <- 1.005
    rect(lmax-legendw,z1[1],lmax,z2[length(z2)], border="grey30")
    rect(lmax-legendw,z1,lmax,z2,col=rectcols, lty=0)
    text(lmax+ .005,rps,rp, adj=c(0,.5),cex=.5)
    par(xpd=xpd)
  }

  xdata <- pred.data$data$get.x()
  xyvar <- expand.grid(names(xdata),names(xdata))[,2:1]
  xyvar <- as.matrix(xyvar[xyvar[,1]!=xyvar[,2],])

  j <- 1

  panelfn <- function(x,y) {

    pdp <-FeatureEffect$new(pred.data, xyvar[j,], method = "pdp", grid.size=10)
    j <<- j+1
    g <- pdp$results
    dx <- g[2,1] - g[1,1]
    dy <- g[11,2] - g[1,2]
    g$left <- g[,1] - dx/2
    g$right <- g[,1] + dx/2
    g$bottom <- g[,2] - dy/2
    g$top <- g[,2] + dy/2

    g$cols <- colfn(g[,3])
    rect(g$left, g$bottom, g$right, g$top, col=g$cols, border=NA)
  }
  plas <- par("las")
  par(las=1)
  pairs(xdata, panel = panelfn, oma=c(4,3,5,6), gap=.75)
  par(las=plas)
  legendn(colfn)

}

