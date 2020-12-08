
pdpZenplot <- function(task, model, zpath=NULL, method = "pdp",
                       colLow = "#D7191C", colMid = "#FFFFBF", colHigh = "#2B83BA",
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

  # Set limits for pairs
  if (is.null(fitlims)){
    pdplist0 <- lapply(pdplist, function(x) x$pdp)
    pdplist0 <-pdplist0[!sapply(pdplist0, is.null)]
    r <- sapply(pdplist0, function(x) range(x$results[,3]))
    r <- range(r)
    limits <- range(labeling::rpretty(r[1],r[2]))
  } else
    limits <- fitlims

  Pred <- pred.data$predict(data)
  colnames(Pred) <- "prd"
  Pred <- Pred$prd
  midLimit <- floor(median(Pred))


  # Zenplot graphing function
  z2index <- 0
  ggplot2d <- function(zargs) {

    z2index <<- z2index+1

    pdp <- pdplist[[z2index]]$pdp
    if (!is.null(pdp)) {
      p <- plot(pdp, rug=FALSE ) +
        scale_fill_gradient2(name="\u0177",low = "#D7191C",
                             mid = "#FFFFBF",
                             high = "#2B83BA",
                             midpoint = midLimit, limits=limits)+
        guides(fill=FALSE, color=FALSE) +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())}
    else p <- ggplot() +theme(panel.background = element_blank())
    ggplot_gtable(ggplot_build(p))
  }

  zenplot(zdata, pkg="grid", labs=list(group=NULL),
          plot2d = function(zargs) ggplot2d(zargs), ...)
}


calcZpath <- function(viv, cutoff=NULL, method=c("greedy.weighted", "strictly.weighted"), connect=TRUE){
  # returns a zpath from viv showing pairs with viv entry over the cutoff
  # if connect is TRUE, connect the edges from separate eulerians (strictly.weighted only)
  # see zenpath

  method <- match.arg(method)

  diag(viv)<- NA
  viv[upper.tri(viv)] <- NA
  # find the off-diagonal entries in viv that are bigger than some number
  if (!is.numeric(cutoff)) cutoff <- quantile(viv, .8, na.rm=TRUE)
  viv[is.na(viv)]<- 0
  w <- viv>cutoff
  if (sum(w) == 0) stop("No off diagonal entries in 'viv' exceed 'cutoff'.")
  zinfo <- cbind(viv[w],  row(viv)[w],col(viv)[w])

  # So the plots we want to see are, because these have a high interaction score
  # cbind(rownames(viv)[zinfo[,2]],rownames(viv)[zinfo[,3]], zinfo[,1])

  # form an eulerian path with these pairs of variables
  if (method=="greedy.weighted"){
    zpath <- tryCatch(zpath <- zenpath(zinfo[,1], pairs=zinfo[,-1], method="greedy.weighted"),
                      error = function(e) NULL,warning = function(w) {})
  }
  if (method=="strictly.weighted"| is.null(zpath)| length(zpath)==0){
    zpath <- zenpath(zinfo[,1], pairs=zinfo[,-1], method="strictly.weighted")
    zpath <-  connect_pairs(zpath)
    if (connect) zpath <- unlist(zpath)
  }

  if (is.numeric(zpath))
    zpath <- rownames(viv)[zpath]
  else if (is.list(zpath)) zpath <- lapply(zpath, function(z) rownames(viv)[z])

  zpath
}


#  testing--------------------
aq <- na.omit(airquality)*1.0  # need this, as iml seems to have trouble with type int (yikes!!)

ozonet  <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
ozonel <- lrn("regr.ranger", importance = "permutation")
ozonef  <- ozonel$train(ozonet)

pdpZenplot(ozonet, ozonef)

#  now try and plot the important pairs from the all pairs version

set.seed(123)
viv <- vividMatrix(ozonet, ozonef)
zpath<-calcZpath(viv,.8)
zpath
pdpZenplot(ozonet, ozonef, zpath=zpath)

#--------------------




# test case where there are categorical predictors

iris1 <- iris
iris1$fake <- factor(letters[1:2])

irist  <- TaskRegr$new(id = "iris", backend = iris1, target = "Sepal.Length")
irisl <- lrn("regr.ranger", importance = "permutation")
irisf  <- irisl$train(irist)

#--------------------

# test Boston
set.seed(123)
library(MASS)
Boston1 <- Boston
Boston1$rad <- Boston1$rad*1.0  # int type causes problem for me, a bug in iml
Boston1$chas <- as.factor(Boston1$chas)
bostont  <- TaskRegr$new(id = "boston1", backend = Boston1, target = "medv")
bostonl <- lrn("regr.ranger", importance = "permutation")
bostonf  <- bostonl$train(bostont)


vivb <- vividMatrix(bostont, bostonf)
plot(vivb)

pdpZenplot(bostont, bostonf)

pdpZenplot(bostont, bostonf, zpath=c("dis", "nox", "lstat","tax"))
pdpZenplot(bostont, bostonf, zpath=list(c("dis", "nox"), c("lstat","tax")))


# zpath<-calcZpath(viv)  # by default uses top 20% of plots
zpath<-calcZpath(vivb, .2)
zpath

pdpZenplot(bostont, bostonf, zpath=zpath)

# version with gaps
zpath<-calcZpath(vivb, .2, connect=FALSE)
zpath
pdpZenplot(bostont, bostonf, zpath=zpath)
# removes the two uninteresting plots from previous display, but you don't get nice compact plot




