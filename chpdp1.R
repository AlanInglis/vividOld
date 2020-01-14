
# This code uses pairs to plot the bivariate pdp plots for the iris data.
# Code is bit hacky.
# The pdp are shown as an image instead of a 3d wireframe as this fits better with the pairs concept.

library(mlr)
library(RColorBrewer)
library(iml)
task <- makeRegrTask(data = iris[,-5], target = "Sepal.Length")
fit  <- train(makeLearner("regr.randomForest", id = 'rf'), task)

pred.iris <- Predictor$new(fit, data = iris[,-5])

colorfn <- function(vec, cols= NULL, expand=.07){
  if (is.null(cols))
    cols <-  brewer.pal(8, "Blues")
    
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
    structure(fn,breaks=r)
}


legendn <- function(colorY){
  
  
  r <- attr(colorY, "breaks")
  z1<- r[-length(r)]
  z2<- r[-1]
  rectcols <- colorY(r[-1])
  

  plot( c(0,1),c(z1[1], z2[length(z2)]),  ann=FALSE, axes=F, type="n")
  
  rect(0,z1,1,z2,col=rectcols, lty=0)
  par(mgp = c(2, 0.2, 0))
  axis(2,cex.axis=.7, lwd=0, lwd.ticks=.5 )
}

colfn <- colorfn(iris$Sepal.Length)



pairs(iris[,2:4], panel = function(x,y) {
  xvar <- names(which(sapply(iris, function(u) identical(u, x))))
  yvar <- names(which(sapply(iris, function(u) identical(u, y))))
  pdp <-FeatureEffect$new(pred.iris, c(xvar,yvar), method = "pdp", grid.size=10)
  g <- pdp$results
  dx <- g[2,1] - g[1,1]
  dy <- g[11,2] - g[1,2]
  g$left <- g[,1] - dx/2
  g$right <- g[,1] + dx/2
  g$bottom <- g[,2] - dy/2
  g$top <- g[,2] + dy/2

  g$cols <- colfn(g[,3])
  rect(g$left, g$bottom, g$right, g$top, col=g$cols, border=NA)
})


par(mar=c(2,2,2,2))
legendn(colfn)


