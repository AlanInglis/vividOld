
# This code uses pairs to plot the bivariate pdp plots for the iris data.
# Code is bit hacky.
# The pdp are shown as an image instead of a 3d wireframe as this fits better with the pairs concept.

library(mlr)
library(iml)
task <- makeRegrTask(data = iris[,-5], target = "Sepal.Length")
fit  = train(makeLearner("regr.randomForest", id = 'rf'), task)

pred.iris = Predictor$new(fit, data = iris[,-5])


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
  g$zint <- as.numeric(cut_number(g[,3], 5))
  g$cols <- brewer.pal(5, "Blues")[g$zint]
  rect(g$left, g$bottom, g$right, g$top, col=g$cols, border=NA)
})

