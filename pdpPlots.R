
rm(list = ls())

library(pdp)
library(randomForest)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(reshape2)
library(iml)

# Simulate data -----------------------------------------------------------

# Set Values
n <- 10
p <- 5
e <- rnorm(n)

# Create matrix of values
xValues <- matrix(runif(n*p, 0, 1), nrow=n)               # Create matrix 
colnames(xValues)<- paste0("x", 1:p)                      # Name columns
FRdf <- data.frame(xValues)                               # Create dataframe 

# Equation:
#y = 10sin(πx1x2) + 20(x3−0.5)^2 + 10x4 + 5x5 + ε
y = (10*sin(pi*FRdf$x1*FRdf$x2) + 20 * (FRdf$x3-0.5)^2 + 10 * FRdf$x4 + 5 * FRdf$x5 + e)

# Adding y to FRdf
FRdf$y <- y


# Random Forest -----------------------------------------------------------

prf <- randomForest(y~., data=FRdf)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------



# Extract all plotPartial values
comb<-combn(length(xValues[1,]),2)
aux<-comb[,1]
pf<-partial(prf, pred.var = c(paste0("x", aux[1]), paste0("x", aux[2])))
for (i in 2:length(comb)){
  aux<-comb[,i]
  pd <- partial(prf, pred.var = c(paste0("x", aux[1]), paste0("x", aux[2])))
  pf<- bind_cols(pf,pd)
}

head(pf)
class(pf)


# using ggplot ------------------------------------------------------------

# split into groups of 3 and plot
library(rlang)

splitDF <- split.default(pf, gl(ncol(pf)/3, 3))
class(splitDF)

library(tidyverse)
temp <- split.default(pf, gl(ncol(pf)/3, 3)) %>%
  map(~{
    x <- syms(names(.))
    ggplot(., aes(x = !!x[[1]], y = !!x[[2]])) + geom_raster(aes(fill = !!x[[3]])) + 
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
      scale_fill_distiller(palette = "Spectral") +
      theme_bw() #+
    #theme(legend.position = "none") 
  })
grid.arrange(grobs = temp)  

# Renaming and using pdp package ------------------------------------------

# Manually renaming 
names(pf)[startsWith(names(pf), "yhat")] <- "yhat"
names(pf) <- sub("^([xy]\\d)\\d$", "\\1", names(pf))

# Split into lists of 3 (i.e., [x1,x2, yhat] etc.)
lst1 <- split.default(pf, LETTERS[as.integer(gl(ncol(pf), 3, ncol(pf)))])

# Function to create partial plots:
FUN1 = function(x,...) {
  pdpPl <- plotPartial(x)
  #pdpPl$legend = NULL
  return(pdpPl)
}

# apply function over list
pdps <- lapply(lst1, FUN1)

# Plot on grid
grid.arrange(grobs = pdps, ncol = 4)



# Zenplots ----------------------------------------------------------------

library(zenplots)
zpath <- zenpath(ncol(FRdf[-6])) # all pairs
zenplot(FRdf[, zpath],  plot2d = "layout") # this will show example layout final zenplot should take


## Some example custom zenplots:
# Using qqtest library
library(qqtest)
zenplot(FRdf[,zenpath(ncol(FRdf))],
        width1d = 1, width2d = 2, n2dcol=5,
        plot1d = function(zargs, ...) {
          r <- extract_1d(zargs) # extract arguments for 1d
          col <- adjustcolor(if (r$horizontal) "firebrick" else "steelblue",
                             alpha.f = 0.7)
          hist_1d_graphics(zargs, col=col, ...)
        },
        plot2d=function(zargs, ...) {
          r <- extract_2d(zargs) # extract arguments for 2d
          x <- as.matrix(r$x)
          xlim <- r$xlim
          y <- as.matrix(r$y)
          ylim <- r$ylim
          qqtest(y, dataTest=x,
                 xlim=xlim, ylim=ylim,
                 cex=0.3, col="black",  pch=19,
                 legend=FALSE, main="", axes=FALSE, ...)
          
        })


# Just making rectangles follow a path
myrect <- function(...) {
  plot(NA, type = "n", ann = FALSE, axes = FALSE, xlim = 0:1, ylim = 0:1)
  rect(xleft = 0, ybottom = 0, xright = 1, ytop = 1, ...)
}
zenplot(matrix(0, ncol = 15),
        n2dcol = "square", width1d = 10, width2d = 10,
        plot1d = function(...) myrect(col = "royalblue3"),
        plot2d = function(...) myrect(col = "maroon3"))



# Using the iml package to display pdp ----------------------------------------------------------------

## Create predictor function
predictor.rf <- Predictor$new(
  model = prf, 
  data = FRdf, 
  y = FRdf$y, 
  class = "regression"
)

# Feature importance
imp.rf <- FeatureImp$new(predictor.rf, loss = "mse")
p1 <- plot(imp.rf) + ggtitle("RF")
p1

# ICE plot
rf.y <- Partial$new(predictor.rf, "x1", ice = TRUE, grid.size = 50)
rf.y$center(min(FRdf$x1))
p2 <- plot(rf.y) + ggtitle("ICE")
p2


rf.ot <- Partial$new(predictor.rf, "x1") %>% plot() + ggtitle("pdp") 
rf.ot

# 2- way interaction
p3 <- Partial$new(predictor.rf, c("x1", "x2")) %>% plot() + ggtitle("2-way pdp") 
p3



# Extract data from iml example
partIML <- FeatureEffect$new(predictor.rf, c("x1", "x2"), method = "pdp")
imlPdp <- partIML$results[,-4]
str(imlPdp)
head(imlPdp)
