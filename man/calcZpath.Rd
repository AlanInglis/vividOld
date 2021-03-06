% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zPath.R
\name{calcZpath}
\alias{calcZpath}
\title{Construct a path of indices to visit to order variables}
\usage{
calcZpath(
  viv,
  cutoff = NULL,
  method = c("greedy.weighted", "strictly.weighted"),
  connect = TRUE
)
}
\arguments{
\item{viv}{A matrix of class 'vivid' to be used to calculate the path.}

\item{cutoff}{Do not include any variables that are below the cutoff value.}

\item{method}{String indicating the method to use. The available methods are:
"greedy.weighted": Sort all pairs according to a greedy (heuristic) Euler path with x as weights visiting each edge precisely once.
"strictly.weighted": Strictly respect the order of the weights - so the first, second, third, and so on, adjacent pair of numbers
of the output of zenpath() corresponds to the pair with largest, second-largest, third-largest, and so on, weight.
see zenpath}

\item{connect}{If connect is TRUE, connect the edges from separate eulerians (strictly.weighted only).}
}
\value{
Returns a zpath from viv showing pairs with viv entry over the cutoff
}
\description{
Constructs a zenpath for connecting and displaying pairs.
}
\examples{
# Load in the data:
aq <- na.omit(airquality)*1.0

# Run an mlr3 ranger model:
library(mlr3)
library(mlr3learners)
library(ranger)
ozonet  <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
ozonel  <- lrn("regr.ranger", importance = "permutation")
ozonef  <- ozonel$train(ozonet)

# Create matrix
viv <- vividMatrix(ozonet, ozonef)

# Calculate Zpath:
zpath<-calcZpath(viv,.8)
zpath

# Create graph:
pdpZenplot(ozonet, ozonef, zpath=zpath)

}
