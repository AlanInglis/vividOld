\name{pdpPairs}
\alias{pdpPairs}

\title{Creates a plot of the partial dependence of each of the variables in pairs plot style matrix
}
\description{
Plots a pdp (partial dependence plot) pairs style matrix. The partial dependence plot shows the marginal effect one or two features have on the predicted outcome of a machine learning model. A partial dependence plot is used to show whether the relationship between the response variable and a feature is linear or more complex.
}
\usage{
pdpPairs(task, model, cols)
}
\arguments{
  \item{task}{Task created from the mlr package (either regression or classification).}
  \item{model}{Any machine learning model.}
  \item{cols}{Colour of the partial dependence plot.}
}

\value{Pairs style matrix plot displaying the partial dependence.
}

\author{
Alan Inglis
}
