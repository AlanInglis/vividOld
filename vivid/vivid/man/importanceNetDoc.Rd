\name{importanceNet}
\alias{importanceNet}

\title{
  Create a Network style plot displaying Variable Importance and Variable Interaction
}
\description{
   Plots a network style graph, where node size represents variable importance and edge width and colour represents interaction strength. The edge colour is displayed as a gradient from black to red. Black represents a weak interaction, whereas red displays stronger interactions.
}
\usage{
importanceNet(task, model, method, thresholdValue, cluster)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{task}{Task created from the mlr package (either regression or classification).}
  \item{model}{Any machine learning model.}
  \item{method}{A list of variable importance methods to be set by the user. These can include any of the importance methods contained within the mlr package. The default is method = randomForest.}
  \item{thresholdValue}{A value chosen by the user which will show all the edges with weights (i.e., the interacions) above that value. For example, if thresholdValue = 2, then only the the interacions greater than 2 will be displayed.}
  \item{cluster}{If cluster = TRUE then the data is clustered according to interactions.}
  }


\value{
Network style plot
}

\author{
Alan Inglis
}
