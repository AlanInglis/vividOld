\name{intHeatmap}
\alias{intHeatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a Heatmap style plot displaying Variable Importance and Variable Interaction
}
\description{
Plots a heatmap of the interaction strength with variable importance on the diagonal.
}
\usage{
intHeatmap(task, model, method, plotly, intLow, intHigh, impLow, impHigh)
}

\arguments{
  \item{task}{Task created from the mlr package (either regression or classification).}
  \item{model}{Any machine learning model}
  \item{method}{A list of variable importance methods to be set by the user. These can include any of the importance methods contained within the mlr package. The default is method = randomForest.}
  \item{plotly}{If plotly = TRUE, an interactive plotly object is displayed where the user can hover the mouse icon over any part of the plot and a text box will appear displaying information related to the selected variables.}
  \item{intLow}{The interaction strength values are displayed using a colour gradient. intLow is a colour, declared by the user, which is assigned to low interaction strength values.}
  \item{intHigh}{intHigh is the colour assigned to high interaction strength values.}
  \item{impLow}{impLow represents the low values of the colour gradient used to display the individual variable importance.}
  \item{impHigh}{impHigh represents the high values of the colour gradient used to display the individual variable importance.}
}

\value{A heatmap style plot
}

\author{
Alan Inglis
}
