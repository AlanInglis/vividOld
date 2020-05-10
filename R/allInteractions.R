## This Script displays 2-way variable interaction

#' allInt
#'
#' @description Plots a Heatmap-tyle display showingVariable Importance and Variable Interaction
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param type The type of plot to display, either "lollipop", "barplot", or "circleBar"
#' @param top A value set by the user to only display the top x amount variables
#' @param ... Not currently implemented
#'
#'
#' @importFrom mlr "getTaskData"
#' @importFrom mlr "normalizeFeatures"
#' @importFrom mlr "generateFilterValuesData"
#' @importFrom mlr "getTaskFeatureNames"
#' @importFrom iml "Predictor"
#' @importFrom iml "Interaction"
#' @importFrom ggplot2 "ggplot"
#' @import progress
#'
#'@examples
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
#' allInt(aqRgrTask, aqMod)
#'
#' @export


# Function ----------------------------------------------------------------

allInt <- function(task, model, type = "lollipop", top = 0, ...){

  data <- getTaskData(task)
  nam <- getTaskFeatureNames(task)

  mod <- Predictor$new(model, data = data)
  res <- NULL
  ovars <- nam

  # Create progress bar
  pb <- progress_bar$new(
    format = "  Calculating variable interactions...[:bar]:percent. Estimated completion time::eta ",
    total = length(ovars),
    clear = FALSE,
    width= 0.8*options()$width)

  res  <- NULL
  for (i in 1:length(ovars)){
    res <- rbind(res, Interaction$new(mod, grid.size = 10, feature=ovars[i])$results)
    pb$tick()
  }

  res[[".feature"]]<- reorder(res[[".feature"]], res[[".interaction"]])

  vars2 <- t(simplify2array(strsplit(as.character(res[[".feature"]]),":"))) # split/get feature names
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[[".interaction"]]                              # set values
  dinteraction <- (dinteraction+t(dinteraction))/2   # avg over values to make symmetrical

  df <- transform(data.frame(dinteraction), y=row.names(dinteraction))

  # RESHAPE LONG
  long_df <- reshape(df, varying = colnames(dinteraction), times = colnames(dinteraction),
                     timevar="x", v.names="value", direction="long")

  # ORDER VALUES
  long_df <- with(long_df, long_df[order(value),])
  long_df$xy <- with(long_df, paste(x, y, sep=":"))

  # SELECT THE DATA I WANT
  df <- long_df[,c(3,5)]

  # REMOVE ZEROS
  df<-df[!apply(df[,1:2] == 0, 1, FUN = any, na.rm = TRUE),]



  if(top > 0){
    res <- tail(res,top)
  }

  if(type == "barplot"){
  # barplot
    pp <-  ggplot(df, aes(x = reorder(xy, value), y = value)) +
      geom_col(aes(fill = value)) +
      scale_fill_gradient(low = "floralwhite",
                          high = "dodgerblue4") +
      theme_minimal() +
      xlab('Features ') +
      ylab("Interaction Strength") +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
      coord_flip()

    pp
  return(pp)
  }else if(type == "circleBar"){
  # Circle barplot
  ppp <- ggplot(df, aes(x = reorder(xy, value), y = value)) +
    geom_col(aes(fill = value)) +
    scale_fill_gradient2(low = "floralwhite",
                         high = "dodgerblue4") +
    # Limits of the plot. The negative value controls the size of the inner circle,
    # the positive is to add size over each bar
    ylim(-0.5,0.3) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.margin = unit(rep(-2,4), "cm")) +    # This remove unnecessary margin around plot
    coord_polar(start = 0) + # This makes the coordinate polar instead of cartesian.
    geom_text(aes(label = round(value,3)), vjust = 2, color = "black", size = 3.5)
    #geom_text(aes(label = intRound),vjust = 0, color = "black", size = 3)
  return(ppp)
  }else if(type == "lollipop"){
    # lollipop plot
    p <- ggplot(df, aes(x = reorder(xy, value), y = value)) +
      geom_linerange(ymin=0, aes(ymax=value)) + geom_point()+
      xlab('Features') +
      ylab("Interaction Strength") +
      coord_flip() +
      theme_bw()
    return(p)
    }
}
