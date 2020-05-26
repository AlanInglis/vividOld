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
#' @importFrom mlr "getTaskData"
#' @importFrom mlr "normalizeFeatures"
#' @importFrom mlr "generateFilterValuesData"
#' @importFrom mlr "getTaskFeatureNames"
#' @importFrom mlr "makeLearner"
#' @importFrom mlr "train"
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

  # if(type != "lollipop" || type != "barplot" || type != "circleBar"){
  #   stop("Invalid plotting type. See ?allInt for available plotting types")
  # }
  message(" Initilizing...")

  data <- getTaskData(task)

  mod <- Predictor$new(model, data = data) # create iml model


# -------------------------------------------------------------------------
#                         FEATURE SELECTION
# -------------------------------------------------------------------------
# This section deals with removing features with low interaction strength:


  intValues <- Interaction$new(mod) # Overall interaction strength
  intVal <- intValues$results # get interaction results
  a <- intVal
  a[,".feature"] <- as.factor(a[,".feature"])
  a <- a[with(a,order(.interaction, decreasing = T)),] #reordering


  n <- nrow(a) # Number of rows in a
  percent_variables_remove = 0.1 # percentage of variables that you want to remove
  n_begin = n - round(n*percent_variables_remove) # Getting the indices of those variables with the lowest variable interactions
  variables_remove = a[n_begin:n,1]

  dat <- data[,-which(names(data) %in% variables_remove)] # Removing the columns here

  varChar <- as.character(variables_remove)
  task1 <- dropFeatures(task, c(varChar)) # drop the features from the task


  # Re-learn/train the mlr model
  lrn.ID <- model$learner$id
  lnr <- makeLearner(lrn.ID)
  mlrMod <- train(lnr, task1)


# -------------------------------------------------------------------------
# Recreate iml model/interaction


  mod1 <- Predictor$new(mlrMod, data = dat) # make predictions on new data

  ovars <- getTaskFeatureNames(task1)
  nam <- getTaskFeatureNames(task1)

  # Create progress bar
  pb <- progress_bar$new(
    format = "  Calculating variable interactions...[:bar]:percent. Estimated completion time::eta ",
    total = length(ovars),
    clear = FALSE)

  res  <- NULL
  for (i in 1:length(ovars)){
    res <- rbind(res, Interaction$new(mod1, grid.size = 10, feature=ovars[i])$results)
    pb$tick()
  }

  res[[".feature"]]<- reorder(res[[".feature"]], res[[".interaction"]])

  vars2 <- t(simplify2array(strsplit(as.character(res[[".feature"]]),":"))) # split/get feature names
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[[".interaction"]]                              # set values
  dinteraction <- (dinteraction+t(dinteraction))/2   # avg over values to make symmetrical

  df <- transform(data.frame(dinteraction), y=row.names(dinteraction))

  # Reshape long
  long_df <- reshape(df, varying = colnames(dinteraction), times = colnames(dinteraction),
                     timevar="x", v.names="value", direction="long")

  # Order values
  long_df <- with(long_df, long_df[order(value),])
  long_df$xy <- with(long_df, paste(x, y, sep=":"))

  # Select correct data
  df <- long_df[,c(3,5)]

  # Remove zeros
  df<-df[!apply(df[,1:2] == 0, 1, FUN = any, na.rm = TRUE),]

  # Show only top X results
  if(top > 0){
    df <- tail(df,top)
  }
# -------------------------------------------------------------------------
# Plotting
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
