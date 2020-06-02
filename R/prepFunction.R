# Prep Function -----------------------------------------------------------
# -------------------------------------------------------------------------

#' prepFunction
#'
#' @description Creates a matrix displaying Variable importance on the diagonal
#'  and Variable Interaction on the off-diagonal.
#'
#'
#' @param task Task created from the mlr package, either regression or classification.
#' @param model Any machine learning model.
#' @param ... Not currently implemented.
#'
#' @return A matrix of values
#'
#' @importFrom mlr "getTaskData"
#' @importFrom mlr "getFeatureImportance"
#' @importFrom mlr "getTaskFeatureNames"
#' @importFrom iml "Predictor"
#' @importFrom iml "Interaction"
#' @importFrom iml "FeatureImp"
#' @import progress
#'
#' @examples
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
#' # Create graph:
#' plotNetwork(task = aqRgrTask, model = aqMod,
#' thresholdValue = 0, cluster = F)
#'
#' @export


prepFunc <- function(task, model){

  message(" Calculating variable importance...")
  # get data:
  data <- getTaskData(task)

  # iml prediction
  mod <- Predictor$new(model, data = data)


  lrnID <- model$learner$id

  # If (embedded learner) - else(agnostic varimp calc)
  if(lrnID == "regr.randomForest" | lrnID == "regr.ranger" | lrnID == "regr.cforest" |
     lrnID == "regr.gbm" | lrnID == "regr.randomForestSRC" | lrnID == "regr.rpart" |
     lrnID == "regr.RRF" | lrnID == "regr.xgboost" |
     lrnID ==  "classif.randomForest" | lrnID == "classif.boosting" |
     lrnID == "classif.cforest" | lrnID == "classif.gbm" |
     lrnID ==  "classif.h2o.deeplearning" | lrnID == "classif.h2o.gbm" |
     lrnID == "classif.h2o.glm" | lrnID == "classif.h2o.randomForest" |
     lrnID == "classif.randomForestSRC" | lrnID == "classif.ranger" |
     lrnID == "classif.rpart" | lrnID == "classif.RRF" | lrnID == "classif.xgboost"){
    Importance <- getFeatureImportance(model)
    Importance <- Importance$res
    suppressMessages({
      Importance <- melt(Importance)
    })
    Imp <-  Importance$value
    ovars <- getTaskFeatureNames(task)
  }else{
    imp <- FeatureImp$new(mod, loss = "mse")
    Imp <- imp$results$importance
    ovars <- imp$results$feature
    print("Agnostic variable importance method used.")
  }




  # Create progress bar
  pb <- progress_bar$new(
    format = "  Calculating variable interactions...[:bar]:percent. Est::eta ",
    total = length(ovars),
    clear = FALSE)

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
  dinteraction <- (dinteraction+t(dinteraction))/2                          # avg over values to make symmetrical
  dinteraction1 <- data.frame(interaction=as.vector(dinteraction))
  diag(dinteraction) <- Imp
  dinteraction
  #dinteraction <<- dinteraction
}

