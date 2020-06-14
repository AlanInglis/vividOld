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
#' @param remove If TRUE then remove the variables with low interaction strength.
#' @param percentRemove The percentage of variables with low interaction strength to remove from the interaction calculation.
#' @param parallel If TRUE then the method is executed in parallel.
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
#' @importFrom doParallel "registerDoParallel"
#' @importFrom parallel "detectCores"
#' @importFrom parallel "makeCluster"
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


prepFunc <- function(task, model, remove = FALSE, percentRemove = 0.5, parallel){

  message(" Calculating variable importance...")
  # get data:
  data <- getTaskData(task)

  # iml prediction
  mod <- Predictor$new(model, data = data)


  ## Get Variable Importance:
  lrnID <- model$learner$properties
  testString <- "featimp"


  for(i in length(lrnID)){
    logID <- grepl(lrnID[i], testString, fixed = TRUE)
  }

  # If (embedded learner) - else(agnostic varimp calc)
  if(logID == TRUE){
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
  percent_variables_remove = percentRemove # percentage of variables that you want to remove
  n_begin = n - round(n*percent_variables_remove) # Getting the indices of those variables with the lowest variable interactions
  variables_remove = a[n_begin:n,1]


  ovars <- getTaskFeatureNames(task)

  # Set up registered cluster for parallel
  if(parallel){
  defaultMaxCoreNo = detectCores(logical = FALSE)
  cl <- makeCluster(defaultMaxCoreNo)
  registerDoParallel(cl)
  }

  if(remove == TRUE){

    intValues <- Interaction$new(mod, parallel = parallel) # Overall interaction strength
    intVal <- intValues$results # get interaction results
    a <- intVal
    a[,".feature"] <- as.factor(a[,".feature"])
    a <- a[with(a,order(.interaction, decreasing = T)),] #reordering


    n <- nrow(a) # Number of rows in a
    percent_variables_remove = 0.5 # percentage of variables that you want to remove
    n_begin = n - round(n*percent_variables_remove) # Getting the indices of those variables with the lowest variable interactions
    variables_remove = a[n_begin:n,1]

    ovars1 <- ovars[-which(ovars %in% variables_remove)]

    # Create progress bar
    pb <- progress_bar$new(
      format = "  Calculating variable interactions...[:bar]:percent. ETA::eta ",
      total = length(ovars1),
      clear = FALSE)

    res  <- NULL
    for (i in 1:length(ovars1)){
      res <- rbind(res, Interaction$new(mod, grid.size = 10, parallel = parallel, feature=ovars1[i])$results)
      pb$tick()
    }

    res[[".feature"]]<- reorder(res[[".feature"]], res[[".interaction"]])

  }else{
    # Create progress bar
    pb <- progress_bar$new(
      format = "  Calculating variable interactions...[:bar]:percent. ETA::eta ",
      total = length(ovars),
      clear = FALSE)

    res  <- NULL
    for (i in 1:length(ovars)){
      res <- rbind(res, Interaction$new(mod, grid.size = 10, parallel = parallel, feature=ovars[i])$results)
      pb$tick()
    }

    res[[".feature"]]<- reorder(res[[".feature"]], res[[".interaction"]])
  }

  vars2 <- t(simplify2array(strsplit(as.character(res[[".feature"]]),":"))) # split/get feature names
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[[".interaction"]]                              # set values
  dinteraction <- (dinteraction+t(dinteraction))/2   # avg over values to make symmetrical
  dinteraction1 <- data.frame(interaction=as.vector(dinteraction))
  diag(dinteraction) <- Imp
  dinteraction
}

