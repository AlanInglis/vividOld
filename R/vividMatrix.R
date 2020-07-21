# Prep Function -----------------------------------------------------------
# -------------------------------------------------------------------------

#' vividMatrix
#'
#' @description Creates a matrix displaying Variable importance on the diagonal
#'  and Variable Interaction on the off-diagonal.
#'
#'
#' @param task Task created from the mlr3 package, either regression or classification.
#' @param model Any machine learning model.
#' @param remove If TRUE then remove the variables with low interaction strength.
#' @param percentRemove The percentage of variables with low interaction strength to remove from the interaction calculation.
#' @param parallel If TRUE then the method is executed in parallel.
#'
#' @return A matrix of values
#'
#' @importFrom iml "Predictor"
#' @importFrom iml "Interaction"
#' @importFrom iml "FeatureImp"
#' @importFrom future "plan"
#' @importFrom reshape "melt"
#' @import progress
#'
#' @examples
#' # Load in the data:
#' aq <- data.frame(airquality)
#' aq <- na.omit(aq)
#'
#' # Run an mlr ranger model:
#' library(mlr3)
#' library(mlr3learners)
#' library(ranger)
#' aq_Task = TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn = lrn("regr.ranger", importance = "permutation")
#' aq_Mod <- aq_lrn$train(aq_Task)
#'
#' # Create matrix
#' myMat <- vividMatrix(task = aq_Task, model = aq_Mod)
#'
#' # Create graph:
#' plot(myMat, type = "heatMap")
#'
#' @export



vividMatrix <- function(task, model, remove = FALSE, percentRemove = 0.5, parallel = FALSE,
                     ...){
  message(" Calculating variable importance...")


  # get data:
  data <-  task$data()
  data <- as.data.frame(data)
  target <- task$target_names
  # iml prediction
  mod <- Predictor$new(model, data = data, y = target)


  ## Get Variable Importance:
  lrnID <- model$properties
  testString <- "importance"

  logID <- logical(length(lrnID))
  for(i in seq_along(lrnID)){
    logID[i] <- grepl(lrnID[i], testString, fixed = TRUE)
  }

  # If (embedded learner) - else(agnostic varimp calc)
  if(any(logID) == TRUE){
   ovars <- task$feature_names
   Importance <- model$importance()
   impReorder <- Importance[order(factor(names(Importance), levels = ovars))]
    suppressMessages({
      Importance <- melt(impReorder)
    })
    Imp <-  Importance$value
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

  # Set up registered cluster for parallel
  if(parallel){
    # noOfCores <- detectCores()
    # cl <- makeCluster(noOfCores)
    # registerDoParallel(cl)
    plan(future::cluster)
  }


  ovars <- task$feature_names
  if(remove){
    suppressMessages({
    intValues <- Interaction$new(mod) # Overall interaction strength
    })
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
      suppressMessages({
      res <- rbind(res, Interaction$new(mod, grid.size = 10, feature=ovars1[i])$results)
      })
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
      suppressMessages({
      res <- rbind(res, Interaction$new(mod, grid.size = 10, feature=ovars[i])$results)
      })
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

  #class(dinteraction) = 'vivid'
  class(dinteraction) <- c("vivid", class(dinteraction))

  dinteraction
}

