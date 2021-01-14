# Prep Function -----------------------------------------------------------
# -------------------------------------------------------------------------

#' vividMatrix
#'
#' @description Creates a matrix displaying Variable importance on the diagonal
#'  and Variable Interaction on the off-diagonal.
#'
#' @param task Task created from the mlr3 package, either regression or classification.
#' @param model A machine learning model created from mlr3 task and learner.
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param normalize Should the variances explained be normalized? Default is FALSE.
#' @param n_max Maximum number of data rows to consider.
#' @param sqrt In order to reproduce Friedman's H statistic, resulting values are root transformed. Set to FALSE if squared values should be returned.
#' @param reorder If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances.
#' @param main Define main category for classification.
#'
#' @return A matrix of values
#'
#' @importFrom flashlight "flashlight"
#' @importFrom flashlight "light_importance"
#' @importFrom flashlight "light_interaction"
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
#' aq_Task <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn <- lrn("regr.ranger", importance = "permutation")
#' aq_Mod <- aq_lrn$train(aq_Task)
#'
#' # Create matrix
#' myMat <- vividMatrix(task = aq_Task, model = aq_Mod)
#'
#' # Create graph:
#' plot(myMat, type = "heatMap")
#'
#' @export



vividMatrix <- function(task, model, filter = NULL, gridSize = 10, normalize = FALSE, n_max = 1000,
                         sqrt = TRUE, reorder = TRUE, main = NULL, ...){

  # if classif
  if(model$task_type == "classif"){


    fl <- flashClassif(task, model, main)


    vImp <- varImportanceFL(fl, task, model, filter)

    flInt <- FLfuncClassif(fl, task, model, gridSize = gridSize,  normalize = normalize, n_max = n_max,
                            sqrt = sqrt)
    message("NOTE: The measured variable importance for
               prediciting ", task$target_names, " is using all variables. Not
               just ", main)
  }else{
    fl <- flashRegr(task, model)

    vImp <- varImportanceFL(fl, task, model, filter)

    flInt <- FLfunc(fl, task, model, gridSize = gridSize,  normalize = normalize, n_max = n_max,
                     sqrt = sqrt)

  }

  flInt[is.nan(flInt)] <- 0
  flInt[is.na(flInt)] <- 0
  flInt[(flInt <= 0)] <- 0

  if(length(colnames(flInt)) == length(vImp)){
    diag(flInt) <- vImp
  }else{vImp <- vImp[1:length(colnames(flInt))]
  diag(flInt) <- vImp}

  if (reorder){
    flInt <- reorderMatrix(flInt)
  }

  class(flInt) <- c("vivid", class(flInt))
  return(flInt)
}




# -------------------------------------------------------------------------
# Create flashlight
flashRegr <- function(task, model){

  data <-  task$data()
  data <- as.data.frame(data)
  target <- task$target_names

  fl <- flashlight(model = model, data = data, y = target, label = "")

  return(fl)
}

flashClassif <- function(task, model, main){

  data <-  task$data()
  data <- as.data.frame(data)
  target <- task$target_names

  fl <- flashlight(
    model = model,
    data = data,
    label = "",
    predict_function = function(m, X) predict(m, X) == main)
}


# -------------------------------------------------------------------------
# Variable importance
varImportanceFL <- function(fl, task, model, filter){
  message("Calculating variable importance...")


  ## Get Variable Importance:
  lrnID <- model$properties
  testString <- "importance"
  target <-  model$state$train_task$target_names

  logID <- logical(length(lrnID))
  for(i in seq_along(lrnID)){
    logID[i] <- grepl(lrnID[i], testString, fixed = TRUE)
  }

  if(is.null(filter)){
    # If (embedded learner) - else(agnostic varimp calc)
    if(any(logID) == TRUE){
      ovars <- model$state$train_task$feature_names
      Importance <- model$importance()
      impReorder <- Importance[order(factor(names(Importance), levels = ovars))]
      suppressMessages({
        Importance <- reshape2::melt(impReorder)
      })
      imp <-  Importance$value
      message("Embedded variable importance method used.")
    }else{
      # Importance Values:
      imp <- light_importance(fl, m_repetitions = 4)
      imp <- imp$data$value
      message("Agnostic variable importance method used.")
    }
  }else{
    ovars <- model$state$train_task$feature_names

    fltImp <- as.data.table(filter$calculate(task))

    fltImp$feature = factor(fltImp$feature, levels = ovars)
    fltImp <- fltImp[order(fltImp$feature), ]

    imp <- fltImp$score
    message("Filter variable importance method used.")
  }

  return(imp)
}



# -------------------------------------------------------------------------
# Flashlight Interactions

FLfunc <- function(fl, task, model, gridSize = gridSize, normalize = normalize, n_max = n_max,
                    sqrt = sqrt){

  message("Calculating interactions...")
  # Interaction Matrix:
  res  <- NULL
  ovars <- model$state$train_task$feature_names



  res <- light_interaction(fl, pairwise = TRUE, type = "H", grid_size = gridSize,
                           normalize = normalize, n_max = n_max,
                            sqrt = sqrt)$data





  res[["variable"]]<- reorder(res[["variable"]], res[["value"]])

  vars2 <- t(simplify2array(strsplit(as.character(res[["variable"]]),":"))) # split/get feature names
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[["value"]]                                     # set values
  dinteraction[lower.tri(dinteraction)] = t(dinteraction)[lower.tri(dinteraction)]
  dinteraction
}


# -------------------------------------------------------------------------
# For classification

FLfuncClassif <- function(fl, task, model, gridSize = gridSize, normalize = normalize, n_max = n_max,
                           sqrt = sqrt){

  message("Calculating interactions...")

  # Interaction Matrix:
  res  <- NULL
  ovars <- model$state$train_task$feature_names
  target <- task$target_names

  res <- light_interaction(fl, pairwise = TRUE, type = "H", grid_size = gridSize,
                           normalize = normalize, n_max = n_max,
                            sqrt = sqrt)$data

  ## Removing rows containing target and adding df back to FL object

  # get data
  res_edit <- res

  # remove target
  res_edit <- res_edit[!grepl(target, res_edit$variable),]

  # add back into FL object
  res <- res_edit

  res[["variable"]]<- reorder(res[["variable"]], res[["value"]])

  vars2 <- t(simplify2array(strsplit(as.character(res[["variable"]]),":"))) # split/get feature names
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[["value"]]                                     # set values
  dinteraction[lower.tri(dinteraction)] = t(dinteraction)[lower.tri(dinteraction)]
  dinteraction
}

