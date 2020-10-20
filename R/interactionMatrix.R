# Prep Function -----------------------------------------------------------
# -------------------------------------------------------------------------

#' vividMatrix
#'
#' @description Creates a matrix displaying Variable importance on the diagonal
#'  and Variable Interaction on the off-diagonal.
#'
#'
#' @param model Any machine learning model.
#' @param data A data.frame to be supplied
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param normalize Should the variances explained be normalized? Default is FALSE.
#' @param n_max Maximum number of data rows to consider.
#' @param seed An integer random seed used for subsampling.
#' @param sqrt In order to reproduce Friedman's H statistic, resulting values are root transformed. Set to FALSE if squared values should be returned.
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
#' aq_Task = TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn = lrn("regr.ranger", importance = "permutation")
#' aq_Mod <- aq_lrn$train(aq_Task)
#'
#' # Create matrix
#' myMat <- vividMatrix(model = aq_Mod)
#'
#' # Create graph:
#' plot(myMat, type = "heatMap")
#'
#' @export



vividMatrix <- function(model, data, gridSize = 10, normalize = FALSE, n_max = 1000,
                        seed = NULL, sqrt = FALSE,...){

    FLobject <- flash(model, data)
    fl <- FLobject

    vImp <- varImportanceFL(fl, model)

    flInt <- FLfunc(fl, model, data, gridSize = gridSize,  normalize = normalize, n_max = n_max,
                    seed = seed, sqrt = sqrt)

    diag(flInt) <- vImp

    class(flInt) <- c("vivid", class(flInt))
    return(flInt)
}


# -------------------------------------------------------------------------
# Create flashlight
flash <- function(model, data){

  response <- model$state$train_task$target_names
  fl <- flashlight(model = model, data = data, y = response, label = "")

  return(fl)
}


# -------------------------------------------------------------------------
# Variable importance
varImportanceFL <- function(fl, model){
    message(" Calculating variable importance...")

    ## Get Variable Importance:
    lrnID <- model$properties
    testString <- "importance"

    logID <- logical(length(lrnID))
    for(i in seq_along(lrnID)){
      logID[i] <- grepl(lrnID[i], testString, fixed = TRUE)
    }

    # If (embedded learner) - else(agnostic varimp calc)
    if(any(logID) == TRUE){
      ovars <- model$state$train_task$feature_names
      Importance <- model$importance()
      impReorder <- Importance[order(factor(names(Importance), levels = ovars))]
      suppressMessages({
        Importance <- reshape2::melt(impReorder)
      })
      imp <-  Importance$value
    }else{
      # Importance Values:
      imp <- light_importance(fl, m_repetitions = 4)
      imp <- imp$data$value
      print("Agnostic variable importance method used.")
    }
    return(imp)
}



# -------------------------------------------------------------------------
# Flashlight Interactions

FLfunc <- function(fl, model, data, gridSize = gridSize, normalize = normalize, n_max = n_max,
                   seed = seed, sqrt = sqrt){

    message("Calculating interactions...")

  if (!is.null(seed)) {
    set.seed(seed)
  }else{seed = NULL}

    # Interaction Matrix:
    res  <- NULL
    ovars <- model$state$train_task$feature_names

    res <- light_interaction(fl, pairwise = TRUE, type = "H", grid_size = gridSize,
                             normalize = normalize, n_max = n_max,
                             seed = seed, sqrt = sqrt)$data

    res[["variable"]]<- reorder(res[["variable"]], res[["value"]])

    vars2 <- t(simplify2array(strsplit(as.character(res[["variable"]]),":"))) # split/get feature names
    dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
    rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
    dinteraction[vars2] <- res[["value"]]                                     # set values
    dinteraction[lower.tri(dinteraction)] = t(dinteraction)[lower.tri(dinteraction)]
    dinteraction[is.nan(dinteraction)] <- 0.000000001
    dinteraction[(dinteraction <= 0)] <- 0.000000001
    dinteraction
}


