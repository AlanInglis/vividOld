# Matrix Reorder -----------------------------------------------------------
# -------------------------------------------------------------------------
#' reorderMatrix
#'
#' @description Used to reorder a square matrix so that values of high importance and
#' interaction strength are pushed to the top left of the matrix.
#'
#' @param d A matrix to be reordered.
#'
#' @return A matrix of reordered values
#'
#' @importFrom DendSer "dser"
#'
#' @examples
#' library(vivid)
#'
#' # Create data
#' myData <- genFriedman(10, 100)
#'
#' # Run an mlr ranger model:
#' library(mlr3)
#' library(mlr3learners)
#' library(ranger)
#'
#' Task <- TaskRegr$new(id = "Fried", backend = myData, target = "y")
#' lrn <- lrn("regr.ranger", importance = "permutation")
#' Mod <- lrn$train(Task)
#'
#' # Create matrix
#' myMat <- vividMatrix(task = Task, model = Mod, gridsize = 100, reorder = FALSE)
#'
#' myMat <- reorderMatrix(myMat)
#' @importFrom DendSer "dser"
#'
#' @export

reorderMatrix <- function(d) {
  vimp <- diag(d)
  rvimp <- range(vimp)
  if (rvimp[2] != rvimp[1]) {
    vimp <- (vimp - rvimp[1]) / (rvimp[2] - rvimp[1])
  }
  vint <- as.dist(d)
  rvint <- range(vint)
  if (rvint[2] != rvint[1]) {
    vint <- (vint - rvint[1]) / (rvint[2] - rvint[1])
  }
  score <- apply(as.matrix(vint), 1, max) + vimp
  o <- dser(-vint, -score, cost = costLS)
  res <- d[o, o]
  class(res) <- c("vivid", "matrix", "array")
  res
}
