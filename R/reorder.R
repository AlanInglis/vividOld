# Matrix Reorder -----------------------------------------------------------
# -------------------------------------------------------------------------
#' reorderMatrix
#'
#' @description Used to reorder a square matrix so that values of high importance and
#' interaction strength are pushed to the top left of the matrix.
#'
#' @param matrix A matrix to be reordered.
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
#' Task = TaskRegr$new(id = "Fried", backend = myData, target = "y")
#' lrn = lrn("regr.ranger", importance = "permutation")
#' Mod <- lrn$train(Task)
#'
#' # Create matrix
#' myMat <- vividMatrix(task = Task, model = Mod, gridsize = 100, reorder = FALSE)
#'
#' myMat <- reorderMatrix(myMat)
#'
#' @importFrom DendSer "dser"
#'
#' @export

reorderMatrix <- function(matrix){
  vimp <- diag(matrix)
  vimp <- as.dist(sqrt(outer(vimp, vimp)))
  svimp <- diff(range(vimp))

  vint <- as.dist(matrix)
  svint <- diff(range(vint))
  o <- dser( -(vimp/svimp + vint/svint ), cost=costLPL)
  matrix <- matrix[o,o]
  class(matrix) <- c("vivid", class(matrix))
  matrix
}




