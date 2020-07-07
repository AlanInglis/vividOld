# Test the allInteractions function
library(mlr3)
library(mlr3learners)

# Function to simulate freidman data
sim_friedman = function(n, p = 0, res_sd = 1, pars = c(10, 20, 10, 5)) {
  # Simulate some data using a multivariate version of Friedman
  # y = 10sin(πx1x2) + 20(x3−0.5)^2 + 10x4 + 5x5 + ε
  X = matrix(runif(n * (5 + p), 0, 1), nrow = n, ncol = 5 + p)
  err = rnorm(n, mean = 0, sd = res_sd)
  mean = pars[1] * sin(pi*X[,1]*X[,2]) + pars[2] * (X[,3]-0.5)^2 + pars[3] * X[,4] + pars[4] * X[,5]
  y = mean + err
  df = data.frame(y,
                  X)
  return(df)
}
#sim_friedman(100)

# Create some data
my_data = sim_friedman(200)



# -------------------------------------------------------------------------

# Regression
# Run it through mlr3
fr_task = TaskRegr$new(id = "Fried", backend = my_data, target = "y")
fr_learn = lrn("regr.ranger", importance = "permutation")
fr_mod = fr_learn$train(fr_task)


# Create Matrix
myMat <- prepFunc(fr_task, fr_learn, fr_mod)

# Test that the basic function works for regression
test_that("allInt works", {
  expect_list(allInt(myMat))
  expect_list(allInt(myMat, type = 'barplot'))
  expect_list(allInt(myMat, type = 'circleBar'))
  expect_list(allInt(myMat, top = 2))
})


# -------------------------------------------------------------------------
# classification
ir <- iris
# Run it through mlr
ir_task = TaskClassif$new(id = "iris", backend = ir, target = "Species")
ir_learn = lrn("classif.ranger", importance = "impurity", predict_type = "prob")
ir_mod = ir_learn$train(ir_task)

# Create matrix
myMat1 <- prepFunc(ir_task, ir_learn, ir_mod)
# Test that the basic function works for classification
test_that("allInt works", {
  expect_list(allInt(myMat1))
  expect_list(allInt(myMat1, type = 'barplot'))
  expect_list(allInt(myMat1, type = 'circleBar'))
  expect_list(allInt(myMat1, top = 2))
})


# -------------------------------------------------------------------------
# test other learners e.g. linear regression to make sure that interactions are zero
fr_task = TaskRegr$new(id = "Fried", backend = my_data, target = "y")
fr_learn = lrn("regr.lm")
fr_mod = fr_learn$train(fr_task)

# Create matrix

# myMat <- prepFunc(fr_task, fr_learn, fr_mod)
# test_that("lm interactions = 0", {
#   aInt <- allInt(myMat)
#   aInt <- aInt$data$value
#   expect_equal(sum(aInt), 0)
# })







