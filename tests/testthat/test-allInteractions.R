# Test the allInteractions function
library(mlr)

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
# Run it through mlr
fr_task = makeRegrTask(data = my_data, target = "y")
fr_learn = makeLearner("regr.randomForest")
fr_mod = mlr::train(fr_learn, fr_task)

# Test that the basic function works for regression
test_that("allInt works", {
  expect_list(allInt(task = fr_task, model = fr_mod))
  expect_list(allInt(task = fr_task, model = fr_mod, type = 'barplot'))
  expect_list(allInt(task = fr_task, model = fr_mod, type = 'circleBar'))
  expect_list(allInt(task = fr_task, model = fr_mod, top = 2))
})


# -------------------------------------------------------------------------
# classification
ir <- iris
# Run it through mlr
ir_task = makeClassifTask(data = ir, target = "Species")
ir_learn = makeLearner("classif.randomForest", predict.type = 'prob')
ir_mod = mlr::train(ir_learn, ir_task)

# Test that the basic function works for classification
test_that("allInt works", {
  expect_list(allInt(task = ir_task, model = ir_mod))
  expect_list(allInt(task = ir_task, model = ir_mod, type = 'barplot'))
  expect_list(allInt(task = ir_task, model = ir_mod, type = 'circleBar'))
  expect_list(allInt(task = ir_task, model = ir_mod, top = 2))
})


# -------------------------------------------------------------------------
# test other learners e.g. linear regression to make sure that interactions are zero
fr_task = makeRegrTask(data = my_data, target = "y")
fr_learn = makeLearner("regr.lm")
fr_mod = mlr::train(fr_learn, fr_task)


test_that("lm interactions = 0", {
  aInt <- allInt(task = fr_task, model = fr_mod)
  aInt <- aInt$data$value
  expect_equal(sum(aInt), 0)
})







