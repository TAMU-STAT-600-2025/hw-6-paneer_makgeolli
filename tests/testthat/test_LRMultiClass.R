library(testthat)
library(GroupHW)

test_that("LRMultiClass returns correct structure", {
  set.seed(123)
  n <- 100
  p <- 5
  K <- 3
  
  # Create design matrix with intercept column
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  
  # Generate class labels
  y <- sample(0:(K-1), n, replace = TRUE)
  
  # Run multi-class logistic regression
  result <- LRMultiClass(X, y, numIter = 20, eta = 0.1, lambda = 0.1)
  
  # Check return structure
  expect_type(result, "list")
  expect_named(result, c("beta", "objective"))
  
  # Check beta dimensions
  expect_equal(dim(result$beta), c(p, K))
  expect_true(is.matrix(result$beta))
  
  # Check objective dimensions
  expect_equal(length(result$objective), 21)  # numIter + 1
  expect_true(is.numeric(result$objective))
  
  # Check objective generally decreases (allow small increases due to numerical precision)
  # Most differences should be negative (decreasing), allow small positive differences
  obj_diff <- diff(result$objective)
  expect_true(sum(obj_diff < 0) > length(obj_diff) * 0.8)  # At least 80% should decrease
})

test_that("LRMultiClass works with different numbers of classes", {
  set.seed(456)
  
  # Test with 2 classes
  n <- 50
  p <- 4
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:1, n, replace = TRUE)
  result <- LRMultiClass(X, y, numIter = 10, eta = 0.1, lambda = 0.1)
  expect_equal(dim(result$beta), c(p, 2))
  expect_equal(length(result$objective), 11)
  
  # Test with 5 classes
  set.seed(789)
  n <- 150
  p <- 6
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:4, n, replace = TRUE)
  result <- LRMultiClass(X, y, numIter = 15, eta = 0.1, lambda = 1)
  expect_equal(dim(result$beta), c(p, 5))
  expect_equal(length(result$objective), 16)
})

test_that("LRMultiClass works with custom beta_init", {
  set.seed(123)
  n <- 80
  p <- 4
  K <- 3
  
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:(K-1), n, replace = TRUE)
  
  # Create custom initial beta
  beta_init <- matrix(rnorm(p * K), nrow = p, ncol = K)
  
  result <- LRMultiClass(X, y, beta_init = beta_init, numIter = 10, eta = 0.1, lambda = 0.1)
  
  expect_equal(dim(result$beta), c(p, K))
  expect_equal(length(result$objective), 11)
})

test_that("LRMultiClass handles different parameter values", {
  set.seed(123)
  n <- 60
  p <- 4
  K <- 3
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:(K-1), n, replace = TRUE)
  
  # Test with different eta values
  result1 <- LRMultiClass(X, y, numIter = 10, eta = 0.01, lambda = 0.1)
  result2 <- LRMultiClass(X, y, numIter = 10, eta = 0.5, lambda = 0.1)
  
  expect_equal(dim(result1$beta), c(p, K))
  expect_equal(dim(result2$beta), c(p, K))
  
  # Test with different lambda values
  result3 <- LRMultiClass(X, y, numIter = 10, eta = 0.1, lambda = 0.01)
  result4 <- LRMultiClass(X, y, numIter = 10, eta = 0.1, lambda = 10)
  
  expect_equal(dim(result3$beta), c(p, K))
  expect_equal(dim(result4$beta), c(p, K))
})

test_that("LRMultiClass objective function generally decreases", {
  set.seed(123)
  n <- 100
  p <- 5
  K <- 3
  
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:(K-1), n, replace = TRUE)
  
  result <- LRMultiClass(X, y, numIter = 30, eta = 0.1, lambda = 0.1)
  
  # Objective should generally decrease overall
  # Check that final objective is not much larger than initial (allow small increases)
  expect_true(result$objective[length(result$objective)] <= result$objective[1] + 0.1)
  
  # Most iterations should show decrease
  obj_diff <- diff(result$objective)
  expect_true(sum(obj_diff < 0) > length(obj_diff) * 0.7)  # At least 70% should decrease
})

test_that("LRMultiClass throws error for invalid X (no intercept column)", {
  set.seed(123)
  n <- 50
  p <- 4
  X <- matrix(rnorm(n * p), nrow = n)  # No intercept column
  y <- sample(0:2, n, replace = TRUE)
  
  expect_error(LRMultiClass(X, y), "First column of X must be all 1s for intercept")
})

test_that("LRMultiClass throws error for dimension mismatch", {
  set.seed(123)
  n <- 50
  p <- 4
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:2, 30, replace = TRUE)  # Wrong length
  
  expect_error(LRMultiClass(X, y), "Number of rows in X must match length of y")
})

test_that("LRMultiClass throws error for invalid eta", {
  set.seed(123)
  n <- 50
  p <- 4
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:2, n, replace = TRUE)
  
  expect_error(LRMultiClass(X, y, eta = -0.1), "eta must be positive")
  expect_error(LRMultiClass(X, y, eta = 0), "eta must be positive")
})

test_that("LRMultiClass throws error for invalid lambda", {
  set.seed(123)
  n <- 50
  p <- 4
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:2, n, replace = TRUE)
  
  expect_error(LRMultiClass(X, y, lambda = -1), "lambda must be non-negative")
})

test_that("LRMultiClass throws error for invalid class labels", {
  set.seed(123)
  n <- 50
  p <- 4
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  
  # Test with only one class
  y <- rep(0, n)
  expect_error(LRMultiClass(X, y), "Must have at least 2 classes")
  
  # Test with invalid class labels (negative)
  y <- c(-1, sample(0:2, n-1, replace = TRUE))
  expect_error(LRMultiClass(X, y), "Class labels must be between 0 and K-1")
  
  # Test with invalid class labels (too large)
  y <- sample(0:2, n, replace = TRUE)
  y[1] <- 5  # Invalid label
  expect_error(LRMultiClass(X, y), "Class labels must be between 0 and K-1")
})

test_that("LRMultiClass throws error for invalid beta_init dimensions", {
  set.seed(123)
  n <- 50
  p <- 4
  K <- 3
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:(K-1), n, replace = TRUE)
  
  # Wrong number of rows
  beta_init <- matrix(0, p+1, K)
  expect_error(LRMultiClass(X, y, beta_init = beta_init), 
               "beta_init must be a p x K matrix")
  
  # Wrong number of columns
  beta_init <- matrix(0, p, K+1)
  expect_error(LRMultiClass(X, y, beta_init = beta_init), 
               "beta_init must be a p x K matrix")
})

test_that("LRMultiClass works with larger datasets", {
  set.seed(123)
  n <- 500
  p <- 10
  K <- 4
  
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:(K-1), n, replace = TRUE)
  
  result <- LRMultiClass(X, y, numIter = 20, eta = 0.1, lambda = 1)
  
  expect_equal(dim(result$beta), c(p, K))
  expect_equal(length(result$objective), 21)
  expect_true(all(is.finite(result$beta)))
  expect_true(all(is.finite(result$objective)))
})

test_that("LRMultiClass produces consistent results with same seed", {
  set.seed(123)
  n <- 50
  p <- 4
  K <- 3
  X <- cbind(1, matrix(rnorm(n * (p-1)), nrow = n))
  y <- sample(0:(K-1), n, replace = TRUE)
  
  result1 <- LRMultiClass(X, y, numIter = 10, eta = 0.1, lambda = 0.1)
  
  set.seed(123)
  result2 <- LRMultiClass(X, y, numIter = 10, eta = 0.1, lambda = 0.1)
  
  # Results should be identical with same inputs
  expect_equal(result1$beta, result2$beta)
  expect_equal(result1$objective, result2$objective)
})

