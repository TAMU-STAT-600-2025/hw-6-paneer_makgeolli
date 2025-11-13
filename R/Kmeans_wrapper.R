#' K-Means Clustering (Rcpp Ver.)
#' 
#' Performs K-means clustering on a numeric matrix. 
#' It validates input, initializes centroids (if not already given),
#' and calls a C++ implementation. 
#'
#' @param X A numeric matrix (or dataframe) of size n x p, where n is the 
#'  number of observations and p the number of features. 
#' @param K Integer. Represents the number of clusters to form, 
#'  must be positive and less than n.
#' @param M (Optional) numeric matrix of size K x p. Specifies initial cluster centroids.
#'  If NULL, centroids are initialized by randomly selecting K rows from X.
#' @param numIter Integer. The maximum number of iterations to run (default = 100). 
#'
#' @return An integer vector of length n. Gives the cluster assignment for each observation.
#' @export
#'
#' @examples
#' set.seed(123)
#' # Create a simple 2D dataset with 3 clusters
#' X <- rbind(
#'   matrix(rnorm(100, mean = 0, sd = 0.5), ncol = 2),
#'   matrix(rnorm(100, mean = 3, sd = 0.5), ncol = 2),
#'   matrix(rnorm(100, mean = 6, sd = 0.5), ncol = 2)
#' )
#'
#' # Run custom K-means with random initialization
#' Y <- MyKmeans(X, K = 3)
#' table(Y)
#'
#' # Use custom initialization
#' init_M <- X[sample(1:nrow(X), 3), ]
#' Y2 <- MyKmeans(X, K = 3, M = init_M)
#' table(Y2)
MyKmeans <- function(X, K, M = NULL, numIter = 100){
  # Validate and convert input matrix X
  # Convert data.frame to matrix if needed
  if (is.data.frame(X)) X <- data.matrix(X)
  # Check that X is a 2D numeric matrix
  if(!(is.matrix(X) && length(dim(X)) == 2)) {
    stop("X is not a 2D matrix.")
  }
  
  # Extract dimensions
  n <- nrow(X)
  p <- ncol(X)
  
  # Validate K: must be a single positive integer less than n
  if (!(length(K) == 1L && is.numeric(K) && K == as.integer(K) && K > 0L && K < n)) {
    stop("K must be a positive integer less than n.")
  }
  
  # Validate and initialize cluster centroids M
  # Convert data.frame to matrix if needed
  if (is.data.frame(M)) M <- data.matrix(M)
  
  if(is.null(M)){
    # Random initialization: select K distinct rows from X
    idx <- sample.int(n, size = K, replace = FALSE)
    M <- X[idx, , drop=FALSE]
  }
  else if (is.matrix(M)) {
    # Validate dimensions of provided M
    if (nrow(M) != K || ncol(M) != p) {
      stop("M must be a K x p matrix compatible with X and K.")
    }
  }
  else {
    stop("M must be NULL or a numeric 2D matrix.")
  }
  
  # Call C++ implementation to perform K-means clustering
  Y <- MyKmeans_c(X, K, M, numIter)
  
  # Return cluster assignments
  return(Y)
}