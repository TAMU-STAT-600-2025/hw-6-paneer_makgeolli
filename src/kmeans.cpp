// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::uvec MyKmeans_c(const arma::mat& X, int K,
                            const arma::mat& M, int numIter = 100){
    // All input is assumed to be correct
    
    // Extract dimensions
    int n = X.n_rows;
    int p = X.n_cols;
    arma::uvec Y(n); // to store cluster assignments
    
    // Precompute squared norms for X (constant across iterations)
    // This avoids recomputing ||x_i||^2 in each iteration
    arma::vec x2 = arma::sum(arma::square(X), 1);
    
    // Working copy of centroids (will be updated each iteration)
    arma::mat curM = M;
    
    // K-means algorithm iteration
    // Stops when: (i) centroids converge, (ii) max iterations reached, or (iii) empty cluster detected
    for (int iter = 0; iter < numIter; iter++) {
      // Compute squared norms for current centroids
      arma::vec m2 = arma::sum(arma::square(curM), 1);
      
      // Compute squared distances: D2[i,j] = ||x_i||^2 + ||m_j||^2 - 2*x_i*m_j^T
      // Optimized: avoid repmat by using broadcasting with each_col/each_row
      arma::mat D2(n, K);
      for (int j = 0; j < K; j++) {
        D2.col(j) = x2 + m2(j) - 2 * (X * curM.row(j).t());
      }
      
      // Assign each point to nearest centroid (1-indexed for R compatibility)
      Y = arma::index_min(D2, 1) + 1;
      
      // Count points in each cluster and check for empty clusters
      arma::uvec counts(K, arma::fill::zeros);
      for (int i = 0; i < n; i++) {
        counts[Y[i] - 1]++;
      }
      if (arma::any(counts == 0)) {
        Rcpp::stop("Empty Cluster Detected.");
      }
      
      // Compute new centroids by averaging points in each cluster
      arma::mat newM(K, p, arma::fill::zeros);
      for (int i = 0; i < n; i++) {
        newM.row(Y[i] - 1) += X.row(i);
      }
      for (int k = 0; k < K; k++) {
        newM.row(k) /= counts[k];
      }
      
      // Check convergence: centroids haven't changed (exact equality)
      // More efficient than vectorise: use all() on matrix comparison directly
      if (arma::all(arma::all(newM == curM))) {
        break;
      }
      
      // Update centroids for next iteration
      curM = newM;
    }
    
    // Return cluster assignments (1-indexed)
    return Y;
}

