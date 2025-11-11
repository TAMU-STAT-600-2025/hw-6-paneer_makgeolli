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
    
    // Initialize some parameters
    int n = X.n_rows;
    int p = X.n_cols;
    arma::uvec Y(n); // to store cluster assignments
    
    // Initialize any additional parameters if needed
    
    
    // Implement K-means algorithm. 
    // It should stop when either 
    // (i) the centroids don't change from one iteration to the next (exactly the same), or
    // (ii) the maximal number of iterations was reached, or
    // (iii) one of the clusters has disappeared after one of the iterations (in which case the error message is returned)
    
    // Precompute X squared since doesnt need to be repeated
    
    // For loop with kmeans algorithm
    for (int iter = 0; iter < numIter; iter++) {
      // Compute distances from each row in X and M, store it in a matrix nxK matrix D2
      
      // Assign value to Y by finding at which column the minimum is at in D
      // then check number of unique values in Y is K
      
      // Compute new centroid values mu, check that it changed, and store into M

    }
    
    // Returns the vector of cluster assignments
    return(Y);
}

