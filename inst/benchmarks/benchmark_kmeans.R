if (!requireNamespace("microbenchmark", quietly = TRUE)) {
  message("Package 'microbenchmark' not installed, skipping benchmark")
} else {
  library(microbenchmark)
  library(GroupHW)
  
  set.seed(123)
  
  # Test 1: Small example
  X <- matrix(rnorm(100), ncol = 2)
  K <- 3
  init_M <- X[sample(1:nrow(X), K), , drop = FALSE]
  
  Y_R  <- MyKmeans_R(X, K, init_M, numIter = 100)
  Y_Cpp <- MyKmeans(X, K, init_M, numIter = 100)
  print(all(Y_R == Y_Cpp))
  
  mb <- microbenchmark(
    R_version   = MyKmeans_R(X, K, init_M),
    Cpp_version = MyKmeans(X, K, init_M),
    times = 10
  )
  print(mb)
  cat("Speedup (R / C++):", median(mb$time[mb$expr == "R_version"]) / median(mb$time[mb$expr == "Cpp_version"]), "\n\n")
  
  
  # Test 2: ZIPCODE example (optional, skip if file missing)
  zipcode_file <- system.file("extdata/ZIPCODE.txt", package = "GroupHW")
  if (file.exists(zipcode_file)) {
    zipcode <- read.table(zipcode_file, header = FALSE)
    Y <- zipcode[,1]
    X <- zipcode[,-1]
    
    mb <- microbenchmark(
      R_version   = MyKmeans_R(X, K=10),
      Cpp_version = MyKmeans(X, K=10),
      times = 10
    )
    print(mb)
    cat("Speedup (R / C++):", median(mb$time[mb$expr == "R_version"]) / median(mb$time[mb$expr == "Cpp_version"]), "\n\n")
  } else {
    message("ZIPCODE.txt not found, skipping ZIPCODE benchmark")
  }
  
  # Test 3: Large synthetic dataset
  n <- 10000
  p <- 10
  K <- 5
  X <- matrix(rnorm(n*p), ncol=p)
  init_M <- X[sample(1:n, K), ]
  
  mb <- microbenchmark(
    R_version   = MyKmeans_R(X, K, init_M),
    Cpp_version = MyKmeans(X, K, init_M),
    times = 5
  )
  print(mb)
  cat("Speedup (R / C++):", median(mb$time[mb$expr == "R_version"]) / median(mb$time[mb$expr == "Cpp_version"]), "\n\n")
}
