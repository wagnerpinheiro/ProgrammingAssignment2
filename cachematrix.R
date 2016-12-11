## Functions to second programming assignment on R Programming course
## This functions calculate the inversed matrix and cache the results

## Creates an matrix with cached attributes in the main environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Solve the inverse of the matrix x

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("inversed matrix already calculated, getting from cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}


## Function to test if cached matrix is working and get a runtime profiler
## expected result:
# inversed matrix already calculated, getting from cache
# Test PASSED
# user.self sys.self elapsed user.child sys.child
# calc_nocached     0.004        0   0.006          0         0
# calc_cached           0        0       0          0         0

runTest <- function(order = 128){
  num = order ^ 2
  x <- matrix(rnorm(num), nrow = order, ncol = order) 
  m = makeCacheMatrix(x)
  m$get()
  
  ptm <- proc.time()
  cacheSolve(m)
  ptm1 <- proc.time() - ptm
  
  ptm <- proc.time()
  cacheSolve(m)
  ptm2 <- proc.time() - ptm
  result <- data.frame(rbind(calc_nocached = as.list(ptm1), calc_cached = as.list(ptm2)))
  if(ptm2[3] < ptm1[3]){
    message("Test PASSED")
  }else{
    message("Test FAILED")
  }
  
  result
}

## sample of run test
# runTest()
## expected result:
# inversed matrix already calculated, getting from cache
# Test PASSED
# user.self sys.self elapsed user.child sys.child
# calc_nocached     0.004        0   0.006          0         0
# calc_cached           0        0       0          0         0

# try runTest(1000) for better estimate the runtime, or, runTest(10000) to get a 10k to 10K matrix and see your CPU burn ;-)