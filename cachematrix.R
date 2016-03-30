##****************************************************************************
## Coursera: R-Programming
## Assignment: Caching the Inverse of a Matrix
## Shankar Gorkhe
## 03/30/2016
##****************************************************************************

##############################################################################
## This function creates a special "matrix" object that can cache its inverse.
##############################################################################

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setsolv <- function(solv) invrs <<- solv
  getsolv <- function() invrs
  list(set = set, get = get,
       setsolv = setsolv,
       getsolv = getsolv)
}
##############################################################################
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.
###############################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getsolv()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solv(data, ...)
  x$setsolv(invrs)
  invrs
}
################################################################################
