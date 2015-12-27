## A set of functions that let us store a matrix and cache results of
## matrix inversion.
##
## Example usage:
## # Initialize a cacheable matrix.
## m <- makeCacheMatrix(matrix(c(1,2,3,1,2,4,2,3,1), 3, 3))
## # Get the matrix.
## m$get()
## # Get the inverse.
## cacheSolve(m)
## # Get the inverse for the second time -- returns the cached value.
## cacheSolve(m)
## # Change the stored value.
## m$set(matrix(c(1,1,4,3), 2, 2))
## # Get the inverse. The value has changed so the inverse is recomputed.
## cacheSolve(m)


## Create the object to hold a matrix and to cache the inverse. Initialize
## the stored value to the given argument, if provided.
## The public interface of the object are the function $get and $set
## that provide read and write access to the stored matrix. The remaining
## functions should not be used directly.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the inverse of the given cacheable matrix.
## If this is the first time we are asking for the inverse since the matrix
## was last modified, the result is cached. Otherwise the cached result is
## returned.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
