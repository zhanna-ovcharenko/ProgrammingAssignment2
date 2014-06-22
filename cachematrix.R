## Provide functions to enable caching for matrix inversion calculations.

## Create the specific container with matrix to be cached itself
## and the required functions for cache support.

makeCacheMatrix <- function(origin = matrix()) {
  inverse <- NULL
  
  set <- function(matrix) {
    origin <<- matrix
    inverse <<- NULL
  }
  get <- function() origin
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Return cached value for inverse matrix or calculate it if cache is empty.

cacheSolve <- function(matrix, ...) {
  inverse <- matrix$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  inverse <- solve(matrix$get(), ...)
  matrix$setInverse(inverse)
  
  inverse
}
