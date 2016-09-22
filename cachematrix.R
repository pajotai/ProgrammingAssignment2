## The results can be tested by creating an invertible matrix first, let's say invertableMatrix,
## then creating the "enriched" matrix with caching abilities with cacheMatrix <- makeCacheMatrix(invertableMatrix),
## then calculating cacheSolve(cacheMatrix). The first cacheSolve() call also caches the result for subsequent calls 


## Function that creates an "enriched" matrix from a regular one, able to cache its inverse.
## It contains four functions: get/set matrix, get/set inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function that calculates the inverse of a matrix.
## It first checks if the result is already available in the cache, avoiding re-calculation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
