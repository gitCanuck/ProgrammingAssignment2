## coursera programming Week 3 Assignment 2
##use the mean example and but use a matrix and inverse

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {i <<- inverse}
  getInverse <- function() {i}
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated (and the matrix has not 
##changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  dataMat <- x$get()
  i <- solve(dataMat, ...)
  x$setInverse(i)
  i
}
