## Inverting a matrix takes a lot of computing resources,
## and it would be advantageous to cache the inverse of a
## matrix once it has been computed once, to avoid redundant
## computations.

## This function creates an object that contains a matrix,
## and has the potential to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix
## stored in the object returned by makeCacheMatrix.
## If the inverse of an identital matrix has already
## been previously computed, the inverse will be retrieved
## from cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
