## Assignment2 - Caching the inverse of a matrix

## [makeCacheMatrix] given a matrix, returns an "object" that keeps a cache of
## the inverse of that matrix.
## Avaliable methods:
##  - get : return currently set matrix
##  - set : sets a new matrix, deletes old cache
##  - setinv : caches the given inverse
##  - getinv : returns the currently cached inverse (or NULL)
makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinv <- function(inv) ix <<- inv
  getinv <- function() ix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## [cacheSolve] given a CacheMatrix, returns the inverse of that matrix without
## recalculating if the matrix inverse has been previosly calculated.
cacheSolve <- function(x, ...) {
  ## Get cached inverse
  ix <- x$getinv()
  ## Return the cached inverse if avaliable
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  # No cached inverse! Get original matrix
  data <- x$get()
  # Calculate the inverse
  ix <- solve(data, ...)
  # Cache the inverse 
  x$setinv(ix)
  # Return inverse
  ix
}
