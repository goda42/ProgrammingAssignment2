## This file is used to create a matrix object that can store the
## inverse of the matrix in memory, as this is a processor
## intensive process

## Programming Assignment 2: R Programming on Coursera
## Chris M.

## makeCacheMatrix creates an object that will store the matrix,
## any computed inverses, and the functions required to set, retrieve,
## and use the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function will attempt to retrieve the inverse of
## the matrix from the cache and if it is null, it will calculate
## the inverse and return that

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
