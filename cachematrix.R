## This file contains the functions that demonstrate the concepts of Lexical
## Scoping and Caching to speed up computations.

## The following function creates a special "vector" which is basically a list
## of getter and setter functions to set and get the value of the matrix as 
## well as the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## The cacheSolve function checks if the matrix's inverse has been cached already
## If yes, then it speeds up the runtime by returning the cached value, else it 
## computes the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
}
