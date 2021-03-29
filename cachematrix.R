## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create a special matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## xminus inverse matrix
  ## original matrix
  
  xminus <- NULL
  set <- function(y) {
    x <<- y
    xminus <<- NULL
  }
  get <- function() x
  setInverse <- function(y) xminus <<- y
  getInverse <- function() xminus
  list (set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## xminus inverse matrix of x
    xminus <- x$getInverse()
    if (!is.null(xminus)) {
      message ("retriving cached data")
      return(xminus)
    }
    data <- x$get()
    xminus <- solve(data,...)
    x$setInverse(xminus)
    xminus
  }

