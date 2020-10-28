## Cache the inverse of a matrix

## For invertible matrix x, return an object containing getters and setters for
## x and its inverse
makeCacheMatrix <- function(value = matrix()) {
  inverse <- NULL
  setValue <- function(newValue) {
    value <<- newValue
    inverse <<- NULL
  }
  getValue <- function() {
    value
  }
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }
  getInverse <- function() {
    inverse
  }
  list(setValue = setValue, getValue = getValue,
       setInverse = setInverse, getInverse = getInverse)  
}

## For cache matrix x, return the inverse of x's value.
## Uses cache implementation
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getValue()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
