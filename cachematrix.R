## Put comments here that give an overall description of what your
## functions do
## Function to demonstrate lexical scoping and closure of functions

## Write a short comment describing this function
## makeCacheMatrix takes a matrix as input and returns a list of functions to set value to the matrix,
## get value from the matrix, set its inverse value and get its inverse value

makeCacheMatrix <- function(x = matrix()) {
  inverseCached <- NULL
  
  set <- function(y) {
    x <<- y
    inverseCached <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverseVal) {
    inverseCached <<- inverseVal
    inverseCached
  }
  
  getInverse <- function() {
    inverseCached
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve function takes an input created with the makeCacheMatrix function and returns the inverse of the value
## Based on the messages printed we can get to know if it the value was returned from the cache or of it was calculated.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedInverse = x$getInverse()    
  
  if (!is.null(cachedInverse) && is.matrix(cachedInverse)) {
    print("Getting cached value")
    return(cachedInverse)
  }
  
  inputMatrix = x$get()
  inverseOfMatrix <- solve(inputMatrix)
  
  x$setInverse(inverseOfMatrix)
  print("Returning calculated value")
  return(inverseOfMatrix)
}
