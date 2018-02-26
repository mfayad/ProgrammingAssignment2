##This programs defines two functions that allows the user to cache the matrix inverse,
##that avoids re-running the same computation and saves computational resources.

##This function creates a special matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set=set, 
       get=get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function calculates the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
  }
  data <- x$get()
  inv <-solve(data)
  x$setInverse(inv)
  inv
}
