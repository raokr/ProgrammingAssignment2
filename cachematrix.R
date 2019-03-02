## The functions below will check if the inverse of a matrix is already computed
## and will return the inverse from cache if it is already computed.
## If the inverse is not in the cache, then it will compute the inverse of the 
## matrix and return the inverse of the matrix

## makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## initializing the inverse to NULL
  inv <- NULL
## assigning the matrix to setMatrix
setMatrix <- function (y) {
  x<<-y
  inv <<- NULL
  }
## returns the Matrix
getMatrix <- function() {x}
## assigning inverse of matrix to setInverse
setInverse <- function(inverse) {inv <<- inverse}
## returning the inverse of the matrix using getInverse
getInverse <- function(){inv}
list(setMatrix = setMatrix,
     getMatrix = getMatrix,
     setInverse = setInverse,
     getInverse = getInverse)

}


## Returns the inverse of the matrix created above by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
   if (!is.null(inv)) {
    message("this is the cached data")
    return(inv)
  }
  passmat <- x$getMatrix()
  inv <- solve(passmat, ...)
  x$setInverse(inv)
  inv
}

