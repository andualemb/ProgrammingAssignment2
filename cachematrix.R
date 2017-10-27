## Caching Inverse of a Matrix
## Matrix inversion requires long computation and there is a benefit to cache a function
## instead of calculating it everytime. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
}
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,setInv = setInv, getInv = getInv)
}


## cacheSolve is a function that computes the inverse of the "matrix" 
## created by makeCacheMatrix above .

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("Retrieving cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}
