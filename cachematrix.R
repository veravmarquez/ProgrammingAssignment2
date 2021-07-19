##Caching the Inverse of a Matrix
##Matrix inversion is typically a time-consuming operation, therefore storing the inverse of a matrix rather than calculating it repeatedly may be advantageous.

##makeCacheMatrix function generates a special "matrix" object capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##cacheSolve function solves the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...){
      inv <- x$getInverse()
      if (!is.null(inv)) {
             message("getting cached data")
             return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
