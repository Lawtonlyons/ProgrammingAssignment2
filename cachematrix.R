## makeCacheMatrix is a function that creates a matrix; cacheSolve calculates the inverse of the matrix.

## makeCacheMatrix returns a list of functions that set the value of a matrix, get the value
## of a matrix, set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = numeric()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function cacheSolve returns the inverse of a the matrix created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it.  If it is not, the IF comman in cacheSolve computes
## the inverse, caches it and returns it.

cacheSolve <- function(x, ...) {
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}
