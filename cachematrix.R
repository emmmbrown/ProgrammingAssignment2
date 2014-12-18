# This creates a pair of functions which cache the inverse
# of a matrix in order to conserve energy spent in its computation.
# It is assumed that the matrix is invertible


makeCacheMatrix <- function(x = matrix()) {
  # Creates a 'matrix' object that can cache its inverse
    inv <- NULL
    # Define object methods
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    # Create list of methods/'matrix' object
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
  # Returns a matrix that is the inverse of 'x'
  # 'x' is an object returned by makeCacheMatrix
    inv <- x$getinv()
    if(!is.null(inv)) {
      message('Getting cached data')
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
