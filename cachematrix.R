# Special matrix-like type with inverse that stored in cache
# Useful for big matrices if solve() called multiple times

# Constructor
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    inverse <<- NULL
    x <<- y
  }
  get <- function() x
  setinverse <- function(inv) {
    inverse <<- inv
  }
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Returns inverse matrix (either from cache or calculates and stores in cache)
# Always assumes inverse exists
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message('return chached data')
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
