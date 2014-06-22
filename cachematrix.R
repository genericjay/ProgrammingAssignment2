# makeCacheMatrix and cacheSolve provide a means to create an object that wraps a matrix
#   and allows its inverse to be cached in order to only have to perform solve(X) once
#   in order to avoid high function overheads. In order to retrieve a cached inverse
#   cacheSolve(x, ...) should be used.
#   In order to retrieve the original matrix x$getmatrix() can be used where x
#   is the result of makeCacheMatrix

# makeCacheMatrix constructs a wrapper for a matrix that has the following functions accessible 
#   with the $ operator: setmatrix, getmatrix, setinverse, and getinverse. Only getmatrix 
#   should be used.
makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  setmatrix <- function(y) {
    matrix <<- y
    cached_inverse <<- NULL
  }
  getmatrix <- function() matrix
  setinverse <- function(inverse) cached_inverse <<- inverse
  getinverse <- function() cached_inverse
  
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve attempts to retrieve and optionally compute the inverse 
#   of a cacheMatrix (the result of makeCacheMatrix). 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cached_inverse <- x$getinverse()
  if (!is.null(cached_inverse)) {
    message("Getting cached data")
    return(cached_inverse)
  }
  
  matrix <- x$getmatrix()
  cached_inverse <- solve(x, ...)
  x$setinverse(cached_inverse)
  
  cached_inverse
}
