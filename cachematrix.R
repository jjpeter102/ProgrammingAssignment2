## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a matrix object and stores its inverse. CacheSolve computes the inverse of the matrix provided in makeCacheMatrix or returns the inverse from memory
## if the inverse has already been computed.

## Write a short comment describing this function
## makeCacheMatrix contains a set of functions that, combined with cacheSolve, produces and stores the matix inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function()x
      setSolve <- function(solve) m <<- solve
      getSolve <- function() m 
      list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Write a short comment describing this function
## cacheSolve produces the cached matrix inverse if already stored. If the inverse is not cached, cacheSolve calculates the inverse matrix from makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getSolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setSolve(m)
      m
}
