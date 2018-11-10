## cachematrix.R cache the inverse of a matrix rather than compute it
## repeteadly

## makeCacheMatrix() creates a special "matrix" object that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
      mInverse <- NULL
      set <- function(y){
            x <<- y
            mInverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) mInverse <<- inverse
      getInverse <- function() mInverse
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
      mInverse <- x$getInverse()
      if(!is.null(mInverse)){
            message("Getting cached data")
            return(mInverse)
      }
      data <- x$get()
      mInverse <- solve(data, ...)
      x$setInverse(mInverse)
      ## Return a matrix that is the inverse of 'x'
      mInverse
}