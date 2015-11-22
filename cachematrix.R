## Caching the inverse of a matrix
## Matrix inversion can be computationally costly.  The following functions work together
## to cache the inverse of a matrix, rather than compute it repeatedly.

## The makeCacheMatrix function creates special "matrix" object that can cache its inverse.
## It sets and gets the value of the matrix
## It sets and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      invm <- NULL
      set <- function(y) {
              x <<- y
              invm <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) invm <<- inverse
      getInverse <- function() invm 
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}



## The cachesolve function computes the inverse of the special "matrix" returned in makeCacheMatrix
## above.  If the inverse has already been calculated, then it returns the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invm <- x$getInverse()
      if (!is.null(invm)) {
            message("getting cashed data")
            return(invm)
      }
      matx <- x$get()
      invm <- solve(matx, ...)
      x$setInverse(invm)
      invm
}
