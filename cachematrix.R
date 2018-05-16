## Comment for R Programming Assignment 2 : Caching the inverse of a Matrix

## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inver <- NULL
      set <- function(y) {
            x <<- y
            inver <- NULL
      }
      get <- function () x
      setInverse <- function(inverse) inver <<- inverse
      getInverse <- function() inver
      list (set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)

}


## Function computes the inverse of the special "matrix" returned by makeChacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        if (!is.null(inver)) {
            message ("Getting cached data")
            return(inver)
        }
        matri <- x$get()
        inver <- solve(matri, ...)
        x$setInverse(inver)
        inver
}
