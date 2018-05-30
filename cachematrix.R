## lalala create a special matrix that can cache its inverse
##  and then compute the inverse of this special matrix

## create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cachesolve computes the inverse of the matrix returned by the makeCacheMatrix. If the inverse has alreeady been calculated (and the matrix has not changed) it is retrieved form the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
        ##  otherwise solve computes the inverse of the matrix.
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

