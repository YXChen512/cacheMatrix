## These functions save the matrix inversion in cache.
## Next time when the inversion is needed, the cached value is returned
## Such operation saves the time of calculating inversion repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) Inverse <<- Inv
  getInv <- function() Inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  Inverse <- x$getInv()
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  MTrix <- x$get()
  Inverse <- solve(MTrix,...)
  x$setInv(Inverse)
  Inverse
}
