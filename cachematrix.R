## a pair of functions that
## cache the inverse of a matrix.

## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # set the values of the matrix to other environment
  mI <- NULL
  set <- function(y) {
    x <<- y
    mI <<- NULL
  }
  
  # get the values of the matrix
  get <- function() x
  
  # set values of the inverse
  setInverse <- function(solve) mI <<- solve
  
  # get values of the inverse
  getInverse <- function() mI
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## `cacheSolve`: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # does the cached matrix already exist
  mI <- x$getInverse()
  if(!is.null(mI)) {
    message("getting cached data")
    return(mI)
  }
  
  # original matrix to be inversed
  data <- x$get()
  # inverse of original matrix
  mI <- solve(data, ...)
  # cache the inverse
  x$setInverse(mI)
  
  ## Return a matrix that is the inverse of 'x'
  mI
}
