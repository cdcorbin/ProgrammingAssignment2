
## Creates a matrix 'object' that contains the cached value of the computed inverse

makeCacheMatrix <- function(mtx = matrix()) {
  inv <- NULL
  ## function for setting the object data, clears cache
  set <- function(x) {
    mtx <<- x
    inv <<- NULL
  }
  ## function for getting the object data
  get <- function() mtx
  ## function for setting the computed inverse
  setinv <- function(x) inv <<- x
  ## function for getting the inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Solves the inverse of the matrix, returning the cached value if already computed

cacheSolve <- function(mtx, ...) {
  ## get from the cache
  inv <- mtx$getinv()
  ## if it exists, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## compute the inverse if the cached version does not exist
  data <- mtx$get()
  inv <- solve(data, ...)
  ## set the cache
  mtx$setinv(inv)
  ## Return a matrix 'inv' that is the inverse of 'mtx'
  inv
}
