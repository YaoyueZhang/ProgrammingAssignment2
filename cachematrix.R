## makeCacheMatrix and cacheSolve are a pair of function used to store a matrix 
## and cache its inverse 

## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse of the special matrix created by makeCachematrix above. 
## If the inverse has already been calculated and the matrix has not been changed, then 
## the cacheSolve should retrive the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
