
## makeCacheMatrix and cacheSolve functions 1) create a cache which stores values for inversed matrix, and 2) retrieve values for inversed matrix from cache or calculate the inversed matrix and sets them in cache. 

## This function returns a list of functions which create a cached matrix "x" and can inverse it.   

makeCacheMatrix <- function(x = matrix()) {
  
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function checks if values of inversed matrix "x" are already cached and retrieves them, if not it calculates it and sets the values of inversed matrix "x" in the cache using the setinv function

cacheSolve <- function(x, ...) {
  
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}