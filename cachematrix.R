# This function creates a special matrix which is a list containing settters and 
# getters functions.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function calculates inverse of the matrix created by makeCacheMatrix(). 
# It first checks if the inverse has already been calculated. If so, it gets the 
# inverse from the cache and skips the computation. Otherwise, it calculates the
# inverse of the matrix and sets the value of the inverse in the cache via the 
# setinverse().
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}