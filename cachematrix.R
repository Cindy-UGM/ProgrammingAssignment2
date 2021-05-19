## Functions to calculate the Inverse of a matrix, if the inverse is
## already calculated, then it will return the cache instead of 
## recalculating it

## Creates a matrix based on user input to be used in finding
## the inverse, the matrix and inverse (if available) is put into
## a list so that it can be recalled later


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Function to check if the inverse is already available
## If the inverse is available then it will return the cache
## If the inverse isn't available then it will calculate the inverse
## and save it to the list

cacheSolve <- function(x, ...) {
       
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
