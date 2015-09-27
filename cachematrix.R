## R Programming - Programming Assignment 2
##
## makeCacheMatrix - create a CacheMatrix object
## cacheSolve - calculate the inverse of a CacheMatrix. If the inverse has already been
##              calculated, use the already calculated value rather than re-computing.            
##
## Example:
##
## create an invertible matrix
## a <- matrix( c(3,1,5,2,-3,-1,-5,2,4), nrow=3, ncol=3)
##
## create a CacheMatrix object using the above matrix
## b <- makeCacheMatrix(a)  
##
## calculate the inverse
## z <- cacheSolve(b)


## makeCacheMatrix
## 
## Creates a CacheMatrix object, initialising it with the value of x
##
## The result is a list of four functions:
## set, get, setinverse, getinverse


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


## cacheSolve
##
## Takes a CacheMatrix object and tests to see if the inverse has already been calculated
## and returns its value.
##
## If the inverse has NOT been calculated yet (i.e. not cached), it calculates it and
## caches it in the CacheMatrix object.


cacheSolve <- function(x, ...) {
  
  ## Get the cached inverse. NULL is returned if the result has not yet been calculated
  m <- x$getinverse()
  
  ## If the result already exists return the cached result
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }
  
  ## Otherwise we need to calculate the inverse and cache it, then return the inverse:
  message("Result not cached, calculating...")
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
