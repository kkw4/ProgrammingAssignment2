## This Function will accept a matrix as an argument only
## It provides 4 methods within: set, get, setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()) {
#Cache Matrix
  m <- NULL
#Definition of various set / get functions
  set <- function(y) {
    x <<- y
    m <<- NULL
  }  
  get <- function() x
#Function to calculate and store the inverse of a matrix
  setInverse <- function(solve) m <<- solve
#Function to return the stored cache matrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##Function to retrieve a matrix from cache if it exists
##This function only accepts a makeCacheMatrix object as input
##It will return a matrix that is the inverse of 'x', either
##from cache or newly calculated by accessing the stored value
##in the makeCacheMatrix object
cacheSolve <- function(x, ...) {
  #Try to obtain the cached Inverse matrix if it exists and return it
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #If it doesn't exist, then calculate the Inverse matrix now and return it
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}