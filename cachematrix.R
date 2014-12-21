##Coursera course: R Programing
##Week 3: Programming Assignment2: Lexical Scoping
##Date of submission 2014-12-21


##Function makeCacheMatrix
##Creates the list of functions for working with cache for inversed matrix:
##1. For setting the value of the matrix
##2. For getting the value of the matrix
##3. For setting the value of the inversed matrix
##4. For getting the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

##Function cacheSolve
##Checks if there's an inverse matrix already
##If no - creates the inversed matrix and sets it's value to the cache
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
