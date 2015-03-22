## Put comments here that give an overall description of what your
## functions do

## Functions allow for storing the inverse of a matrix in memory.
## If the inverse of the matrix is needed again, the previously
## calculated inverse matrix is returned rather than having R calculate
## the inverted matrix again.

## Write a short comment describing this function

## makeCacheMatrix takes in the original matrix x. It creates an
## environment where the original matrix and its inverse matrix are stored.

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


## Write a short comment describing this function

## cacheSolve checks if the inverse of the matrix x has been previously
## calculated. If it has, it will return that value rather than recalculate
## the inverse of x again. If the inverse has not previously been
## calculated, the function will calculate the inverse matrix using the
## solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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
