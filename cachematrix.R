## Put comments here that give an overall description of what your
## functions do
## Two paired functions are used to cache the inverse of a matrix   

## Write a short comment describing this function
## makeCacheMatrix takes a matrix as input, and returns a special matrix object,
## which saves the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve takes a special matrix object created by makeCacheMatrix, 
## and computes the inverse of the matrix.
## If the inverse of the matrix is saved in the special matrix object 
## (the matrix does not change), cacheSolve will not compute again.
## It will retrieve the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
