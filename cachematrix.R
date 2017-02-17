## Put comments here that give an overall description of what your
## functions do
## function makeCacheMatrix is a function that creates a matrix object that can cache its inverse.
## function cacheSolve computes the inverse of the matrix created by makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed, it returns the cached inverse.

## Write a short comment describing this function
## takes a matrix as input and creates a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## this function takes the result of makeCacheMatrix as input and returns the inverse of the matrix
## it tries to get the cached inverse of the matrix. if this is null, it uses the inputs functions
## to get the actual matrix, calculate its inverse and store it in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
