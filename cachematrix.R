## Put comments here that give an overall description of what your
## functions do:
##
## The first function creates a matrix and calculate its inverse
## which is then cached. The next function returns the inverse of
## a matrix, which is recalled if the inverse is found in cache.
## Otherwise, the function calculates and returns the inverse
## of the matrix.


## Write a short comment describing this function:
## 
## Below uses "set" and "get" to store the matrix
## and use "setinverse" and "getinverse" to calculate
## and store the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setInverse <- function() inverse <<- solve(x)
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function:
## 
## The following returns the inverse of the matrix x
## from cache if inverse of matrix is already stored.
## Otherwise, calculates and returns inverse of matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
