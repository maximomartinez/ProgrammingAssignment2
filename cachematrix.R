# Matrix inversion is usually a costly computation and their may be some 
#benefit to caching the inverse of a matrix rather than compute it repeatedly
# These are a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix
# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
# Returns a list that contains the following methods:
# set: sets matrix and resets cached inverse
# get: returns matrix
# setInv: sets the inverse value
# getInv: returns cached inverse value

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <- NULL
    }
    get <- function () x
    setInv <- function(inv) inverse <<- inv
    getInv <- function() inverse
    list(set=set, get=get, 
         setInv=setInv, 
         getInv=getInv)
}

# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInv()
    if(!is.null(inverse)) {
        message("Retrieving cached data...")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInv(inverse)
    inverse ## Return a matrix that is the inverse of 'x'
}
