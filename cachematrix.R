## These functions are the building blocks for a matrix-like object that
## caches its own inverse, so that the inverse is not calculated more
## than once.

## 'makeCacheMatrix' creates a matrix object that is capable of caching
## its own inverse.
## The 'CacheMatrix' is a list consisting of functions which set the
## matrix, get the matrix, cache the inverse, and retrieve the cached
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, 
        setinv = setinv, getinv = getinv)
}


## 'cacheSolve' calculates the inverse of a matrix 'x', or retrieves
## the cahced inverse if it has already been calculated.
## 'x' is a matrix object of the type created in CacheMatrix,
## so it contains a cached inverse and a function for retrieving this
## inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
