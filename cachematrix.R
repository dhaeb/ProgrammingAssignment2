## Functions for the second assignment of the R-Introduction Course
## Cached inverse matrix calculation

## makeCacheMatrix creates a list of functions, which are used by cacheSolve to calclate the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function gets a list with four functions, created by "makeCacheMatrix"
## In the following, this wrapped matrix can be used to calculate (only once) the inverse.
## If you have a common R matrix m, use it in the following way: 
# 
# m <- makeCacheMatrix(m); cacheSolve(m)
# 
## Any subsequent call of cacheSolve(m) will than return the cached value of the inverse, not recomputing it.
cacheSolve <- function(x, ...) {
        
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
