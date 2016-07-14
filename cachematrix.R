#' This pair of functions create a matrix object that is able to cache its
#' inverse, once the inverse has been calculated.  This is accomplished
#' by defining four functions within the matrix object (an R list):
#' - "set" to set the matrix value and clear the inverse value 
#' - "get" to get the matrix value so that the inverse may be calculated
#' - "setinverse" to store the inverse value after calculating it
#' - "getinverse" to get the cached inverse if it has already been calculated
#'
#' The matrix and its inverse are stored within the environment of the
#' functions defined in the object, courtesy of R's lexical scoping.  To use
#' these functions, first create the matrix object using makeCacheMatrix.  Next
#' calculate the inverse by passing the object to the cacheSolve function.
#' This function will check the environment of the object to see if the inverse
#' has been previously calculated, will calculate it if necessary, and then
#' store the value in the cache. When using the interface provided (i.e. by
#' using the makeCacheMatrix function, or the set function defined within the
#' object) it ensures that the cache is emptied so as to have the correct
#' inverse recalculated. This code assumes the matrix is invertable.  If the
#' matrix is singular, it will allow the matrix object to be created, but 
#' calculation of the inverse will fail with a Lapack error.

## This function creates the matrix object, where the matrix and its inverse
## are stored, and returns the object created.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks to see if the inverse is available, and fetches it if
## possible.  If not available, it caculates the inverse, stores it for later
## use, and returns the inverse.
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
