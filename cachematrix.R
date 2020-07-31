## These functions will calculate the inverse of a matrix, after
## checking if the calculation is in cache first.


## The function makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse with the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(invert) m <<- invert
        getinvert <- function() m
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## The function cacheSolve calculates the inverse of a matrix, after
## checking if the calculation is in cache first.

cacheSolve <- function(x, ...) {
        m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvert(m)
        m
}
