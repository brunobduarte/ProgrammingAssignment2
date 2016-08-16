## Theses two functions cache the inverse of a matrix.
## You must use the makeCacheMatrix first and then, call its result to cacheSolve.

## The makeCacheMatrix creates a "special" matrix object that can store as cache its inverse.
## It creates a list with 4 different functions that set the matrix and its inverse,
## and make way for the other function to get the results back.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function looks for an existing inverse calculation to return.
## If there's none, it calculates the inverse and store it in the "makeCacheMatrix" object.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

