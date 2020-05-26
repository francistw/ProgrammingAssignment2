## A pair of functions that cache the inverse of a matrix

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse matrix
        setsolve <- function(solve) m <<- solve
        ## get the value of the inverse matrix
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Calculate the inverse of the special "matrix" created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## check to see if the inverse has already been calculated
        ## if so, gets the inverse from the cache and skips the computation
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## calculates the inverse of the data and sets the value in the cache
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
