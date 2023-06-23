## The makeCacheMatrix and cacheSolve functions work together to provide a
## caching mechanism for matrix operations.

## The makeCacheMatrix function creates a cache-enabled matrix object.
## The function returns a list of functions that allow setting and retrieving
## the matrix, as well as setting and retrieving a cache for the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # initialise cache

    set <- \(y) {  # set the matrix
        x <<- y    # update x, and therefore
        m <<- NULL # invalidate the cache
    }
    get <- \() x   # get the matrix

    setinverse <- \(inv) m <<- inv # set the inverse in cache
    getinverse <- \() m            # get the inverse from cache

    list(set = set,              # return a list of functions
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function calculates and returns the inverse of a matrix while utilizing
## the caching mechanism provided by the makeCacheMatrix function. The function takes a matrix object x
## as input, along with additional optional arguments that can be passed to the solve function
cacheSolve <- function(x, ...) {
    m <- x$getinverse()          # retrieve the cache,
    if (is.null(m)) {            # if the cache is invalid
        m <- solve(x$get(), ...) # calculate the inverse, pass along additional arguments
        x$setinverse(m)          # save the calculation in the cache
    }
    m                            # return the cached or calculated matrix
}
