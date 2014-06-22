## 
## cachematrix.R
## creates a representation of a matrix that allows for easy caching of the
## inverse.  When the inverse is needed, cacheSolve() can be used on the
## representation.  If the inverse has been computed previously, it is
## returned and not calculated, otherwise it is computed.
##

## Creates a special representation of a matrix that allows for easy
## caching of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Returns the cache of a matrix representation.
## If it has been previously computed, use that.  Otherwise compute it now

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)  ## Actual solving done here
    x$setinverse(i)
    i
}
