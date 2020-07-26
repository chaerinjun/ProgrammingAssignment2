## When asked to find an inverse matrix, check for cached data first
## If it exists, return it. If not, solve and cache the new inverse matrix.

## Make a list of functions that set the data, get the data,
## set the inverse matrix, or get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## if the inverse matrix is cached, use the cached data
## if not, solve for the inverse matrix, save it and return it.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        
        ## Return a matrix that is the inverse of 'x'
}
