## This function will take an input matrix and give its inverse as output.
## The inverse matrix will come from cache if it has been inversed once
## otherwise it will inverse the input matrix.

## This function will take an input matrix and create a similar dimensioned
## output matrix. It will also create a list with 4 functions to be used by
## the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        newrow <- nrow(x)
        newcol <- ncol(x)
        m <- matrix(NA,nrow=newrow,ncol=newcol)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function will determine if the inverse of the input matrix already
## exists in cache. If it does then the cache inverse matrix is returned
## otherwise an inverse of the input matrix is created and cached in memory.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!all(is.na(m))) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}
