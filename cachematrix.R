## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                ## set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                             ## get the value of the matrix
        setinverse <- function(solve) m <<- solve       ## set the value of the inverse
        getinverse <- function() m                      ## get the value of the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates or retrives the cache of the inverse 
## of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
