
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# cached inverse
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function will return inverse of the special matrix x, or cached value if it was used before
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    # if cached value was found, use it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # otherwise try to solve it and cache result
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}