## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a Vector to hold the Inverse Cache Matrix, and functions
## to get or set the Inverse Cache Matrix, or to get the original data matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## This function accepts a vector that was created by makeCacheMatrix, then
## to check if there exist already a Cached Inverse Matrix.  If it does not exist, 
## then it would create the Inverse Matrix using the solve function, and then to store
## the Inverse matrix to the vector that was provided.  If it exist, then the Inverse
## Cache Matrix is return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
