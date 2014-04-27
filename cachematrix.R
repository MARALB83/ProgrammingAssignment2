## Caching of a matrix inversion operation, which can be computationally intensive.
## If matrix is too big and the inverse is required within a loop, it makes sense to cache it so it can be retrieved instead of being recalculated.
## Student: Mario Albuquerque

## makeCacheMatrix creates a special object that has 4 functions
## It receives a matrix object and creates 4 methods that: 1) set the matrix; 2) get the matrix; 3) set the inverse of the matrix; 4) get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes the special matrix object created by makeCacheMatrix and either retrieves the cached inverse matrix or computes the inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
}
