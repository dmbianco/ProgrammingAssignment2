## This function creates a special "matrix" object that can cache its inverse.
## The object is a list of functions. First, the inverse matrix variable (inv) is
## initialized to NULL. Then, the functions get, setinverse and getinverse are defined.
## Finally, the function makeCacheMatrix returns a list with the aformentioned functions.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve compute the inverse of a matrix if it has not been previously computed.
## First it checks whether the inverse matrix has been already computed. 
## Then, it returns the cached data or compute the inverse matrix and invoke
## x$setinverse to save the result. Finally, it returns the inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
