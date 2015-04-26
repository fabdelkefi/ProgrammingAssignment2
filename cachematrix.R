## Put comments here that give an overall description of what your
## functions do
## This pair of functions allow to cache the inverse of a matrix.
## For this assignment the matrix is assumed to be square invertible.

## Write a short comment describing this function
## The following function creates a special matrix object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## set inv to null
        inv <- NULL
        ## define the function set
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## the function get
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        ## the output a list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache otherwise the inverse is computed, then saved
## in the special matrix then returned as output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## if the inv is not null : the inverse is already cached        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## compute the inv and save the inverse matrix in the cacheMatrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
