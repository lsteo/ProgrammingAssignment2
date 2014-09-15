## This script contains 2 functions to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse. makeCacheMatrix creates a
## list containing functions to
## a. set the value of the matrix, if the new matrix y to be assigned is the same as the current matrix m, it 
##    will not perform any assignment of the value to m.
## b. get the vaule of the matrix
## c. set the inverse of the matrix
## d. get the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL 
        set <- function(y) {                                    ## a. set the value of the matrix
                if (!((dim(m) == dim(y) && all(m == y)))) {     ## to check whether m and y is the same
                        m <<- y
                        inv <<- NULL
                } 
        }
        get <- function() m                                     ## b. get the value of the matrix
        setinverse <- function(invmatrix) inv <<- invmatrix     ## c. set the inverse of the matrix
        getinverse <- function() inv                            ## d. get the inverse of the matrix

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cacheSolve function should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()                   ## get inverse matrix
        if(!is.null(inv)) {                     ## inverse matrix exists, return the cache inverse matrix
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                         ## get the matrix
        invmatrix <- solve(data)                ## use solve to calculate the inverse matrix
        x$setinverse(invmatrix)                 ## cache the inverse matrix
        invmatrix
}
