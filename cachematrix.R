#
## These functions allow the computation of the multiplicative inverse
## of an arbitratry (invertible) matrix and store the result so it can
## be recalled multiple times without incurring the computational
## overhead associated with matrix inverting.
#

## makeCacheMatrix() encapsulates (in an R 'list' object) the data
## and methods that implement the storage of a matrix ans its inversion.
## The return list contains the named elements:
##  set: a function to set a new matrix object
##  get: a function to get a previously set matrix object
##  setinverse: a function to store the inverse of a matrix
##  getinverse: a function to retrieve a previously stored matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        store <- NULL
        set <- function(y) {
                x <<- y
                store <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) store <<- inverse
        getinverse <- function() store
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheResolv computes the inverse of the matrix stored in the
## encapsulating cache object (produced by makeCacheMatrix()) that
## is passed in the 'x' argument. It stores the computed inverse
## matrix using 'x' so it can be recalled multiple times without
## the need to recompute the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	# not cached: we need to compute the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)

	# return the result
        m
}
