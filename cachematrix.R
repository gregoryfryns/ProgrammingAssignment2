## This file defines two functions to store matrices and their inverses to 
## a cache and retrieve them 

## Example usage :
#  > mat <- matrix(sample.int(16,16),4,4) # generates a random 4x4 matrix
#  > matCache <- makeCacheMatrix(mat)     # makes the cached version of the matrix
#  > cacheSolve(matCache)                 # returns the inverse of the matrix
#                                         # (computes it and stores it in the cache)
#  > cacheSolve(matCache)                 # returns the inverse of the matrix
#                                         # (from the cache this time)


## makeCacheMatrix creates a cached version of the matrix given as an input
## This cached version can store the matrix's inverse. It generates a vector 
## of functions that can
##   1. set the matrix value in the cache
##   2. get the matrix from the cache
##   3. set the inverse of the matrix in the cache
##   4. get the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of the cached matrix given as an argument. 
##   If the inverse is in the cache, this value is returned. 
##   If not the inverse is computed and stored in the cache
## This function assumes all the provided matrices are invertible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
