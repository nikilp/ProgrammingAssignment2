## These two functions are allowing once computed (with "solve")
## the inverse of a matrix to be cached rather than computing it repeatedly.



## Creates a special "matrix" object that can cache its inverse
cacheSolve <- function(x, ...) {
        mInv <- x$getMatrixInverse()
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$getMatrix()
        mInv <- solve(data, ...)
        x$setMatrixInverse(mInv)
        mInv
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        setMatrix <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        getMatrix <- function() x
        setMatrixInverse <- function(solve) mInv <<- solve
        getMatrixInverse <- function() mInv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}