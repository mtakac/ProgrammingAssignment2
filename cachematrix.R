## Caching of matrix inversion computation.
## Function `makeCacheMatrix` creates matrix abble to cache it's inversion.
## Function `cacheSolve` gets saved or computes and stores matrix inversion.
##
## Example:
##    1. Create the matrix:
##        matrix1 <- makeCacheMatrix(matrix(1:4, 2, 2))
##    2. Compute it's inversion:
##        cacheSolve(matrix1)
##
## First run of function `cacheSolve` will compute the matrix inversion, store it and
## return the value. Later runs will retrieve the stored value of the inversion without
## any computation.

## Create matrix `x`. Allows to use getter and setter functions of the matrix and it's
## inversion.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(inverse) {
        i <<- inverse
    }

    getInverse <- function() {
        i
    }

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculate inversion of matrix `x`. Calculated value is stored in a matrix and will be
## returned by any future calls of the function, untill the matrix is changed or another
## matrix is passed as `x`.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()

    if (!is.null(i)) {
        message("Returning cached data...")
        return(i)
    }

    i <- solve(x$get(), ...)
    x$setInverse(i)
    i
}
