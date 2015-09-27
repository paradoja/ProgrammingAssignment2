## The following functions allows creating special matrices that cache
## the result of the calculation of the inverse, so that the cost of
## computing it is only paid once.

## Creates a special matrix that can "remember" the value of its
## inverse. Recieves as parameter a normal matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set.inverse <- function(inverse) inverse <<- inverse
    get.inverse <- function() inverse
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Returns the inverse of a matrix created with makeCacheMatrix, which
## it receives as parameter. It can also receive as arguments extra
## parameters to pass to the solve function to calculate the inverse.
cacheSolve <- function(x, ...) {
    inverse <- x$get.inverse()
    if (!is.null(inverse)) {
        message("getting cached inverse of matrix")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$set.inverse(inverse)
    inverse
}
