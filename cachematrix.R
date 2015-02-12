## These two functions (makeCacheMatrix() and cacheSolve()) enable you to
## store a cached version of the inverse of a matrix and manage access to the
## stored cache such that if the underlying matrix has changed a new inverse
## will be created.

## This function creates a special "matrix" object that can cache its inverse.
## It takes a matrix as an argument.

makeCacheMatrix <- function(x = matrix()) {

        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setsol <- function(x) sol <<- solve(x)
        getsol <- function() sol
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)
        
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        sol <- x$getsol()
        if(!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        matrix <- x$get()
        sol <- solve(matrix, ...)
        x$setsol(sol)
        sol
        
}
