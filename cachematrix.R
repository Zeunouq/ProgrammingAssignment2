## The function cachematrix.R is a set of two functions 1- makeCacheMatrix and 2- cacheSolve
## that allow to calculate the inverse of a matrix. If the contents of the matrix are not changing, 
## the function will cache the value of the inverse so that when we need it again, it can 
## be looked up in the cache rather than recomputed.

## The function makeCacheMatriw creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(solve) m <<- solve
        getinvmatrix <- function() m
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}

## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),  
## then the cachesolve should retrieve the inverse from the cache. 
## Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinvmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmatrix(m)
        m
}
