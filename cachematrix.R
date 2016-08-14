## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse
## The first function, 'makeCacheMatrix' creates a list containing functions to
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the inverse of the matrix
## 2. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix() above.
## The following function calculates inverse of the matrix created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it 'gets' the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the matrix in the cache via the 'setmatrix' function.

cacheSolve <- function(x, ...) {
		m <- x$getmatrix()
        if(!is.null(m)) {
                    message("getting cached data")
                    return(m)		## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m		## Return a matrix that is the inverse of 'x'
}
