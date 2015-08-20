## Programming assignment 2, R-programming Coursera
##
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The assignment is to write a pair of functions that
## cache the inverse of a matrix.

## Note. the `<<-` operator can be used to assign a value to an object 
## in an environment that is different from the current environment

## The first function, `makeCacheMatrix` creates a special "matrix" object
## that can cache its inverse. It contains the following:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <-function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getmatrix()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setmatrix(matrix)
    return(inv) 
}

## Testing the script
## > source("cachematrix.R")
## > testMatrix <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
## > testMatrix
## [,1] [,2]
## [1,]    0    2
## [2,]    1    0
## > solve(testMatrix)
## [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0
## > testCached <- makeCacheMatrix(testMatrix)
## > testInv
## [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0

