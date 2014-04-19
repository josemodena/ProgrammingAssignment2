## --------------------------------------------------------------------------
## Coursera - R Programming - Programming Assignment 2 - Peer Assessment
## 
## This R script implements two functions, makeCacheMatrix and cacheSolve,
## which reduce the time needed to determine the inverse of a matrix in 
## repeated calculations, by storing the result in a cache.
## --------------------------------------------------------------------------
##
## --------------------------------------------------------------------------
## makeCacheMatrix
## 
## Description
## Creates a special "matrix" object that can cache its inverse.
##
## Usage
## makeCacheMatrix(x = matrix())
##
## Arguments
## x    a matrix.
##
## Value
## Special "matrix" object, i.e., a list containing functions to:
##      set the value of the "matrix",
##      get the value of the "matrix",
##      set the inverse of the "matrix",
##      get the inverse of the "matrix".
##
## Examples
## myMatrix <- makeCacheMatrix()        creates empty myMatrix
## myMatrix <- makeCacheMatrix(x)       creates myMatrix initialized with x
## myMatrix$set(x)                      sets myMatrix data to x
## myMatrix$get()                       gets myMatrix data
## myMatrix$setInverse(i)               sets myMatrix inverse to i
## myMatrix$getInverse()                gets myMatrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL  ## NULL marks the matrix has changed
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## --------------------------------------------------------------------------
## cacheSolve
## 
## Description
## Computes the inverse of the special "matrix" x created by makeCacheMatrix.
##
## Usage
## cacheSolve(x, ...)
##
## Arguments
## x    a special "matrix" created by makeCacheMatrix.
##      x must be a square invertible matrix.
## ...  further arguments passed to or from other methods.
##
## Value
## If the inverse of x has already been calculated and the matrix has not
## changed, cacheSolve returns the inverse of x from the cache. Otherwise,
## cacheSolve calculates the inverse, stores the result in the cache, and
## returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        ## inv is NULL when the inverse has not been calculated yet or
        ## the matrix has changed.
        if(!is.null(inv)) {
                message("Getting cached inverse")
        } else {
                data <- x$get()
                inv <- solve(data, ...)
                x$setInverse(inv)                
        }
        inv
}
