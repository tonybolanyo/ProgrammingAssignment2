## Put comments here that give an overall description of what your
## functions do

## To solve the assignment we need two functions. The first one creates
## an object with four functions to get and set the matrix and its inverse
## this object is used in the second function that calculates the inverse
## of the matrix or returns a cached value if it was previously calculated

## Write a short comment describing this function

## As in the makeVector this function creates a "special" matrix
## as a list containing a function to
##    1. set the values of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the original matrix
##    4. get the value of the inverse of the original matrix
makeCacheMatrix <- function(x = matrix()) {

        ## initailize the inverse as NULL, not calculated
        inverse <- NULL
        
        ## with the operator <<- assign the value as our matrix
        ## int he original environment, not the current one
        ## when we assign the matrix value, the inverse value
        ## is not calculated, so we assign the value NULL to the inverse variable
        set <- function(y)
        {
            x <<- y
            inverse <<- NULL
        }
        
        ## return the matrix (not inverted)
        get <- function() x
        
        ## assign the inverted of the original matrix
        setinverse <- function(invx) inverse <<- invx
        
        ## return the value of the inverted matrix (or NULL if not calculated)
        getinverse <- function() inverse
        
        ## the list with the four functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## This function return the inverse of the matrix passed as parameter (x)
## if the value it was calculated in a previous call to this function
## the value is saved in a cache variable (using makeCacheMatrix) to
## avoid recalculating. If the value is not cached, the function calculates
## the inverted matrix and saves the result for future calls
cacheSolve <- function(x, ...) {
    
    ## try to get a previous saved inverted matrix
    inverse <- x$getinverse()
    
    ## if is cached (it's not NULL) so we can return without recalculate
    if(!is.null(inverse)) {
        message("Getting cached inverted matrix...")
        return(inverse)
    }
    
    data <- x$get()
    
    inverse <- solve(data, ...)
    
    x$setinverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    ## after make the calculation and save the value in cache
    inverse
}
