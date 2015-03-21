## Two functions are defined here: 
## [1] makeCacheMatrix  - Creates a variable and a list of functions to cache the calculated inverse for a matrix argument.
## [2] cacheSolve       - Calculate the inverse matrix by first checking for a cached value.
## 

## The makeCacheMatrix function takes a matrix argument, 
## creates and initialise a variable to cache the calculated inverse for the matrix,
## and also a list of associated functions to store and access the parsed matrix and its inverse.
## It returns the list of functions.

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setinvM <- function(inv_m) invM <<- inv_m
        getinvM <- function() invM
        list(set = set, get = get,
             setinvM = setinvM,
             getinvM = getinvM)
}


## The cacheSolve function takes the list of functions created by makeCacheMatrix as argument.
## By calling the functions from the list, it accesses and checks for the cached inverse matrix.
## It returns the cached inverse matrix if valid due to previous calculation,
## otherwise it makes a new calculation, stores it in the cache variable and returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getinvM()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data)
        x$setinvM(invM)
        invM
}
