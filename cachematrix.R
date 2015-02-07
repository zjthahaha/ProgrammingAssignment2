## Put comments here that give an overall description of what your
## functions do

## These functions allow user to cache the inverse of a matrix for later use

## Write a short comment describing this function

## makeCacheMatrix take a matrix as argument, and return cached matrix which is a list of functions:
## set: set the value of the matrix
## get: get the value of the matrix
## setInv: set the inverse of the matrix
## getInv: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function

## cacheSolve take a cached matrix, and if the inverse is already set
## it will simply return the inverse; otherwise it will calculate the
## inverse, set it in the cached matrix and return it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
