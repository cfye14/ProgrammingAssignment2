## The makeCacheMatrix and cacheSolve functions allow the user to store a matrix for later use, set or re-assign the value of the 
## original matrix or its inverse, and perform the inverse operation on the cached matrix.

## The makeCacheMatrix function caches a matrix. The enclosed functions allow the user to re-set the value of the parameter matrix,
## retrieve the matrix, assign a value for a (the) inverse matrix, and retrieve the inverse matrix. 
## The output is a list of the enclosed functions.

makeCacheMatrix <- function(x) {
    inv <- NULL
    set <- function(y=matrix()){
        x <<- y 
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(z=matrix()) inv <<- z
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function inverts the matrix cached by the makeCacheMatrix function, if there is no existing value for the inverse matrix.
## The output is an inverted matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    temp <- x$get()
    inv <- solve(temp)
    x$setInv(inv)
    inv
}
