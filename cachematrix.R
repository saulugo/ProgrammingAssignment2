## Put comments here that give an overall description of what your
## functions do

## This function builds an object that contains the set and get
## functions, and the setInv and getInv to set and get the inverse matrix
## value of a atomic matrix "x"

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) i <<- inverse
        getInv <- function() i
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function verifies first if the value of the inverse matrix of x is
## stored in cache memory. If it is not stored, then the function calculates
## the inverse of the matrix x and returns it. The second time one uses this
## function and the inverse matrix of x is already stored in cache the function
## returns the value from the cache and does not calculate it again.

cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)){
                message("getting the inverse matrix from cache")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}
