## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    ## Creates special "matrix" object which allows to cache inverse value
    
    inv <- NULL                                   ## will hold inverse matrix
    set <- function(y) {                          
        x <<- y                                   
        inv <<- NULL
    }
    get <- function() x                           
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  ##needed to be able to refer to functions with $ operator
}


## Write a short comment describing this function
## Computes inverse of the special "matrix" object returned by makeCacheMatrix
## If the inverse has been already calculated and the matrix did not change, 
## returns cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    inv <- solve(data, ...) ## default function in R to compute inverse matrix
    x$setInverse(inv)
    inv
    
    
}
