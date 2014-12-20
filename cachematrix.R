## In these functions, we utilize the super assignment operator (<<-) to cache 
## an expensive calculation by assigning the result to an object in an 
## environment that is different from the current one. The two functions below 
## are used to create a special object that stores a matrix and caches its 
## inverse. 

## The following function creates a special "matrix", which is really a list 
## containing a function to
##     set/cache the value of the matrix
##     get the value of the matrix
##     set/cache the value of the inverse matrix
##     get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse matrix variable
    i <- NULL
    
    # set/cache the value of the original matrix in another environment
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # return value of original matrix
    get <- function() x
    
    # set/cache the inverse matrix in another environment
    setinverse <- function(solve) i <<- solve
    
    # get the inverse matrix
    getinverse <- function() i
    
    # list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the matrix in the cache via the setinverse 
## function.
cacheSolve <- function(x, ...) {
    # retrieve the inverse matrix, assuming it is already cached
    i <- x$getinverse()               
    
    # if it is cached, return the inverse matrix and exit the function
    if(!is.null(i)) {               
        return(i)                       
    }
    
    # otherwise get the matrix, calculate the inverse, cache it using the super
    # assignment operator in "setinverse", and return the result
    data <- x$get()                 
    i <- solve(data, ...)            
    x$setinverse(i)                    
    i
}
