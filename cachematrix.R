## Pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## initialise the matrix to NULL
        m <- NULL
        
        ## create another function, which will cache the value
        set <- function(y) {
                
                x <<- y
                m <<- NULL ## re-initialise the matrix to NULL, if the matrix is changed
                
        }
        
        ## create function to get the value of x
        get <- function() x
        
        ## create function to compute the inverse of the square matrix (using the solve function)
        setinverse <- function(solve) m <<- solve
        
        ## create function to get the inverse
        getinverse <- function() m
        
        ## passes a list of the functions in the makeCacheMatrix function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
        
        m <- x$getinverse()
        
        ## check to see if the inverse exists
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if the inverse is not available, create it and then return it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
