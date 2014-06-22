## Put comments here that give an overall description of what your
## functions do

# Caching of Inverse of Matrix

## Write a short comment describing this function
## Create a matrix object that can cache inverse
makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    
    ## Set Matrix
    set <- function(m) {
        
        ## Update matrix
        x <<- m
        
        ## Clear the cache as matrix changed
        i <<- NULL
    }
    
    ## Get Matrix
    get <- function() x
    
    ## Set the new inverse
    ## Will be called by cacheSolve to set the new inverse value
    setinv <- function(inv) i <<- inv
    
    ## Get the inverse value. Should return NULL if matrix is updated
    getinv <- function() i
    
    ## Return the list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## Compute inverse using cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinv()
    
    # If data is cached, just return the cached data
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Inverse not cached. So load the new matrix and compute inverse
    current_matrix <- x$get()
    
    ## Compute inverse for the updated matrix
    i <- solve(current_matrix, ...)
    
    ## Set the cache and return inverse value
    x$setinv(i)
    i
}
