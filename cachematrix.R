### The following pair of functions cache the inverse of a matrix.
## makeCacheMatrix creates a matrix object
## cacheSolve computes the inverse of a matrix 
## using makeCacheMatrix as a cache


## The function makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function (y) {  
        x <<- y
    }
    
    get <- function() x
    
    setinv <- function(t) inv <<- t
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function computes the inverse of the 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## it retrieves the inverse from the cache
## It assumes that the matrix is always invertible
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    # Retrieve the cached value if any
    m <- x$getinv()
    
    if(!is.null(m)) {
        # Found one
        message("Getting cached inverse")
        return(m)
    }
    
    # No value cached. Need to compute it.
    m <- solve(x$get(), ...)
    
    # Save it for later
    x$setinv(m)
    
    # Return it
    m   
}
