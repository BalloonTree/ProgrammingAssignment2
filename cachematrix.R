makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    ## set function    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## get function    
    get <- function() x
    
    
    ## set inverse function    
    setInverse <- function(inverse) inv <<- inverse
    
    ## get inverse function    
    getInverse <- function() inv
    
    
    ## function construction
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}







cacheSolve <- function(x, ...) {

    ## inverse matrix obtained
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
