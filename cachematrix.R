## Matrix inversion is a costly process. These functions will solve for the
## inverse of the matrix and cache the results for later usage.

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Create an empty container for the cached matrix
    cached <- NULL
    
    ## A Set function for the cache matric
    set<-function(y){
        x <<- y
        cached <<- NULL
    }
    
    get <- function() x ## A get function for the matrix
    
    setmatrix <- function(solve) cached <<- solve ## Setting the cache matric
    
    getmatrix <- function() cached ## Getting the cache matric
    
    ## Collapsing it all together into a returned list
    list(set = set, get = get, 
         setmatrix = setmatrix, 
         getmatrix = getmatrix)
    
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getmatrix() ## Retrieving the cached matrix
    if(!is.null(m)){ ## Checking to see is the cached matrix exists
        message("getting cached data")
        return(m)
    }
    
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    return(m)
}
