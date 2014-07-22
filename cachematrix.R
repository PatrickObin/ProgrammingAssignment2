## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. 
## The following pair of functions cache the inverse of a matrix.
#####################################################################

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invMatrix<-NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        get <- function() x
        setinv <- function(inv) invMatrix <<- inv
        getinv <- function() invMatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##  This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        invMatrix <- x$getinv()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data,...)
        x$setinv(invMatrix)        
        ## Return a matrix that is the inverse of 'x'
        invMatrix
}
