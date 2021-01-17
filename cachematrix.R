

## This function creates a matrix that can cache its inverse.
## Stores matrix x  and i in the environments of set, get, setinv, and getinv functions.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y){
                x <<- y
                i <<- NULL
                
        }
        
        get <- function() x
        
        
        setinv <- function(inverse) i <<- inverse
        
        
        getinv <- function() i
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calls upon aleady created matrix and calculates the inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        
        if(!is.null(i)){
                message("getting cached data")
                return(i)
                
        }
        
        mx <- x$get()
        
        i <- solve(mx,...)
        
        x$setinv(i)
        i
        
        ## Return a matrix that is the inverse of 'x'
}
