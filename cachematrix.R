## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) 
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        ##creat core matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        ##arrange and return
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## check if calculated
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}
