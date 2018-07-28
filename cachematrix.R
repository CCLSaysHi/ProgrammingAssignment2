## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # Define function to set the value of the matrix. It also clears the old
        # inverse from the cache
        set <- function(y) 
        {
                x <<- y
                inv <<- NULL
        }
        # Define function to get the value of the matrix
        get <- function() x
        # Define function to set the inverse. This is only used by getinverse() when
        # there is no cached inverse
        setInverse <- function(inverse) inv <<- inverse
        # Define function to get the inverse
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
        inv <- x$getInverse() #this fetches the cached value of the inverse
        if (!is.null(inv)) { #check if the cache was not empty
                message("getting cached data")
                return(inv)
        }
        ## check if calculated
        mat <- x$get() #get value of matrix
        inv <- solve(mat, ...) #calculate 
        x$setInverse(inv) #cache
        inv #return the inverse
        
}
