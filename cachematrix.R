## A function that creates a matrix object and its cache
makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize inverse as NULL
        inverse <- NULL
        
        # Function to set the matrix
        set <- function(y) {
                
                # Set the matrix
                x <<- y
                # Set the inverse as NULL to avoid errors
                inverse <<- NULL
        }
        
        # Function to get the matrix
        get <- function() x
        
        # Function to set the cached inverse
        setInverse <- function(inv) { 
                inverse <<- inv
        }
        
        # Function to get the cached inverse
        getInverse <- function() {
                inverse
        }
        
        # Return list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## A function that computes and caches the inverse of a matrix
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

## Test the functions
mymatrix <- makeCacheMatrix()
mymatrix$set(matrix(c(1.123,2.123,3.123,4.234,5.456,6.234,7.234,8.234,9.432), nrow = 3))
cacheSolve(mymatrix)