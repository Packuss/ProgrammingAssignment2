## A function that creates a matrix object and its cache
makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize inverse as NULL
        inverse <- NULL
        
        # Function to set the matrix
        set <- function(matrix) {
                
                # Set the matrix
                x <<- matrix
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
        # Get the cached inverse from the matrix object
        inv <- x$getInverse()
        # Check if the inverse has been cached
        if(!is.null(inv)) {
                # If yes, return the cached inverse
                message("Getting cached data")
                return(inv)
        }
        # Get the matrix from the matrix object
        data <- x$get()
        # Compute the inverse of the matrix
        inv <- solve(data, ...)
        # Cache the inverse in the matrix object
        x$setInverse(inv)
        # Return the inverse
        inv
}

## Test the functions
mymatrix <- makeCacheMatrix()
mymatrix$set(matrix(c(1.123,2.123,3.123,4.234,5.456,6.234,7.234,8.234,9.432), nrow = 3))
cacheSolve(mymatrix)