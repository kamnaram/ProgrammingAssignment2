## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.


## The two pairs of functions defined creates special objects to store a matrix 
## and inverse its cache.



## makeCacheMatrix creates a special "matrix" object and contains a list of 
## functions to 
##       1. set the value of the matrix
##       2. retrieve the value of the matrix
##       3. inverse of the value of the matrix
##       4. retrieve the inverse of the value of the matrix



makeCacheMatrix <- function(x = matrix()) {
        
        tab <- NULL 
        set <- function(y) {
                x <<- y
                tab <<- NULL
        }
        
        get <- function() x
        
## The "<<-" operator below assigns a value to an object in an environment 
## different from the current environment.

        setInverse <- function(inverse) tab <<- inverse
        getInverse <- function() tab
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve is the second special object that computes the inverse of the 
## special "matrix" created with the above function. 

## The solve() function computes the inverse of the matrix "mat" where "mat" is 
## a square matrix.

## The below function first checks if the inverse has already been calculated 
## and retrieves the inverse from the cache if it has been otherwise 
## computes it.



cacheSolve <- function(x, ...) {
        
        tab <- x$getInverse()
  
## checks if inverse has been calculated
        if(!is.null(tab)) {      
                
## gets the inverse from the cache and skips the computation
              message("getting cached data")  
              return(tab)
        }

## Otherwise, computes the inverse of the matrix via the solve() function.
        mat <- x$get()
        tab <- solve(mat, ...) 

## sets the value of the inverse in the cache
        x$setInverse(tab)   
        tab
}
