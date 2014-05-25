## Caching computations. This source file contains two functions
## used to compute and cache the inverse of a matrix. These functions
## allows to reduce the execution time, by obtaining the inverse
## of a matrix only once, and, later, retrieving the cached matrix 
## inverse when it is needed.

## makeCacheMatrix
## Builds a named list of four functions to store 
## a matrix and cache its inverse
##
## Args:
##      x: The matrix that will be stored
## 
## Returns:
##      A named list of functions to manage the matrix and its inverse
##      get: retrieves the original matrix
##      set: updates the original matrix
##      setInverse: stores in the cache the inverse of the original matrix
##      getInverse: retrieves from the cache the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
        ## x stores the matrix
        ## m caches the inverse of x
        m <- NULL 
        
        ## set
        ## Stores a new original matrix
        ## and resets the cache for inverse matrix
        ##
        ## Args:
        ##       y: The new original matrix  
        set <- function(y) {                
                x <<- y 
                m <<- NULL
        }
        
        ## get
        ## Retrieves the original matrix
        ## 
        ## Returns:
        ##       The original stored matrix
        get <- function() x
        
        ## setinverse
        ## Stores in the cache the a new inverse matrix
        ## It uses the <<- operator to access to the variable "m"
        ## in the enclosing environment the variable m
        ##
        ## Args:
        ##       inverse: The new inverse matrix to cache
        setinverse <- function(inverse) m <<- inverse 
        
        ## getinverse
        ## Retrieves the inverse of the matrix from cache
        ##
        ## Returns:
        ##       The cached inverse matrix
        getinverse <- function() m
        
        ## Build a named list with the above functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve
## Computes a matrix inverse or retrieves its cached value
## to speed-up program
##
## Args:
##      x: A "cacheMatrix" built with makeCacheMatrix
##
## Returns:
##      The inverse of the matrix stored in x

cacheSolve <- function(x, ...) {
        ## try to obtain the cached inverse
        m <- x$getinverse()     
        if(!is.null(m)) {
                ## if cached inverse matrix exists, we are done. Return it
                message("getting cached data")
                return(m)       
        }
        
        ## If there is no inverse matrix cached, compute it. 
        ## Obtain the original matrix from the cacheMatrix
        data <- x$get()
        ## Compute the inverse of the original matrix
        m <- solve(data, ...)
        ## Store the inverse of the original matrix in the cache
        x$setinverse(m)
        
        ## Return the inverse from the original matrix computed just before 
        m

}
