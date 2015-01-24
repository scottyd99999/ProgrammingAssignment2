## The below functions allow an object to be created to store a matrix and cache its inverse in order to 
## avoid the need to repeat the computation costs of calculating the inversion in cases where the 
## matrix has not changed between calculations.
## The functions make use of the super assignment operator and lexical scoping.

## Creates a list holding a set of functions which allow a matrix to be set, retrieved,
## have its inverse set or have its inverse retrieved.

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL ## ensure that there is no cached value for the inverse when the matrix changes
        
        set <- function (y) {
                x <<- y
                inverseMaxtrix <<- NULL
        }
        
        get <- function () x
        
        setInverse <- function (inverse)  inverseMatrx <<- inverse
               
        getInverse <- function () inverseMatrix
        
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Determines the inverse of a matrix, but first checks to see if a the inverse for the matrix in question 
## has been previously calculated and stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##  check to see if there is a cached value for the inverse of the matrix
        inverseMatrix <- x$getInverse() ## read the cache
        
        
        if (!is.null(inverseMatrix)) { ## there is a value in the cache so use it
                
                message("Retrieving cached value...")
                
                return(inverseMatrix)
        }
        
        
        ## no cached value found, so go ahead an calculate the inversion
        
        matrx <- x$get() ## get the matrix to be inverted
        
        inverseMatrix <- solve(matrx, ...) ## work out its inversion
        
        x$setInverse(inverseMatrix) ## cache the inversion
        
        inverseMatrix
}
