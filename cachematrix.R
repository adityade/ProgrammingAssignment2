## A pair of functions that cache the inverse of a matrix

# makeCacheMatrix is a function that returns a list of functions
# Its purpose is to store a matrix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * set            set the value of a matrix
# * get            get the value of a matrix
# * setInverse     set the cached value (inverse of the matrix) in cache for further use
# * getInverse     get the cached value (inverse of the matrix)
#

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x , ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    ## m <- solve(data) %*% data
    m <- solve(data)
	
    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
