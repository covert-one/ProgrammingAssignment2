## Two functions that cache the inverse of a matrix

## Creates a special matrix that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Start the inverse property
    i <- NULL

    ## A method of setting the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## A method of getting the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## A method of setting the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## A method of gettig the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods used
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix". 
## If the inverse has already been calculated and the matrix has not
## changed, then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return the inverse matrix of 'x'
    m <- x$getInverse()

    ## If the inverse is already set, simply return it
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix 
    data <- x$get()

    ## Calculate the inverse of the matrix using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
