## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	## Initialize the inverse property
        m <- NULL
		
	##Set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
	## Return the matrix
        get <- function() x
	
	## Method to set the inverse of the matrix
        setinv <- function(inverse) m <<- inverse
	## Method to get the inverse of the matrix
        getinv <- function() m
		
	## Return a list of the methods
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix" above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachemean" should retrieve the inverse from the cache.
cachemean <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
	## Just return the inverse if its already set
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
	## Get the matrix from our object
        data <- x$get()
		
	## Calculate the inverse using matrix multiplication
        m <- solve(data)
	## Set the inverse to the object
        x$setinv(m)
	## Return the matrix
        m
}
