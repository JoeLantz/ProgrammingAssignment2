## makeCacheMatrix creates four functions that are returned in a list
## Input is a invertible matrix
## set() stores the matrix in the parent environment
## get() gets the stored matrix
## setinv() caches the inverse of the matrix in the parent environment
## getinv() returns the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
	  cacheinv <- NULL
        set <- function(y) {
		 x <<- y
		 cacheinv <<- NULL
	  }
	  get  <- function() x
        setinv <- function(solve) cacheinv <<-solve    
        getinv <- function() cacheinv 
        list(set = set,
		 get = get,
		 setinv = setinv, 
             getinv = getinv)
}
## cacheSolve attempts to retrieve the inverted matrix
## If the inverted matrix has been stored, it retrieves the inverted matrix, 
## the inverted matrix is the output of the function.
## If no inverted matrix is found, the original stored matrix is retrieved, 
## the inverse is calculated using solve(), the inverted matrix is cached in the 
## parent environment, the inverted matrix is the output of the function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 	cacheinv <- x$getinv()
	if (!is.null(cacheinv )) {
		message("Retrieving the cached inverse")
		return(cacheinv )
	}
	data <- x$get()
	cacheinv <- solve(data, ...)
	x$setinv(cacheinv )
	message("Returning calculated inverse")
	return(cacheinv )
}

# Invertible matrix to test the functions
mat3x3 <- rbind(c(2,1,0), c(2,0,0),c(2,0,1) )  # an invertible matrix
x <- makeCacheMatrix(mat3x3)
cacheSolve(x)
cacheSolve(x)

mat2x2 <- rbind(c(1, -1/4), c(-1/4, 1))  # an invertible matrix
x <- makeCacheMatrix(mat2x2)
cacheSolve(x)
cacheSolve(x)



