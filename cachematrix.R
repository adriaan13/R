## Functions to wrap a matrix so that the inverse of the matrix can be cached in the function scope (makeCacheMatrix), 
## as well as a function to return the inverse of a matrix, using the cached version if available

## makeCacheMatrix wraps a matrix in a function scope. 
##
## Args:
##    x - a matrix, set to an empty matrix by default
##
## Methods:
##    set - set a new matrix
##    get - get the matrix
##    getinverse - get the inverse matrix - getting the cached version if available
##
## Example:
##    x <- makeCacheMatrix(matrix(c(2,1,1,2),nrow=2))
##    x$set(matrix(c(3,2,1,2,3,2,1,2,3),nrow=3))
##    x$getinverse()
##
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	getinverse <- function() {
		if (is.null(inverse)) inverse <<- solve(x)
		inverse
	}
	list(set = set, get = get,
			getinverse = getinverse)
}


## Returns the inverse of a matrix previously wrapped with makeCacheMatrix. This will return the cached inverse if available. 
## This function is equivalent to calling x$getinverse()
##
## Args:
##    x - a matrix wrapped with makeCacheMatrix
##
## Example:
##    x <- makeCacheMatrix(matrix(c(2,1,1,2),nrow=2))
##    cacheSolve(x)
##    cacheSolve(x) %*% ( x$get() %*% c(1,2))

cacheSolve <- function(x, ...) {
        x$getinverse()
}
