## When using the inverse of the same matrix a number of times,
## its best to calculate the result once, cache it, and retrieve it from this
## cache the next time it is needed.

## Function calculates the inverse of a matrix and caches it
## Usage:
##   x <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)   - A matrix
##   y <- makeCacheMatrix(x)                      - Create the object   
##   z <- cacheSolve(y)                           - Retrieve the inverse of the matrix
##   z                                            - z now contains the inversted matrix

## makeCacheMatrix creates a special "vector", which is really a list containing functions to
##   set the value of the matrix
##   get the value of the matrix
##   set the inverse of the matrix
##   get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
			x <<- y
			inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inversematrix) inverse <<- inversematrix
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix
## It first checks whether the inverse of the matrix has already been stored in the cache
## If so, it returns the cached (inverted) matrix.
## If not, it calculates the inverse, stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached inverted matrix")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	## Return a matrix that is the inverse of 'x'
	inverse
}
