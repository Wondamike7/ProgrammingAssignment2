## This code has two functions: makeCacheMatrix and cacheSolve
## These functions cache the value of a matrix inverse to be retrieved easily and quickly if needed again

## makeCacheMatrix creates a list of functions to use with a given matrix
## Get returns the matrix, Set sets the matrix, getinverse returns the matrix inverse, and setinverse sets the matrix inverse (via solve)
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse) 
	## Returns a list of four functions to use with cacheSolve
}

## cacheSolve function calculates the inverse of the matrix, unless it has already been calculated
## if the inverse already exists, this function gets the inverse from the cache
## if the inverse doesn't exist, this 
cacheSolve <- function(x, ...) {
	## Check to see if the inverse already exists
	## if it does, return the inverse and exit the function
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## If the inverse wasn't already created, take the input matrix and solve for the inverse
	input_matrix <- x$get()
	m <- solve(input_matrix, ...)
	## Set the inverse matrix in our function list, x, for future use
	x$setinverse(m)
	m
}
