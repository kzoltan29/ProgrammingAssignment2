## Assignment 2 data science: 
## functions:


## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
	    m <<- NULL
	}
	get <- function() x
	setcacheSolve <- function(cacheSolve) m <<- cacheSolve
	getcacheSolve <- function() m
	list(set = set, get = get,
		setcacheSolve = setcacheSolve,
		getcacheSolve = getcacheSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getcacheSolve()
		if(!is.null(m)) {
			message("getting cached data")
		    return(m)
		}
	 	data <- x$get()
		m <- solve(data, ...)
		x$setcacheSolve(m)
		m
}