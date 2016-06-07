## These two functions are used to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
 	setinverseM <- function(inverseM) m <<- inverseM
	getinverseM <- function() m
	list(set = set, get = get,
		setinverseM = setinverseM,
		getinverseM = getinverseM)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated(and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverseM()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverseM(m)
	m
}
