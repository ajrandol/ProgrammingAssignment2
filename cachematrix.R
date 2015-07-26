## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions are used to cache the inverse of a matrix
## Example: 
##   source("cachematrix.R")
##   c1 = rbind(c(1, -1/4), c(-1/4, 1)) 
##   m1 <- makeCacheMatrix(c)
##   cacheSolve(m1)
##  first call of cacheSolve(m1) computes the inverse and caches
##  second call of cacheSolve(m1) gets the cached data


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	# initialize to NULL
	m <- NULL
	# create the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	## get the value
	get <- function() x
	# invert the matrix and store in cache
	setinverse <- function(inverse) m <<- inverse
	# get the inverted matrix from cache
	getinverse <- function() m
	# return the created functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated then it retrieves the inverse from the cache.
## Else it computes the inverse and adds it to the cache.
cacheSolve <- function(x, ...) {        
	## get the inverse of the matrix stored in cache
	cache <- x$getinverse()
	## if the inverse exists (not null), then return it.
	if(!is.null(cache)) {
		message("Getting cached data")
		return(cache)
	}
	## else compute the inverse
	message("Not cached. Computing the inverse")
	data <- x$get()
	cache <- solve(data, ...)
	# set inverted matrix in cache
	x$setinverse(cache)
	# return matrix
	cache               
}
