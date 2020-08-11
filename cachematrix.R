## this pair of functions creates a matrix, inverts the matrix, and stores
## the result in the cache

## makeCacheMatrix is a function which sets and gets the value of a matrix,
## then sets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) m<<-solve
	getinverse<-function() m
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve inverts the matrix created with makeCacheMatrix,
## but first checks to see if the inverse has already been stored in the cache
## if the inverse is in the cache, the function returns the cached value

cacheSolve <- function(x, ...) {
	m<-x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setinverse(m)
	m
}
