## This file is used to cache the inverse of a matrix as 
## this computation is time consuming. This file contains two 
## functions: makeCacheMatrix() and cacheSolve().  

## The makeCacheMatrix() function creates a special R object which
## sets and gets values for a matrix and its inverse. Here, x and inv
## are objects defined in the parent environment whereas set(), get(), 
## setinverse() and getinverse() are
## functions defined in the environment of makeCacheMatrix()

makeCacheMatrix <- function(x) {
         inv <- NULL
         set <- function(y) {
	     x <<- y
	     inv <<- NULL
}
         get <- function() x
         setinverse <- function(inverse) inv <<- inverse
         getinverse <- function() inv
         list(set=set, get=get, 
              setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve() function requires an input argument,
## of the type makeCacheMatrix() function. If the inverse
## already exists, it retrieves the inverse from the cached value
## which is stored in makeCachematrix() object environment.
## Else, it computes the inverse using the solve() function and 
## sets the inverse in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
