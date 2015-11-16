## The following two functions are used to cache the inverse of a matrix. 
## When we need the inverse repeatedly, it does not have to be recomputed over and 
## over again but i can be lookup up in the cache. In this way it has to be calculated only once

## Create a cacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) inv <<- inverse
		getinverse <- function() inv
		list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
	}


## Return the inverse of a cacheMatrix object
cacheSolve <- function(x,  ...) {
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




