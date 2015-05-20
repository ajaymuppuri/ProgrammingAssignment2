## 
# makeCacheMatrix 
# Arguments - Matrix
# This function atcs as a getter/setter of inverse method 
# for the matrix passed

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##
# cachesolve 
# Arguments - List created by the makeCacheMatrix function
# This function checks its own cache (simple variable) whether
# the value exists or not and then returns based on that. 

cachesolve <- function(x, ...) {
    # basic check if the passed value is a list or not
	if(typeof(x) == "list"){
    	    i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i    
    }
}
