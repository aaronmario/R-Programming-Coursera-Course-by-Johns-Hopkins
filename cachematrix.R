# makeCacheMatrix function contains a list of functions as below
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
	set <- function(y) 
	{
      	x <<- y
      	inv <<- NULL
    	}
	
	get <- function() x
    	
	setinverse <- function(inverse) inv <<- inverse #using lexical scoping to return inverse value from different function body
    	
	getinverse <- function() inv
    	
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
# If not calculated it will solve for inverse
# if present in cache it will return saved inverse value
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse() #assigns NULL or saved inverse value
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv) #if stored in cache return inv and exit function. Wont recompute inverse
    }
    data <- x$get()
    inv <- solve(data) # using solve function to return inverse of matrix
    x$setinverse(inv)
    inv
}