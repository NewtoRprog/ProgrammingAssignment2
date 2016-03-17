###Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of
##a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will
##not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This function will help in making a matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv<- NULL
## set function helps in assigning a matrix to x through y. It can accept either
## 1X1 matrix or 2X2 or 3X3 similarly nXn

	set <- function(y){
		x <<- y
		inv <<- NULL
	}
##Get function will display the matrix which was given by user as argument set function.

	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}



## This will create and return inverse of matrix which is created in above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
## This will check if inv is not null, already existing data then goes into loop.
	if(!is.null(inv)){
		message("cached data")
		return(inv)
	}
## Set matri equals to matrix which was entered in first function
	matri <-x$get()
## Create inverse of matri
	inv <- solve(matri, ...)
	x$setinverse(inv)
	inv
}
