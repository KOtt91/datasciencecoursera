## makeCacheMatrix creates a matrix that can chache its inverse.
## cacheSolve calculates the inverse of the matrix created in 'makeCacheMatrix' and returns the value... unless the inverse has already been calculated in which case the inverse of the cache is simply returned.

## 1. Set value of the matrix
## 2. Get value of the matrix
## 3. Set value of inverse of the matrix
## 4. Get value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
		}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(
		set = set, get = get, 
		setinv = setinv, 
		getinv = getinv)
	}


## Returns the inverse of the matrix created in the makeChacheMatrix if the inverse has not yet been calculated. If the inverse has been calculated already then retrieves the inverse (getting cached data)

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getting cached data.")
        	return(inv)
        	}
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        }
