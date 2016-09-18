## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Checks if the matrix is a square matrix and proceeds if the determinant value is not zero

makeCacheMatrix <- function(x = matrix()) {
if(dim(x)[1]==dim(x)[2]) {	
		if (det(x)!=0){	
			m <- NULL
			set <- function(y) {
					x <<- y
					m <<- NULL
			}
			get <- function() x
			setinverse <- function(solve) m <<- solve
			getinverse <- function() m
			list(set = set, get = get,
				setinverse = setinverse,
				getinverse = getinverse)
		}
		else{
			print("enter a matrix whose determinant is not zero")
			}
	}
	else{
	print("enter a square matrix")
	}


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
