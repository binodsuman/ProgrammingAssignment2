## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly.
## here are the two methods to create matix and catch their inverse.

## This method will create matrix on given function argument. Which has list of four method
## set: This method will set the value of matrix.
## get: This method will return the matrix as it is.
## setInverse: This method will set the inverse matrix data.
## getInverse: This methos will return saved inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
	# This matInverse variable will store the final inverse of matrix value. Initializing with NULL value at starting.
	matInverse <- NULL
	
    # This method will set matrix of different value as per the given function argument.	
	# And again it will set matInverse to NULL 
	  set <- function(mat) {
		x <<- mat
		matInverse <<- NULL
	  }
	  
	 # This method will simply return the actual matrix either created initialize time or using set method.
	  get <- function() {
		x
	  }
	  
	 # This method will set the inverse of matrix as per the method argument.
	  setInverse <- function(inverseMat) {
		matInverse <<- inverseMat
	  }
	  
	 # This method will return the inverse of matrix.
	  getInverse <- function() {
		matInverse
	  }
	  
	 # List of all accessible method name.
	  list(set = set,
		   get = get,
		   setInverse = setInverse,
		   getInverse = getInverse)
}


## This method actually take matrix and check if the inverse of given matrix is already available.
## If available then simply return the inverse of matrix with print message that is "getting cached data"
## If does not available then first it will calculate the inverse of matrix using solve command and return inverse matrix.

cacheSolve <- function(x, ...) {
     
	 ## Return a matrix that is the inverse of 'x'
	  matInverse <-x$getInverse()
  
    # First check if given matrix is already available then simply return inverse matrix with message "getting cached data"
	  if(!is.null(matInverse)){
		message("getting cached data")
		return (matInverse)
	  }
	  
	# Below code only applicalbe when inverse matrix is not available earlier. 
	# Getting actual matrix.
	  mat <- x$get()
	# Calculate inverse of matrix using solve command and assign to matInverse variable.  
	  matInverse<-solve(mat, ...)
	# Setting matInverse to the matrix variable.  
	  x$setInverse(matInverse)
	# Returning the inverse matrix.  
	  matInverse
}
