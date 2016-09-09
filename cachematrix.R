## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	matInverse <- NULL
	  
	  set <- function(mat) {
		x <<- mat
		matInverse <<- NULL
	  }
	  
	  get <- function() {
		x
	  }
	  
	  setInverse <- function(inverseMat) {
		matInverse <<- inverseMat
	  }
	  
	  getInverse <- function() {
		matInverse
	  }
	  
	  list(set = set,
		   get = get,
		   setInverse = setInverse,
		   getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  matInverse <-x$getInverse()
  
	  if(!is.null(matInverse)){
		message("getting cached data")
		return (matInverse)
	  }
	  
	  mat <- x$get()
	  matInverse<-solve(mat)
	  x$setInverse(matInverse)
	  matInverse
}
