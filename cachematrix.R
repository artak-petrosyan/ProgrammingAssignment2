##cachematrix.R: Programming Assignment 2: Lexical Scoping

## creates a special wrapper "vector" for a matrix, which is really a list of the functions:
## set() - sets the value of the matrix
## get() - returns the value of the matrix
## setInverse() - sets the value of the inverse matrix
## getInverse() - returns the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	invm <- NULL
      set <- function(origm) {
      	x <<- origm
		invm <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) invm <<- inv
	getInverse <- function() invm
	list(set = set, get = get,
	setInverse = setInverse ,
	getInverse = getInverse )

}


## returns cached value of the inverse matrix if exists,
## otherwise calls "solve" function for the matrix wrapped in the special 'list' created by 'makeCacheMatrix', 
## stores result as a cached variable in the environment and returns it.

cacheSolve <- function(x, ...) {
	im <- x$getInverse ()
      if(!is.null(im)) {
		message("getting cached data")
            return(im)
	}
	data <- x$get()
	im <- solve(data, ...)
	x$setInverse(im)
	im
	
}
