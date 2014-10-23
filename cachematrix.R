## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	setMat <- function(m){
		x <<- m
		inv <<- NULL
	}
	getMat <- function() x
	setInv <- function(invIn) inv <<- invIn
	getInv <- function() inv
	list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInv()
	if(!is.null(inverse)){
		message("retrieving cached matrix inverse...")
		return(inverse)
	}
	mat <- x$getMat()
	inverse <- solve(mat,diag(dim(mat)[1]),...)
	x$setInv(inverse)
	inverse
}
