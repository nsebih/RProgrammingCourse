##The two functions participate to solve the inverse of a matrix (provided it is squared), and store in cache the result for subsequent calls to computation of the inverse of the same matrix. Every time a net matrix is sat, inverse is reinitialized to null values.

## Provides getters and setters for the matrix, return an object (list) of these functions.
	
makeCacheMatrix <- function(x = matrix()) {
	#reject a matrix that is not square; non-square matrices can only have a pseudo-inverse or best approximation
	if(nrows(x)!=ncols(x))
		return "Error: non square matrix"
	
	#in case function's arguments is square:
	r<-nrows(x)
	c<- ncols(x)
	#iniialize the inverse matrix of argument to null values
	inverse <- matrix(NULL, r,c)	
	
	setMatrix <- function(y){
		x <<- y
		r<-nrows(x)
		c<- ncols(x)
		#perform same check for dimension of the matrix, in case it is set with setMatrix function:
		if(nrows(x)!=ncols(x))
			return "Error: non square matrix"
		#reinitilize matrix inverse to null values
		inverse <<- matrix(NULL, r,c)
	}
	
	getMatrix <- function() x
	
	setMatrixInverse <- function(i) inverse <<-i
	getMatrixInverse <- function() inverse
	#return an object consisting of setters/getters for matrix and its inverse
	list(setMatrix= setMatrix, getMatrix=getMatrix, setMatrixInverse=setMatrixInverse, getMatrixInverse=getMatrixInverse)
		
}


## Retrieve the inverse of a matrix if already cached, otherwise compute it store it and return it.

cacheSolve <- function(x, ...) {
        
		inverse <- x$getMatrixInverse()
		
		if(inverse[1][1]!=NULL){
			#if the inverse has already been computed 
			return inverse
		}
		else{
			#if inverse has not yet been computed, solve it, cache it then return it
			matrix <-x$getMatrix()
			inverse <- solve(matrix)
			x$setMatrixInverse(inverse)
			return inverse	
		}

}
