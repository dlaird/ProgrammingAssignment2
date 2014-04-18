## These two functions work together to efficiently manage matrix inverse
## calculation, providing functionality to calculate the inverse of an 
## invertible matrix the first time it is required, and to return a cached 
## copy of the previously calculated inverse on future requests.

## The makeCacheMatrix function contains and manages the source matrix and it's
## inverse.  It provides functionality to set and retrieve these two objects.
## It requires that it receive a a square, invertible matrix, and provides
## error checking to provide more helpful feedback in case it receives
## bad input.

makeCacheMatrix <- function (matrix=matrix()){
        # Requires the matrix be a square, invertible matrix
        # This if/else block checks to make sure that the input is a matrix,
        # that it is square and that it is invertible.  Although the assumption 
        # is that it is, the function will return more helpful feedback
        # in the case that it is given bad input.
        if(!is.matrix(matrix)){
                print("Input is not a matrix!")
        }
        else if (nrow(matrix) != ncol(matrix)) {
                print("Input matrix is not square!")
        }
        else if (det(matrix)==0){
                print("Input matrix is not invertible!")
        }
        # If the input is valid, the requested logic is in this block.
        else {
                matrixInverse <- NULL
                setMatrix <- function (newMatrix){
                        matrix <<- newMatrix
                        matrixInverse <<- NULL
                }
                getMatrix <- function() matrix
                setInverse <- function(inverse) matrixInverse <<- inverse
                getInverse <- function() matrixInverse
                list(setMatrix = setMatrix, getMatrix = getMatrix,
                     setInverse = setInverse, getInverse = getInverse)
        }
        
}


## The cacheSolve function takes as input an object of type makeCacheMatrix,
## calculates the inverse of the source matrix if has not been calculated
## before, and returns a cached copy of the inverse if it has already
## been calculated.

cacheSolve <- function (matrixList, ...){
        # Assumes matrixList is an object of type makeCacheMatrix
        inverse <- matrixList$getInverse()
        # If inverse has been previously calculated and cached
        # the stored value is returned.
        if(!is.null(inverse)){
                message("getting cached inverse")
                return(inverse)
        }
        # If the inverse has not been previously calculated, this function
        # retrieves the source matrix and calculates the inverse.
        data <- matrixList$getMatrix()
        # Invertability is checked before the inverse is calculated.
        # Although there is one check already for this in makeCacheMatrix()
        # a non-invertible matrix could have been specified by means of
        # setMatrix().
        if (det(data)==0){
                inverse = NULL
        }
        else{
                inverse <- solve(data, ...)
        }
        matrixList$setInverse(inverse)
        inverse
}

# Test Script
# f<-matrix(c(1,1,1,0,3,2,4,6,1),3,3) # a good matrix
# d<-matrix(1:9,3,3) # a non-invertible matrix
# a<-makeCacheMatrix(f)
# cacheSolve(a)
# cacheSolve(a) # see that it returns the cached value
# a$setMatrix(d)
# cacheSolve(a) # see that it returns a NULL, not an error
# a$setMatrix(f)
# cacheSolve(a)
# cacheSolve(a) # see that it returns the cached value
