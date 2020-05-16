## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function gets a matrix as an input, sets the value of the matrix,
#gets the value of the matrix, sets the inverse Matrix and gets the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix<-NULL
  
  setMatrix <- function(y){
    x <<- y
    invMatrix <<- NULL
    
  }
  
  getMatrix <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse  # assigns value of inv in parent environment
  getInverse <- function() invMatrix                     # gets the value of inv where called
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
    message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
    return(invMatrix)                             #return the invertible matrix
  }
  MatrixData <- x$getMatrix()                     #get the original Matrix Data 
  invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
  x$setInverse(invMatrix)                         #set the invertible matrix 
  return(invMatrix)  
}
