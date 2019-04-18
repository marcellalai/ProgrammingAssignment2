##  makeCacheMatrix() function creates a special 'matrix' object that can cache its inverse. 
##  It returns a list of functions to:
##  ./ set value of matrix
##  ./ get value of matrix
##  ./ set value of inverse of  matrix
##  ./ get value of inverse of  matrix
##  This list is input to cacheSolve() function


makeCacheMatrix <- function(x = matrix())  # x is a invertible matrix
{
  #initialize xInv    
  xInv <- NULL
  
  setMatrix <- function(y)  
  {
    x <<- y 
    xInv <<- NULL
    # x & y are assigned to an environ. different from current one
  }
  
  getMatrix <- function() return(x)  # function returns matrix x stored in global env
  
  setMatrixInverse <-  function(inverse)  xInv <<- inverse
  
  getMatrixInverse <-  function()  xInv 
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
  
}



##  cacheSolve() function calculated (solve) inverse of a 'matrix' returned by makeCacheMatrix.
##  If inverse has been already calculated, it retrieves inverse from cache, avoiding calculation.
##  It takes as input x, the matrix returned by makeCacheMatrix

cacheSolve <- function(x,...)
{
  xInv <- x$getMatrixInverse() 
  
  if(!is.null(xInv)) # check if XInv has been already calculated
  {
    message("using cached data") 
    return(xInv)
  }
  
  matrix_data <- x$getMatrix()  # computing inverse because xInv is NULL
  
  xInv <- solve(matrix_data)    
  
  x$setMatrixInverse(xInv)      # storing in cache
  
  return(xInv)
}
