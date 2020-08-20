## This code has been written to formulate the inverse of a matrix repeatedly

## The first function creates the 'special' matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL  ##setting the value of the matrix
  }
  get <- function()x   ##getting the value of the matrix to be inverted
  setInverse <- function(inverse) j <<- inverse   ##setting the value of the inverse
  getInverse <- function() j  ##getting the value of the inverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function computes the inverse of the matrix returned by the function above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j) ##checking to see if the inverse has already been calculated
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j) ##in case of matrix not inverted, the inverse is calculated and set through the setInverse function
  j
}
