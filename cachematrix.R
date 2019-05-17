## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #in this function we create a matrix to calculate the inverse and store the inverse matrix. 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(matrixInv) m <<- matrixInv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  # if inverse of matrix exist return it 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ma <- x$get()
  # check if the matrix is singular to calculate the inverse 
  if (det(ma)!=0) { 
    m <- solve(ma, ...)
    x$setInverse(m)
    m
  }
  # otherwise notify user that matrix is not singular 
  else{ 
    message("The given matrix is not singular")
  
  }
 
}
