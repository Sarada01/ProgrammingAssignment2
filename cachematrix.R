## Put comments here that give an overall description of what your
## This makeCacheMatrix function creates a special matrix that contains following list of functions:
## 1) Sets the matrix (Set)
## 2) Gets the matrix (get)
## 3) sets the Inverse of the Matrix  (setInverse)
## 4) Gets the Inverse of the Matrix  (getInverse)

makeCacheMatrix <- function(x = matrix()) {

  inverseMatrix <- NULL
  
  set <- function (m) {
      x <<- m
      inverseMatrix <<- NULL
    
  }
  
  get <- function() x
  
  setInverse <- function (m) inverseMatrix <<- m
  
  getInverse <- function() inverseMatrix
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
   
}


## The cacheSolve function will calculate the Inverse Matrix of a matrix that is created using above function makeCacheMatrix.
## This function will calculate the inverse matarix and cache the value.  In the subsequent calls it cached value instead of recomputing again.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  
  if(!is.null(invMatrix)) {
    
      message("Cached Inverse matrix is returned")
      return (invMatrix)
  }
  
  m<- x$get()
  
  invMatrix <-  solve(m)
  
  x$setInverse(invMatrix)
  
}
