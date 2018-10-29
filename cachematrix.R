## Programming Assignment 2

## makeCacheMatrix() will get a matrix from the browser/ user and will set 
## the given matrix in the variable "m" here which will reside in parent 
## environment (using << operator)

makeCacheMatrix <- function(m = matrix()) {

  
  inv <- NULL	 
  
  set <- function(tempmat) {
    #initializing variables
    m <<- tempmat				
    inv <<- NULL	
  }
  
  get <- function() m
  #Setting inverse of matrix m into inv variable
  setInverse <- function() inv <<- solve(m)
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function gets the inverse of the matrix "inv" which it can
## access due to the variable residing in an parent environment or cache
## This function will return the inv or inverse of the matrix given

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  inv <- m$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  matr <- m$get()
  inv <- solve(matr, ...)
  m$setInverse(inv)
  inv
  
}


##Example of how to call function

#fn <- makeCacheMatrix()
#fn$set(matrix(1:4, 2))
#fn$get()
#fn$setInverse()
#fn$getInverse()
