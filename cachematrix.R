#contains functions:
#makeCacheMatrix
#cacheSolve


#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function (x = matrix ()) {
  inverse <- NULL
  
  #set matrix
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }
  
  #get matrix
  get <- function() x
  
  #store inverse into makecachematrix object
  setinverse <-  function (solve) inverse <<- solve
  
  getinverse <- function() inverse
  list(set = set, get = get, setinverse=setinverse, getinverse = getinverse)  
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by 
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  # if inverse has been computed already, it can be returned
  if (!is.null(inverse)){
    message("getting cached inverse")
    return(inverse)
  }
  # otherwise - compute inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

#some test values
testmatrix <- matrix(1:4,2,2)
z <- makeCacheMatrix (x= testmatrix)
solved <- cacheSolve(z)
test_solved  <- cacheSolve(z) %*% z$get() 
#result should be an identity matrix of size 2x2, if the matrix is solvable
