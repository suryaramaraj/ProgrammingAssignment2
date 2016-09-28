## Put comments here that give an overall description of what your
## functions do

##The "makeCacheMatrix" creates a special object to create a matrix and caches its inverse
## The "cacheSolve" matrix essentially retrives the inverse of the passed matrix if it exists, 
## else inverses the matrix using Solve function and returns it

## Write a short comment describing this function
##caches the matrix
makeCacheMatrix <- function(x = matrix()) {
  matINV <- NULL
  set <- function(y){
    x <<- y
    matINV <<- NULL
  }
  get <- function() x
  setmat <- function(matrix) matINV <<- matrix
  getmat <- function() matINV
  list(set = set, get=get, setmat = setmat, getmat = getmat)
}


## Write a short comment describing this function
## retrives cached inverse matrix or inverses the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matINV <- x$getmat()
  if(!is.null(matINV)){
    message("getting cached data")
    return(matINV)
  }
  data <- x$get()
  matINV <- solve(data,...)
  x$setmat(matINV)
  matINV
}
