## the following functions cache the inverse of a matrix to reduce
## computation costs associated with matrix inversion

## the makeCacheMatrix function is used to create a 'matrix' object
## to cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #set stored inverse value (i) to NULL
  i <- NULL
  
  #function to set the value of the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  #function to get the value of the matrix
  get <- function() x
  
  #function to set the value of the inverse of the matrix
  setinverse <- function(solve) i <<- solve
  
  #function to get the value of the inverse of the matrix
  getinverse <- function() i
  
  #list with the avaliable functions
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## the cacheSolve function will find the inverse of the matrix created
## with makeCacheMatrix if the inverse has not yet been calculated.

cacheSolve <- function(x, ...) {
  ##return a matrix that is the inverse of 'x'
  
  ##check to see if the inverse already exists (is cached)
  i <- x$getinverse()
    if(!is.null(i)){
      message("retrieving cached data")
      return(i)
    }
  
  #if it not cached, 'get' the matrix 
  data <- x$get()
  
  #find the inverse
  i <- solve(data, ...)
  
  #cache the results
  x$setinverse(i)
  
  #return the inverse
  i
  
}
