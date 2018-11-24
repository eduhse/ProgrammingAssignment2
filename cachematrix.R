## These functions written for of Coursera R Programming course

## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             
    set <- function(y) {               
      x <<- y                             
      inv <<- NULL                       
    }
    get <- function() x                    
    
    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }
  

  ## This function computes the inverse of the matri" returned by makeCacheMatrix
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }