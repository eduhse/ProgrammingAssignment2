<<<<<<< HEAD
## These functions written for Coursera R Programming course

##creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                             
  set <- function(y) {                   
    x <<- y                            
    i <<- NULL                       
  }
  get <- function() x                    
  
  setinverse <- function(inverse) i <<- inverse  
  getinverse <- function() i                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  
}


##computes the inverse of the matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("get cache data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
=======
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
>>>>>>> 7f657dd22ac20d22698c53b23f0057e1a12c09b7
}
