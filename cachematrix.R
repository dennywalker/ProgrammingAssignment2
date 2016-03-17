## These two function are the programming assignment for 
## Coursera R Programming, Week 3, Programming Assignment 2: Lexical Scoping
##
## Submitted by: Denny Walker
## email: denny@10baybeach.com


## This function contains four functions
##    set        - sets the value of the matrix
##    get        - gets the value of the matrix
##    setinverse - sets the value of the inverse
##    getinverse - gets the value of the inverse

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function will check to see if the inverse of the matrix has already
## been calculated.  If so, it returns the cached inverse.  If not, it
## calculates the inverse of the matrix.

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
