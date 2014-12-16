## Matrix inversion is one of time consuming operation, the
## matrix inversion is to calculate of inverse of matrix.
## Eg: A is matrix then A^-1 is matrix inverse. The following
## two function calculate the inverse of matrix and store it in
## Cache for easy retrival

## makeCacheMatrix creates a list with the following functionality
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function checks whether the inverse of matrix
## is already computed and available in cache, if so the return the 
## inverse from cache, else calculate the inverse set the value in cache 
## and returns it

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    print(cat("getting cached data.\n"))
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  return(inver)
}
