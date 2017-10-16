
## The combination of these two functions computes the
## inverse of matrix using the cached valued if previously computed.
##
## Usage: a <- makeCacheMatrix(x), where x is a matrix.
## cacheSolve(a)

## The makeCacheMatrix fuction creates a "special matrix" that is actually a list of 
## four functions, set, get, setinv, getinv. 
##
## 1. a$set(x): updates the "special matrix" a, so that
##    its argument matrix is x (with x being an ordinary matrix)
## 2. a$get(): returns the ordinary matrix contained in the "special
##    matrix a. 
## 3. a$setinv: sets the value of the inverse of the matrix x
## 4. a$getinv: returns the inverse matrix of x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The cacheSolve fuction returns a matrix that is the inverse of a matrix 'a'.
## The matrix 'a' must be a 'special matrix' created with makeCacheMatrix. 
## If the inverse has been already computed the function returns the cached 
## value, if not it will compute the inverse value.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
  
}
