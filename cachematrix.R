## The functions cache the inverse of a matrix

## The makeCacheMatrix contains:
# SET FUNCTION that assigns to x a new matrix y and reset the inverse matrix m
# GET FUNCTION that returns a matrix x
# SET INVERSE FUNCTION that assigns to m the inverse matrix solve
# GET INVERSE FUNCTION that returns the inverse matrix m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function (solve) m <<- solve
  getinverse <- function() m 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve retrieves the inverse matrix of a matrix x, verify if the inverse is already calculated. 
## If yes, it returns the inverse matrix
## If not, it retrieves the original matrix, calculates de inverse and assign the result to the cached object

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

