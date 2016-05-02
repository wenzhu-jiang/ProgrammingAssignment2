## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## inverse starts with NULL
  i <- NULL   
  ## define the matrix and its inverse
  set <- function(y){ 
    x <<- y
    i <<- NULL
  }
  ## get function gets the value of the matrix x
  get <- function() x
  ## setInverse function define the inverse
  setInverse <- function(inverse) i <<- inverse
  ## get the inverse value i
  getInverse <- function() i
  ## return a list of set and get for matrix and matrix inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  ## if the inverse was already calculated, return the existing inverse value
  if(!is.null(i)){
    return(i)
  }
  ## if the inverse does not exist, get the value of the matrix first
  data <- x$get()
  ## then calculate the inverse of the matrix
  i <- solve(data)
  x$setInverse(i)
  i
}
