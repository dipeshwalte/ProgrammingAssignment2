

## This function makes the matrix and its inverse available for access  

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

## This function computes the matrix inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  if(det(data) == 0) {
    message("Cannot get the inverse as determinant is zero")
    return(NULL)
    }
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
