
makeCacheMatrix <- function(x = matrix()) {

  matInv <- NULL
  set <- function(y){
    x <<- y
    matInv <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matInv <<- inverse
  getinverse <- function() matInv
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInv <- x$getinverse
  if(!is.null(matInv)){
    message("getting cached data")
    return(matInv)
  }
  data <- x$get()
  matInv <- solve(data, ...)
  x$setinverse(matInv)
  return(matInv)
}
