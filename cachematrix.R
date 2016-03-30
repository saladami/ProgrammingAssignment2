#used to store a square matrix and its inverse, to be manipulated indirectly via set/get/setinverse and getinverse
#e.g. my_matrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#will either compute the inverse of matrix associated with x
#or return the cached value

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