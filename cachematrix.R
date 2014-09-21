## The objective of the functions below is to cache the inverse of a matrix so that the   
## computation involved need not be carried out every time the inverse of a matrix is called


## First function defines the matrix function that sets the inverse matrix and gets the return value

makeCacheMatrix <- function(x = matrix()) {
	inv_mat <- NULL
  print(x)
  get <- function() x
  setinv <- function(inv) inv_mat <<- inv
  getinv <- function() inv_mat
  list( get = get,
       setinv = setinv,
       getinv = getinv)
       
       }


## Second function calculates the inverse of the marix if it is null, if not returns the cached value

cacheSolve <- function(x, ...) {
         inv_mat <- x$getinv()
  if(!is.null(inv_mat)) {
    message("getting cached matrix")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setinv(inv_mat)
  inv_mat
}
