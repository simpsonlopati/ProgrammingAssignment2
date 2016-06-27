# function based on the makeVector function from the assignment.
makeCacheMatrix <- function(x = numeric()) {
#set intial values of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
  inv <<- NULL
  }
#set funtions to inv matrix x and makes the "special vector"
  get <- function() x
  invmatrix <- function(solve) inv <<- solve
  getmatrix <- function() inv
  list(set = set, get = get,
       invmatrix = invmatrix,
       getmatrix = getmatrix)
}

# creates the inverse of the matrix calculated in makeCacheMatrix
cachesolve <- function(x, ...) {
  #inv defines as the inversed matrix read from the special vector
  #if the value of inv is not null that then the inverted matrix returned from the cache.
  inv <- x$getmatrix()
  if(!is.null(inv)) {
    message("getting inverted matrix")
    return(inv)
  }
  #the matrix x is inverted if the value of null is found for inv.
  data <- x$get()
inv <- solve(data, ...)
  x$invmatrix(inv)
  inv
}

#Testing matrix 
mat <- makeCacheMatrix(matrix(1:4, 2, 2))
cachesolve(mat)

