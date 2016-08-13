makeCacheMatrix <- function(x = matrix()) {
  # assigning inverseMatrix as NULL value
  inverseMatrix <- NULL
  # get function to get Matrix
  get <- function() {
    x
  }
  # set function to set a new Matrix
  set <- function(newMatrix) {
    x <<- newMatrix
    inverseMatrix <<- NULL
  }
  # getInverse to get InverseMatrix
  getInverse <- function() {
    inverseMatrix
  }
  # setInverse with Inverse Matrix solution
  setInverse <- function(solvedMatrix) {
    inverseMatrix <<- solvedMatrix
  }
  #return a list of functions
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}
## function cacheSolve computes the inverse of the special "matrix" created by
## function makeCacheMatrix above
cacheSolve <- function(x, ...) {
  # querying and asssigning inverseMatrix cached value
  inverseMatrix <- x$getInverse()
  # checking value of cache and using it if not NULL
  if(!is.null(inverseMatrix)) {
    message("getting cached Inverse Matrix data ....")
    return(inverseMatrix)
  }
  # gettting matrix and calculating inverse
  data <- x$get()
  solve(data, ...)
}