## makeCacheMatrix's output is meant to be assigned to an object with x input into makeCacheMatrix as the matrix we want to solve the inverse for
## cacheSolve's argument is meant to be the object storing makeCacheMatrix's output
## so that cachedatastore <- makeCacheMatrix(somematrix)
## followed by cacheSolve(cachedatastore)
## should return the inverse of somematrix

## the logic of cacheSolve() is to check if the inverse matrix is already stored in cachedatastore and return the inverse if it is cached
## then to retrieve somematrix from cachedatastore and store it in a variable that can be readily accessed from cacheSolve in the form of input
## then to assign the variable inverse as the inverse of input
## then to store this inverse within cachedatastore using setinverse(inverse)
## finally returning inverse as the result of cacheSolve

## makeCacheMatrix stores the data and functions used to store the results of the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## new input is important if we want x to change without rerunning makeCacheMatrix
  ## an example for its use would be to run cachedatastore$new.input(newmatrix), so that the output of cacheSolve for this modified cachedatastore
  ## would be the inverse of newmatrix rather than somematrix
  new.input <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseResult) inverse <<- inverseResult
  getinverse <- function() inverse
  return(list(new.input = new.input, get = get, setinverse = setinverse, getinverse = getinverse))
}


## cacheSolve calls the functions defined in makeCacheMatrix to return the inverse of the matrix x

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached matrix inverse")
    return(inverse)
  }
  input <- x$get()
  inverse <- solve(input)
  x$setinverse(inverse)
  inverse
}
