##The functions below creates a special matrix object through makeCacheMatrix
##And return its inverse thru cacheSolve function which uses the solve function

## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize inverse of a matrix to null
  inv <- NULL   
  
  ## set value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x                         ## get the inverse of the matrix
  setinvmat <- function(invmat) inv <<- invmat  ## set inverse of a matrix
  getinvmat <- function() inv                 ## get inverse of the matrix
  
  ##list containg above functions so we can use makeCacheMatrix object as
  ##x <- makeCacheMatrix() 
  ##x$set() --setting new matrix
  ##x$get() --to get the matrix
  ##x$setinvmat() -- to set the inverse 
  ##x$getinvmat() -- to get the inverse
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## cacheSolve computes the inverse of the matrix returned 
## in previous function makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  inv <- x$getinvmat() ## gets the invmat in previous function
  
  ## if inverse matrix is already stored, get cached data and return the inv matrix
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if not, calculate inverse of the matrix 
  data <- x$get()              ## get the matrix
  inv <- solve(data, ...)      ## calculate the inverse
  x$setinvmat(inv)             ## set the inverse of the matrix
  inv                          ## print the inverse of the matrix

}

