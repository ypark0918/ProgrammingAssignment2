## Put comments here that give an overall description of what your
## functions do

##A function of "makeCacheMatrix" caches the inverse of the matrix.
##A function of "cacheSolve" computes the inverse of the matrix returned by
##"makeCacheMatrix".

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x} ##Function to get matrix x
  setInv <- function(inverse) {
    inv <<- inverse
  }
  getInv <- function() { ##Function to get inverse of matrix
    inver <- ginv(x)
    inver%*%x
  }
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x,...) { ##Get the cache data
  inv <- x$getInv()
  if(!is.null(inv)){ ##Logical check to see if the inverse is null
    message("getting cached data")
    return(inv) ##Return inverse value
  }
  mat <- x$get() 
  inv <- solve(mat, ...) ##Calculate inverse value
  x$setInv(inv)
  inv ##Return the matrix's inverse 
}

