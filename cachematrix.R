## Coursera R Programming - Allen Seol - Programming Assignment 2


## This Script creates a list of functions for a matrix that lets you store a matrix and cache it's inverse
## setMatrix() - Sets the Value of the Matrix in the cache
## getMatrix() - Gets the Value of the Matrix in the cache
## setInvMat() - Sets the Inverse Matrix in the cache
## getInvMat() - Gets the Inverse Matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
  trix <- NULL ##trix is the cached matrix.  Initially set as NA with 1x1
  
  ##Sets argument y matrix to parent environment's x
  setmat <- function(y) { 
    x <<- y
    trix <<- NULL
  }
  ##Returns the previously set X matrix
  getmat <- function() x
  
  ##Caches the Inverse Arguement into the Trix
  setinv <- function(inv) trix <<- inv
    
  ##Returns the Cached Inverse matrix
  getinv <- function() trix

  ## Returning a list of the four functions so that it can be subsetted
 list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ##Let's retrieve the possible cached inv matrix
  InvMatrix <- x$getinv() 
  
  ##If there is already an inverse Matrix in the list, it will return it
  if(!is.null(InvMatrix)) {
      message("Getting Cached Data!!!")
      return(InvMatrix)
  }
  ##If there isn't a cached inv matrix
  
  temp <- x$getmax() ## gets original cached non-inv matrix
  InvMatrix <- solve(temp,...)  ##inv original matrix 
  
  x$setinv(InvMatrix)  ##Caches newly made inverse matrix
  
  InvMatrix ##returns the newly made cached matrix  
    
}



