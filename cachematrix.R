## Functions for creating a matrix with the ability to cache its inverse and
## the complementary function to retrieve/set the inverse of this matrix.

## Creates an instance of a matrix with the ability to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  # Change the matrix.
  set<-function(y) {
    x<<-y
    inv<<-NULL # Reset the inverse if the matrix is changed.
  }
  # Return the matrix.
  get<-function() x
  # Set the inverse of the matrix.
  setinv<-function(inverse) inv<<-inverse
  # Get the inverse of the matrix.
  getinv<-function() inv
  # Returns the class defined above with functions for getting/setting the matrix and its inverse.
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Function returning the inverse of matrix x, returns the cached inverse if such exists.

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  # Return cached data if such exists.
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  # Calculate inverse (if no cached version exists), cache and return it.
  data<-x$get()
  inv<-solve(x,...)
  x$setinv(inv)
  inv
}
