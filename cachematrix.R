## This function creates a special "matrix" object that can cache its inverse and computes the inverse of the 
## special matrix returned by makeCacheMatrix. If the inverse has already been calculated, then the cachesolve 
## retrieves the inverse from the cache.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
    }
  
  get <- function(){
    x
    }
  
  set.inverse <- function(matrix.inverse){
    inverse <<- matrix.inverse
    }
  
  get.inverse <- function(){
    inverse
    }
  
  list(
    set = set, get = get, 
    set.inverse = set.inverse, 
    get.inverse = get.inverse
    )

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse 
## has already been calculated then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  
  inverse <- x$get.inverse()
  
  if(!is.null(inverse)){
    print("Inverse is in cache, loading from cache.")
    return(inverse)
    }
  
  matrix <- x$get()
  inverse <- solve(matrix)
  x$set.inverse(inverse)
  
  inverse   
  }
