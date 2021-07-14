## Making cache matrix and solving inverse of the cache matrix
## making cache matrix, with inverting the variables

makeCacheMatrix <- function(x = matrix()) {
 z<- NULL
 set<- function(y){
   x<<-y
   z<<-NULL
 }
 get <- function()x
   setInverse<- function(inverse) z<<- inverse
   getInverse <- function() z
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## getting the inverse of the cache matrix

cacheSolve <- function(x, ...) {
  z<- x$getInverse()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  mat<- x$get()
  z<- solve(mat,...)
  x$setInverse(z)
  z
  }
