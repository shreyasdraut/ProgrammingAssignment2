## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
invMatrix <- NULL
set<- function(val){
  x<<-val
  invMatrix<<-NULL
}
get<- function() x
setInverse<-function(inverseVal) invMatrix<<-inverseVal
getInverse<- function() invMatrix
list(set = set, get = get, setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix<-x$getInverse()
  if(!is.null(invMatrix)){
    message("geting cached data.")
    return(invMatrix)
  }
  mat<- x$get()
  invMatrix<-solve(mat)
  x$setInverse(invMatrix)
  invMatrix
}
