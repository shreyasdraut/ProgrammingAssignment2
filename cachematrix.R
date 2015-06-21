## Put comments here that give an overall description of what your
## functions do

## the following function "makeCacheMatrix"  makes a list of function like
# 1. set the matrix 'x'
# 2. get the value of the matrix 'x'
# 3. set the inverse matrix 'invMatrix'
# 4. get the value of the inverse matrix 'invMatrix'

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


## this function take an matrix as input
# then it assigns the value of the inverse of the matrix  if any
# if there is non it fetches the cached value
# if there is no value previously set the it goes on to set the inverse of the matrix using the solve fuction

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix<-x$getInverse()
  if(!is.null(invMatrix)){
    #check if there is a value previously set if yes fetch it else set a new value
    message("geting cached data.")
    return(invMatrix)
  }
  mat<- x$get()
  invMatrix<-solve(mat)
  x$setInverse(invMatrix)
  invMatrix
}
