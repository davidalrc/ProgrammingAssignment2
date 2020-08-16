## The following script is an application of lexical scoping in R
## in this case the end of the code is to create an initial function that contain
## another functions that set the value of a matrix, get the value of the matrix, 
##set the value of the inverse matrix,  and get the value of the inverse matrix.
## the  last function calculates the inverse of the special matrix  created with 
## the first function.  

## makeCacheMatrix -> create a special matrix, which is really contains a list containing a 
## function to:  1. set the value of the matrix  2. get the value of the matrix 
## 3. set the value of inverse matrix   4. get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  invertible=NULL
  set<- function(y){
    x<<- y
    invertible<-NULL
  }
  get<- function() x 
  setinverse<- function(inverse){inverse<<-invertible} 
  getinverse<- function(){invertible}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## CacheSolve -> Return a matrix that is the inverse of 'x',  to get the inverse
## this function use arguments from makeCacheMatrix 

cacheSolve <- function(x, ...) {
  invertible<- x$getinverse()
  if(!is.null(invertible)){
    message("getting cached data")
    return(invertible)
  }
  mat<-x$get()
  invertible<- solve(mat,...)
  x$setinverse(invertible)
  invertible
        
}
