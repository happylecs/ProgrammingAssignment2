## Note: This code has dependencies on MASS package. 
##  Ensure you have this package or install using
##  install.packages("MASS")

## makeCacheMatrix is a method to define
## a matrix for which Inverse has to be cached

makeCacheMatrix <- function(x = matrix()) {
#This Function contains four functions: get,set to 
# call & initialize a matrix respectivBely
# getInv & setInv to the inverse and compute
# inverse the matrix defined via first two functions
  xInv<<-NULL
  set <- function(y){
    x<<-y
    xInv<<-NULL
  }
  get <-function() x
  setInv<- function(inv) xInv<<-inv
  getInv<- function() xInv
  
  list(set =set,get=get,setInv=setInv,getInv=getInv)
}


## cacheSolve method is a function to get matrix inverse
## of the makeCacheMatrix. If a new matrix is used this 
## function returns the inverse as well as caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv<- x$getInv()
  if(!is.null(xInv)){
    message("getting cached data")
    return(xInv)
  }
  
  mat<-x$get()
  #Load MASS package to compute matrix inverse
  library(MASS)
  inv<-ginv(mat)
  x$setInv(inv)
  inv
}
