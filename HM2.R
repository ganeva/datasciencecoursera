## This is the functions to cache the inversion of a given matrix

## This functions creates a list including:
## set(initiate the matrix and its inversion),
## get(get the matrix),
## setInverse(cache the matrix' inversion) 
## and getInverse(get the matrix' inversion)

makeCacheMatrix <- function(x = matrix()) {
  inversed<-NULL
  set<-function(y){
    x<-y
    inversed<-NULL
  }
  get<-function() x
  setInverse<-function(inv) inversed<<-inv
  getInverse<-function() inversed
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function is to cache the inversion. 
## First, get the inversion cached and check if the inversion is null:
## if it's not, just return the result
## else, get the inversion and cache it

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inversed<-x&getInverse()
  if(!is.null(inversed)){
    return(inversed)
  }
  mat<-x&get()
  inversed<-solve(x)
  x&setInverse(inversed)
  inversed
}