#' title: 'Caching the Inverse of a Matrix'
#' author: 'Ettore Riva'
#' Date: '26/09/2015'

## this function sets the data structures to storage
## the inverse matrix of x

makeCacheMatrix <- function(x = matrix()) {
  Inv<- NULL
  set<- function(y){
    x<<-y
    Inv<<- NULL
    
  }
  get<-function() x
  setsolve<- function(Solve) {
      Inv<<-Solve
  }
  getsolve<<-function() Inv
  
  list(set=set,
       get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}


## Compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # initialize the Inv variable
  Inv<-x$getsolve()
  
  if(!is.null(Inv)){
   message("getting cached data")
    return(Inv)
  }
  ## compute the inverse and store it in x
  data<- x$get()
  Inv<- solve(data,...)
  x$setsolve(Inv)
  return(Inv)
  
}
