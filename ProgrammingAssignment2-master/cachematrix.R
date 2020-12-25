## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  im<-NULL
  set<-function(y){
    x<<-y
    im<<-NULL
  }
  get<-function() x
  setmat<-function(matx) im<<-matx
  getmat<-function() im
  list(set=set,get=get,setmat=setmat,getmat=getmat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  f<-x$getmat()
  if(!is.null(f)){
    message("getting inverse catched matrix")
    return(f)
  }
  data<-x$get()
  g<-solve(data, ...)
  x$setmat(g)
  g
}