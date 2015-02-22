makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
    ## Method the get the matrix
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}
## Compute the inverse of the unique matrix
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
