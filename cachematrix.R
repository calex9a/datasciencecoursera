## Set and get the matrix x
## Set and get the inverse of matrix x

MakeMatrix<-function(x=matrix()) {
  invmatrix<-NULL
  setMatrix<-function(y) {
    x<<-y
    invmatrix<<-NULL
  }
  getMatrix<-function() x
  setInverse<-function(inverse) invmatrix <<- inverse
  getInverse<-function() invmatrix
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
}


## Solves for Inverse Matrix
## Unless already Cached

MatrixSolve<-function(x,...) {
  invmatrix<-x$getInverse()
  if(!is.null(invmatrix)) {
    message("Getting Cached Matrix")
    return(invmatrix)
  }
  z<-x$getMatrix()
  invmatrix<-solve(z,...)
  x$setInverse(invmatrix)
  return(invmatrix)
}
