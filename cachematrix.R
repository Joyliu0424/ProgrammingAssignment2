##These functions are used to cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  setMatrix<-function(y){
    x<<- y
    m<<-NULL
  }
  getMatrix<-function() x
  setInverseMatrix<- function(solve) m<<- solve
  getInverseMatrix<- function() m
  list(setMatrix=setMatrix, getMatrix=getMatrix, 
       setInverseMatrix=setInverseMatrix,
       getInverseMatrix=getInverseMatrix)
  
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<- x$getInverseMatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m<- solve(data)
  x$setInverseMatrix(m)
  m
}
