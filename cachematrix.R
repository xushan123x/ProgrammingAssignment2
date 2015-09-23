## makeCeMatrix will return a list including four functions which can:
#  1 set the value of the matrix
#  2 get the value of the matrix
#  3 set the value of the inversion
#  4 get the value of the inversion

## The following function ceSolve calculates the inversion of the special "matrix" created 
#  with the above function. However, it first checks to see if the inversion has already been 
#  calculated. If so,it gets the inversion from cache and skips the computation. Otherwise,
#  it calculates the inversion of the data and sets the value of the inversion in the cache  
#  via the setinverse function.

##########################################################################

makeCeMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y){
    x<<-y
    n<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) n<<-inverse
  getinverse<-function() n
  list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)

}

## Function ceSolve calculates the inversion of the special "matrix" created 
#  with the above function. However, it first checks to see if the inversion has already been 
#  calculated. If so,it gets the inversion from cache and skips the computation. Otherwise,
#  it calculates the inversion of the data and sets the value of the inversion in the cache  
#  via the setinverse function.


ceSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mn<-x$getinverse()
  if(!is.null(n)){
    message("getting ced data")
    return (n)
  }
  data<-x$get()
  n<-solve(data,...)
  x$setinverse(n)
  n
}


###################################################################

