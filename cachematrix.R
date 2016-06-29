## These functions cache the inverse of a matrix
## We assume the matrix is always invertable

## 1st function: creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invt<-NULL
    set<-function(y){
        x<<-y
        invt<<-NULL
    }
    get=function()x
    setinverse=function(inverse)invt<<-inverse
    getinverse=function()invt
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##2nd function: computes the inverse of the "matrix" returned by "makeCacheMatrix()". 
##If the inverse has already been calculated and the matrix has not changed, 
##it'll retrieve the inverse from the cache directly.

cacheSolve <- function(x, ...) {
    cacheSolve<-function(x, ...){
        invt<-x$getinverse()
        if (!is.null(invt)) {
            message("getting cached data")
            return(invt)
        }
        dat<-x$get
        invt<-solve(dat)
        x$setinverse(invt)
        invt
    }
    
}
