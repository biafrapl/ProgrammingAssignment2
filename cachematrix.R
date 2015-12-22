## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){ ##Set the value of the matrix
        x<<-y
        m<<-NULL
    }
    get<-function() x  ##Get the value of the matrix
    setmatrix<-function(solve) m<<- solve ##Set the value of the inverse
    getmatrix<-function() m  ##Get the value of the inverse
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix() ##checking if the inverse is already calculated
    if(!is.null(m)){ 
        message("getting cached data")
        return(m)
    }
    matrix<-x$get() #if the invered matrix was not calculated, it is being calculated now and set in the cache
    m<-solve(matrix, ...)
    x$setmatrix(m) 
    m
}