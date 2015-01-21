## this fuction will help to cache the inverse of a Matrix, since computing of the inverse is time-consuming


## makeCacheMatrix will help you to Cache the inverse of a matix. 

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setinver<-function(inver)m<<-inver
    getinver<-function()m
    list(set=set,get=get,setinver=setinver,getinver=getinver)
    
}


## cacheSolve will check if the value is stored, or it will compute the value and store it in makeCacheMatrix

cacheSolve <- function(x, ...) {
    m<-x$getinver()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinver(m)
    m
}
