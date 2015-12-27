## The makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        IMatrix<-NULL 
                #The container where the inverted matrix will be stored
        set<-function(y){ 
                #Sets the original matrix
                x<<-y
                IMatrix<<-NULL 
                        #Reset IMatrix as new input matrix is used now
        }
        get<-function()x 
                #Gets original Matrix
        setinverse<-function(inverse)IMatrix<<-inverse
                #Sets the inversed matrix
        getinverse<-function()IMatrix
                #Gets the inversed matrix
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
                #Return the list that exposes the methods available
}

## cacheSolve is a function that calls the functions stored in the list returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        InversedM<-x$getinversed()
                #Gets the inverse
        If(!is.null(inversedM){
                #If inversed matrix is available, return it
        message("getting cached data")
        return(InversedM)
        }
        message("calculate and cache data")
        #If inversed matrix is not available, solve it
        OriginalM<-x$get()
        InversedM<-solve(OriginalM)
        x$setinversed(InversedM)
        InversedM
}

