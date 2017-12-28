## Theses functions try to avoid time wasting
## by computing the inverse of a matrix repeatedly,
## the functions cache the value of the matrix and its inverse
## in a list using the function makeCacheMatrix, then 
## the function cacheSolve can be used to get
## the inverse of the matrix. 
## If the data is already available,
## this value will only be retrieved 
## The variable i is the inverse of the matrix


## This funcition creates an special list of functions 
## for a given matrix, that allows the user to keep the 
## following information:
#1. the setter of the matrix, 
#2. The getter of the matrix,
#3. The setter of the inverse of the matrix
#4. The getter of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        
        i<-NULL
        
        set<-function(k){
                x<<-k
                i<<-NULL
        } #Set the value of the matrix to be k and set its inverse to be null
        
        get<-function() x
        #Get the value of the matrix
        
        setinv<-function(inv) i <<- inv
        #Set the inverse of the matrix
        
        getinv<-function() i
        #Returns the value of the inverse of the matrix
        
        list(set=set,get=get,
             setinv=setinv, getinv = getinv)
        
        
        
}


## cacheSolve gets the inverse of an object, 
## which has already been cached using the makeCacheMatrix function, 
## if the inverse of the matrix had already been calculated, 
## then the value is retrieved.

cacheSolve <- function(x, ...) {
        
        i<-x$getinv()
        
        if(!is.null(i)){
                message("---getting cached data---")
                return(i)
        }
        
        data<-x$get()
        
        i<-solve(data)
        
        x$setinv(i)
        
        i
        # Return a matrix that is the inverse of 'x'
}
