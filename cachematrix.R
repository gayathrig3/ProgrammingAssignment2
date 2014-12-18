## Put comments here that give an overall description of what your
## functions do

##  This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

inv<-matrix(nrow=nrow(x),ncol=ncol(x)) #creating a matrix object that will hold the inverse
get <- function(){x} # this function is used to get the matrix
setInv <-function(inverse){inv<<-inverse} # setting the inverse
getInv <- function(){inv} #This function returns the vale of set inverse or NULL
list(get = get,          
         setInv = setInv, 
         getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
 inv2 <- x$getInv() ##Getting the value of inverse that may have been set
if(is.null(inv2)) { ##If the inverse value has not been set and is null proceed
matrix <- x$get() ##getting the value of original matrix
inv2<-solve(matrix) ##the solve function computes the inverse of the matrix
x$setInv(inv2)##The computed inverse is set to the inv variable in above function
}
return inv2 ##returns the inverse.Computation directly comes here if "if" condition fails
}
