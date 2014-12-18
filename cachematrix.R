## Put comments here that give an overall description of what your
## functions do

##  This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

inv<-matrix(nrow=nrow(x),ncol=ncol(x)) #creating a matrix object that will hold the inverse
get <- function(){x}
setInv <-function(inverse){inv<<-inverse}
getInv <- function(){inv}
list(get = get,          
         setInv = setInv, 
         getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
 inv2 <- x$getInv()
if(is.null(inv2)) { 
matrix <- x$get()
inv2<-solve(matrix)
x$setInv(inv2)
}
return inv2
}
