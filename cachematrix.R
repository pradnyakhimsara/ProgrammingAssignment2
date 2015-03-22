# Matrix inversion is usually a costly computation and there may be some benefit to caching
# the inverse of a matrix rather than compute it repeatedly . This program contains a pair 
# of function that cache the inverse of matrix.

## The following function creates a special matrix object that has the ability to cache its
## mean 
## It contains a output list of functions : 
#1. set - allows for setting value of the matrix
#2. get - gets the set value for the matrix
#3. setinv - allows setting the value of the inverse of the matrix
#4. getinv - allows getting the value of the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
	 ## Initialize inverse 
    inv <- NULL
    
    ## set and get function for matrix 
    set<-function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    ## set and get function for inverse 
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function()inv
    
    ## Function output
    list(set = set, get = get, setinv = setinv,
        getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache. The output of this function is a matrix

cacheSolve <- function(x, ...) {
    ## Get the inverse of the special object
    inv <- x$getinv()
   
    if(!is.null(inv)){
		## If the inverse has already been calculated --
       message( "Getting cached data" )
       ## output of the function is a pre-computed inverse matrix
	   return(inv)       
    } 
    ## If inverse has not been computed before -- 
    matrix <- x$get()
    inv<- solve(matrix)
    ## Caching the computed value for retrieval the next time around (i.e if 
	## the value of the matrix does not change )
	x$setinv(inv)
    ## The output inverse matrix returned by the function
	inv
}

## Test Run 
##  
# > matrix1 = matrix(c(2,3,4,5), c(2,2))
# > solve(matrix1)
     # [,1] [,2]
# [1,] -2.5    2
# [2,]  1.5   -1

# > matrix1Object <- makeCacheMatrix(matrix1)
# > cacheSolve(matrix1Object)
     # [,1] [,2]
# [1,] -2.5    2
# [2,]  1.5   -1

# > cacheSolve(matrix1Object)
# Getting cached data
     # [,1] [,2]
# [1,] -2.5    2
# [2,]  1.5   -1



