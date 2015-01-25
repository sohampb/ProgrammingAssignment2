## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x=matrix()) {
        varM <- NULL #this variable will later hold the cached matrix
        getMatrix <- function() x #this function obtains the original matrix whose inverse is to be found
        setInverseMatrix <- function(invMatrix) varM <<- invMatrix #function to store inverse matrix in varM
        getInverseMatrix <- function() varM #function to retrieve varM - the inversed matrix
        
        # return the list of functions as an R object
        list(getMatrix=getMatrix, setIinvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
}


## Write a short comment describing this function 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        varM <- x$getInvMatrix()
        if(!is.null(varM)){
                message("Found the cached result. Returning the same matrix.")
                return(varM)
        }
        else {
                message("There was no cache data. Returning inverse matrix")
                matrixData <- x$getMatrix() # obtains matrix from arguement x
                varM <- solve(matrixData) # calculate the inverse martix 
                x$setInvMatrix(varM) # assigns resulting inverse matrix back to object x
                return(varM)
        }
}
