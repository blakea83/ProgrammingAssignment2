## These functions check to see if an inverse of matrix 
## has been calculated and returns the previous calculated
## inverse or calculates an inverse.

# This function takes an matrix and returns a list

makeCacheMatrix <- function(x = matrix()) {
    
    # This primes mean variable and sets it to zero
    m <- NULL
    
    # Creates the set function    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Function to get the original matrix  
    get <- function() x
    
    # function to get the inverse
    setinverse <- function(solve) m <<- solve
    
    # Function to store the inverse 
    getinverse <- function() m
    
    # Creates a list to be fed to cacheSolve
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
    
}


#This functions check to see if an inverse 
#of a matrix has been calculated.
#The function returns the stored value or calculates
#an inverse if the inverse has not been calculated.
#The inverse matrix is returned.

cacheSolve <- function(x, ...) {
    
    # Check to see if an inverse has been calculated
    # Returns the inverse if it has been calculated.
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Takes the matrix from the list 
    data <- x$get()
    # Determines the matrix inverse
    m <- solve(data, ...)
    # Takes the inverse and puts it into the list 
    x$setinverse(m)
    #Returns the inverse
    m
    
}
