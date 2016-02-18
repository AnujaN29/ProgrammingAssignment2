## The pair of functions below cache the inverse of a matrix.
## Computation of matrix inversion being a costly process, caching
## the results is benefecial.

## makeCacheMatrix function takes a matrix as an input argument.
## It returns a list of functions to :
## 1. Set the value of the input  matrix to an object which is cached across
##    environments.
## 2. Get the value of the input matrix.
## 3. Set the inverse of the input matrix.The inversion is cached.
## 4. Get the inverse of the matrix.
## Usage :
# > x<-matrix(c(1,1,1,3,4,3,3,3,4),3,3)
# > y<-makeCacheMatrix(x)
# > y$get()
# [,1] [,2] [,3]
# [1,]    1    3    3
# [2,]    1    4    3
# [3,]    1    3    4
# > y$getinverse()
# NULL
# y$setinverse(matrix(c(7,-1,-1,-3,1,0,-3,0,1),3,3)) ... Ideally this should not be done.
# y$getinverse()
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1


makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function looks if the cached inverse of the matrix 
## is present in the getinverse function defined in makeCacheMatrix.
## If not , it calculates the inversion and sets the setinverse fuction
## and returns the matrix inversion result.
## Usage : cacheSolve(y)
## If cacheSolve(y) is executed again, it will now retrieve the cached
## output and display the message "getting cached data"
## y$getinverse() will return the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}

