## For this assignment I took the "Cachin the Mean of a Vector"
## example and transformed it for new purposes. The idea is
## that when this functions are used, they will make the
## solve() function easier for the program, since it will
## be able to use the cached memory of the matrix

## this functions will:
## 1. Set the value of the matrix
## 2. get the value of the matrix


makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y){
                x <<- y
                z <<- NULL
        }
        get <- function () x
        setinv <- function(inverted) z <<- inverted
        getinv <- function() z
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


## 3. inverse de matrix
## 4. get the inverse matrix

cacheSolve <- function(x, ...) {
        z <- x$getinv()
        if(!is.null(z)){
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setinv(z)
        z
}
                
m1 <- matrix(c(1,2,3,4),2,2)
m2 <- makeCacheMatrix(m1)
cacheSolve(m2)
