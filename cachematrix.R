## makeCacheMatrix creates special vector of 4 functions - set the value of matrix, get the value of matrix, use stored inverse matrix, calculate inverse and store


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # cash to store inverse matrix
  set <- function(y) {  #stores base matrix and clears inverse matrix (when changed matrix, then inverse should be recalculated)
    x <<- y
    m <<- NULL
  }
  get <- function() x # get matrix data for calculation inverse
  setinverse <- function(inverse) m <<- inverse #store inverse matrix
  getinverse <- function() m #get inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve return stored inverse matrix in case it was calculated previosly. 
## In other case it calculates inverse, stores it and returns

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

x <- makeCacheMatrix(matrix(c(2,0,0,2),2,2)) #diagonal matrix with 2'a on diagonal.
cacheSolve(x) #full calculation
cacheSolve(x) #get cached data

