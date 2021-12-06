#This function creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {     
                   
  m <- NULL
  set <- function(y) {                            #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                           #get the value of the matrix
  setinverse <- function() m <<- solve(x)      #set the value of the inverse
  getinverse <- function() m                   #get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix                            
cacheSolve <- function(x, ...) { 
  m <- x$getinverse()
  if(!is.null(m)) {            #it first checks to see if the inverse has already been calculated.
    message("retrieve data") 
    return(m)                 #if the inverse has already been calculated, it gets the inverse from the cache and skips the computation 
  }
  data <- x$get()             
  m <- solve(data, ...)    #if the inverse hasnt been calculated, 
  x$setinverse(m)          #it calculates the inverse of the data and sets the value of the inverse in the cache via the getinverse function             
  m
}


#Code check
 
ssg <- makeCacheMatrix()
ssg$set(matrix(1:4, 2, 2))
ssg$get()
ssg$setinverse()
ssg$getinverse()
