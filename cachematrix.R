makeCacheMatrix <- function(x = matrix()) {
                    #return a list that will set the matrix, get the matrix,
                    #set the inverse, and get the inverse 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

                            
cacheSolve <- function(x, ...) { #returns inverse of matrix in makeCacheMatrix
  m <- x$getinverse()
  if(!is.null(m)) {           # if the inverse has already been calculated
    message("retrieve data") 
    return(m)                 #then retrieve data from cache 
  }
  data <- x$get()             #if not then calculate inverse
  m <- solve(data, ...)
  x$setinverse(m)             #Sets inverse value in cache
  m
}


#Code check
 
ssg <- makeCacheMatrix()
ssg$set(matrix(1:4, 2, 2))
ssg$get()
ssg$setinverse()
ssg$getinverse()
