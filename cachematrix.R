## idea behind this task thab enable you to cache a matrix and its inverse to get them any time without needing to calculate inverse again
## makeCacheMatrix that set matrix and its inverse and cacheSolve that calculate inverse and send it to makeCacheMatrix 


##makeCacheMatrix function that set a matrix and set its inverse and get them back 
makeCacheMatrix <- function(x = matrix()) {                
inv <- NULL
  ##set matrix x 
  set <- function(y){               
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



##check first if matrix X was caculated it inverse before , if invesre is null , calculate it by solve() and set it  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmean(inv)
  inv
}
