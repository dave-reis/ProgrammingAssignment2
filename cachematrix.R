##Assignment 2 Part 1
##This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
  ##Set Matrix
  set<-function(y){
    inv<<- Null
    x<<-y
    }
  ##Get Matrix
  get <- function() x
  setinv <- function(inverse) inv <<-inverse
  getinv <-function()inv
  list(set=set, get=get, setinv = setinv, getinv = getinv)
}


##Assignment 2 Part 2
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cachesolve
##should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv = x$getinv()
  ##has inverse already been calculated?
  if (!!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ##If not then go ahead and calculate inverse
  mat = x$get()
  inv = solve(mat, ...)
  x$setinv(inv)
  return(inv)
}
