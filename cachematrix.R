#######
#Sample input
#m <- makeCacheMatrix(c(1,3,5,4,2,6,7,4,0))
#cacheSolve(m)

#sample output
#            [,1]    [,2]        [,3]
#[1,] -0.21428571  0.3750  0.01785714
#[2,]  0.17857143 -0.3125  0.15178571
#[3,]  0.07142857  0.1250 -0.08928571

#The above output is inverse off
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    3    2    4
#[3,]    5    6    0
#######


#takes in the list of numbers of a matrix column wise
#if the number of values given as input are not proper square
#then it will pad the rest of the columns with '0' to make a perfect square matrix
makeCacheMatrix <- function(x = numeric()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() {
    if((length(x)^(1/2))%%1==0){
      matrix(x,length(x)^(1/2),length(x)^(1/2))
    }else {
      padding <- (floor(length(x)^(1/2))+1)^2-length(x) 
      matrix(c(x,rep(0,padding)),floor(length(x)^(1/2))+1,floor(length(x)^(1/2))+1)
    }
  }
  setInv <- function(solve) s <<- solve
  getInv <- function() s
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


#Caches the data first time and returns the same value when called
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  print(data)
  m <- solve(data)
  x$setInv(m)
  m
}

