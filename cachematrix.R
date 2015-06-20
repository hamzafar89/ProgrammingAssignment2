## Put comments here that give an overall description of what your
## functions do
##this is for testRepo
#
# The makeCacheMatrix function has two attributes i.e. it can be used to store 
# and return back the matrix and inverse of the matrix. whereas the second 
# funciton takes the list of functions reterived from the makeCacheMatrix and 
# then checks the cache inverse of matrix in the makeCacheMatrix, if inverse is
# already been computed then it returns the computed inverse else, it will get 
# the matrix from the 1st funciton and calculates the inverse, stores it in the 
# cache and return the inverse value
#


## Write a short comment describing this function
# makeCacheMatix function has four functions that are used to store the values 
# and give back the sotre values when desire, the funciton takes matrix and 
# stores matrix and inverse of matrix as null values in 'x' & 'inMatrix' variables 
# respectively in the 'setMatrix' function.whereas matrix inverse is stored in
# 'inMatrix'variable in 'setVariable'. the both variable values can be accessed
# by 'getMatrix & getInverse' functions.
# the function returns the list of four function for manipulating the values 
# stored in

makeCacheMatrix <- function(x = matrix()) {
       inMatrix <- NULL
       setMatrix <- function(y) {
              x <<- y
              inMatrix <<- NULL
       }
       getMatrix <- function() x
       setInverse <- function(iN) inMatrix <<- iN
       getInverse <- function() inMatrix
       
       list(setMatrix = setMatrix, getMatrix = getMatrix,
            setInverse = setInverse,
            getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve function is responsible for calculting the inverse of matrix and 
# checking if the value is exist in cache or not. if the value is not cached 
# then this function reterive the matrix from the above function and then 
# calcuates the inverse and then stores it to cache. whereas if value exits then
# function return the cache inverse value.

cacheSolve <- function(x, ...) {
       iN <- x$getInverse()
       if(!is.null(iN)) {
              message("getting cached data")
              return(iN)
       }
       data <- x$getMatrix()
       #m <- mean(data, ...)
       iN <- solve(data, ...)
       x$setInverse(iN)
       iN
       ## Return a matrix that is the inverse of 'x'
}


##Below are the invertible examples to whom the funciton is verified.
#http://www.mathwords.com/i/inverse_of_a_matrix.htm#
a2<-matrix(c(4,3,3,2), nrow = 2, ncol = 2)
a3<-matrix(c(7,0,-3,2,3,4,1,-1,-2), nrow = 3, ncol = 3)
a4<-matrix(c(1,1,1,1,1,2,1,4,1,1,1,2,1,2,0,3), nrow = 4, ncol = 4)
