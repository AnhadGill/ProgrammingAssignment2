## 2 functions are being created in this assignment:-
#         1. makeCacheMatrix to set,get,set the inverse of or get the inverse of
#            a matrix
#         2. cacheSolve function to retrieve the value of the inverse of a 
#            matrix preferably from cache.


# 1. makeCacheMatrix function which takes a matrix as input parameter and contains
# the following functions to be executed on the object:-
#         a)Set the value of the matrix object
#         b)Get the value of the matrix object
#         c)Set the value of theinverse of the object
#         d)get the value of the inverse of the object

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        #sets value of matrix
        set<-function(y){
                x<<-y
        inv<<- NULL}
        #returns value of matrix
        get<- function() {x} 
        #set value of inverse
        setinverse <- function(inverse) {inv<<-inverse}
        #get value of inverse
        getinverse<-function(){return(inv)}
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function: To solve a matrix for its inverse.If it can find a value
# in the cache, it retrieves the value from there, otherwise it calculates the 
# inverse of a matrix and stores it in cache. is executed as follows:-
#         a) Look for inverse matrix in cache
#         b) If value retrieved from cache, then print "Getting Cached Data" and
#            the inverse
#         c)Otherwise, find inverse using Solve() function and write to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        #Look for inverse in cache
        if(!is.null(inv)){
                #Retrieve and print from Cache
                message("Getting cached data")
                return(inv)
                }
                {
                #Calculate inverse and store in cash
                data <- x$get()
                inv <- solve(data)
                x$setinverse(inv)
                inv
        }
}

# Sample output.
# Using inversable matrix suggested by Steve Sparling in discussion forums

#First<-makeCacheMatrix(B)

#First$get()
#[,1] [,2] [,3]
#[1,]    1    2    6
#[2,]    2    5   -3
#[3,]    3    2    1

# > cacheSolve(First)
# [,1]        [,2]        [,3]
# [1,] -0.1428571 -0.12987013  0.46753247
# [2,]  0.1428571  0.22077922 -0.19480519
# [3,]  0.1428571 -0.05194805 -0.01298701

# > cacheSolve(First)
# Getting cached data
# [,1]        [,2]        [,3]
# [1,] -0.1428571 -0.12987013  0.46753247
# [2,]  0.1428571  0.22077922 -0.19480519
# [3,]  0.1428571 -0.05194805 -0.01298701
