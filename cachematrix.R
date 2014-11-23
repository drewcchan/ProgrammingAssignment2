## Put comments here that give an overall description of what your
## functions do

##Creates a list of functions to save a matrix and inverse matrix and allow for retrieval 
##by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL                                         
        set<-function(y,...){               
                x<<-y
                s<<-NULL
        }
        get<-function(){x}                              
        setsolve<-function(solve){s<<-solve}           
        getsolve<-function(){s}                         
        list(set=set, get=get,setsolve=setsolve, getsolve=getsolve)
}


##Creates a function to retrieve a saved inverse matrix from makeCacheMatrix 
##or create one if there is none saved

cacheSolve <- function(x, ...) {
        a<-x$getsolve()                         
        if(!is.null(a)){                        
                message("getting cache data")   
                return(a)                       
        }                                     
        data<-x$get()                           
        a<-solve(data,...)                      
        x$setsolve(a)                           
        a 
}
