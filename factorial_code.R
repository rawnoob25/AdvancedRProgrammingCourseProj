##block below installs relevant packages
##and adds them to search list
{
  if(!require(purrr)){
    install.packages("purrr")
    library(purrr)
  }
  if(!require(magrittr)){
    install.packages("magrittr")
    library(magrittr)
  }
  if(!require(microbenchmark)){
    install.packages("microbenchmark")
    library(microbenchmark)
  }
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
}

#loop function
Factorial_loop<-function(n){
  stopifnot(n>=0)
  if (n==0)return(1)
  
  fact<-1
  for(i in 1:n){
    fact<-fact*i
  }
  return (fact)
}

#function that uses purrr's "reduce" function
#Props to Faizan Uddin Fahad Khan 
#for the suggestion to use as.numeric here
#to avoid integer overflow issues
Factorial_reduce<-function(n){
  stopifnot(n>=0)
  if (n==0)return(1)  
  
  answer<-reduce(1:n,function(x,y){
    as.numeric(x*y) #Wrapping x*y inside
    ##a call to as.numeric prevents the answer
    ##from being NA, for n values that result
    ##in large n! values (for e.g. n=40)
    
  })
  return (answer)
}

#normal recursive function
Factorial_func<-function(n){
  stopifnot(n>=0)
  if (n==0|n==1)return(1)  #handles both base cases in one line
  return(n*Factorial_func(n-1))
}

#This memoized function is just creates the memo table (Factorial_tbl),
#handles the base cases (n=0 and n=1),
#and calls the recursive helper function (Fact(n)) that does the rest of the work
#(including writing to the memo)
Factorial_mem<-function(n){
  stopifnot(n>=0)
  if(n<2)return (1) 
  Factorial_tbl<<-c(1,rep(NA,n-1))  #Factorial_tbl stores n! (for n>=1)
  out<-Fact(n) #call to recursive helper
  return (out)
}

#recursive helper function
Fact<-function(n){
  val<-Factorial_tbl[n]
  if(!is.na(val)){
    return (val)
  }
  if(n>2){ #this if-test is here to avoid recomputing 1!- which is already 
    #stored in Factorial_tbl[1]
    Factorial_tbl[n-1]<<-Fact(n-1) #store result of recursive call in memo  
  }
  return(n*Factorial_tbl[n-1])
}


#function for benchmarking
benchMarking<-function(n){
  microbenchmark(Factorial_loop(n),Factorial_reduce(n),
                 Factorial_func(n),Factorial_mem(n))
}

#benchmarking(n) for 
#n in {5,10,50,100,150}
bench<-function(){
  if(!dir.exists("output"))dir.create("output")
  op<-"output"
  
  vals<-c(5,10,50,100,150)
  for(i in vals){
    cat("n:",i,"\n")
    benchMarking(i)%>%print
    cat("-------------------------","\n")
  }
}
