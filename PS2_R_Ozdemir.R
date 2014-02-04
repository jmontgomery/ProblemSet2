rm(list = ls()) #clear the workspace
##Question 1
results<- sample(1:100, 10, replace=TRUE) #generate random election results 
my.fnc<- function(results,stat){ #function to derive the statistics
  i<- as.numeric(substr(results, start=1, stop=1)) #takes the first integers and stores
  if(stat="m"){        #if stat option is specified as "m", returns m statistics
    for(n in 1:9){     #applies the formula for m statistics for every integer stored
      X<- (sum(results[i==n]))/(sum(results)-log(1+1/i))
      m<- sqrt(length(results))*max(X)
      return(m)
    }
  }  
  else if(stat="d"){    #if stat option is specified as "d", returns d statistics
    for(n in 1:9){      #applies the formula for d statistics for every integer stored
      Y<- sum((sum(results[i==n]))/(sum(results)-log(1+1/i))^2)
      d<- sqrt(length(results))*sqrt(Y)
      return(d)
    }
  }
  else{     #if stat option is not specified as "m" or "d", it returns both statistics
    for(n in 1:9){  #derive m statistics, same as above
      X<- (sum(results[i==n]))/(sum(results)-log(1+1/i))
      m<- sqrt(length(results))*max(X)  
  }
    for(n in 1:9){  #derive d statistics, same as above
      Y<- sum((sum(results[i==n]))/(sum(results)-log(1+1/i))^2)
      d<- sqrt(length(results))*sqrt(Y)
    }
    output<-list(m,d)  #return two values in a function
    return(output)
}
  
##Question 2
#Print fraud information
if(m>=0.851 & m<0.967){
  print("Fraud at 0.10 significance level")
}
else if(m>=0.967 & m<1.212){
  print("Fraud at 0.05 significance level")
}
else if(m>=1.212){
  print("Fraud at 0.01 significance level")
}
else{
  print("No fraud")
}

#Print fraud information
if(d>=1.212 & d<1.330){
  print("Fraud at 0.10 significance level")
}
else if(d>=1.330 & d<1.569){
  print("Fraud at 0.05 significance level")
}
else if(d>=1.569){
  print("Fraud at 0.01 significance level")
}
else{
  print("No fraud")
}