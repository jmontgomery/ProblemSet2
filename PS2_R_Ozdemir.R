rm(list = ls()) #clear the workspace
##Question 1
results<- sample(1:100, 10, replace=TRUE)#generate random election results 
my.fnc<- function(results,stat){ #function to derive the statistics
  i<- as.numeric(substr(results, start=1, stop=1)) #takes the first integers and stores
  if(stat=="m"){        #if stat option is specified as "m", returns m statistics
    for(n in 1:9){     #applies the formula for m statistics for every integer stored
      X<- (sum(results[i==n]))/(sum(results)-log(1+1/i))
      m<- sqrt(length(results))*max(X)
      return(m)
    }
  }  
  else if(stat=="d"){    #if stat option is specified as "d", returns d statistics
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
      Y<- sum((sum(results[i==n]))/(sum(results)-log(1+1/i))^2)
      d<- sqrt(length(results))*sqrt(Y)
      output<-c(m,d)  
      return(output) #return two values in the function
    }   
}
}  
#try the function and pass the values to the following print function
value.m<- my.fnc(results=results, stat="m")
value.d<- my.fnc(results=results, stat="d")

##Question 2
print.benfords<- function(){
  if(value.m>=0.851 & value.m<0.967){
    asterisk.m<- c("*")
    }else if(value.m>=0.967 & value.m<1.212){
    asterisk.m<- c("**")
    }else if(value.m>=1.212){
    asterisk.m<- c("***")
    }else if(value.m<0.851){
    asterisk.m<- c("")
    }
  if(value.d>=1.212 & value.d<1.330){
    asterisk.d<- c("*")
    }else if(value.d>=1.330 & value.d<1.569){
    asterisk.d<- c("**")
    }else if(value.d>=1.569){
    asterisk.d<- c("***")
    }else if(value.d<1.212){
    asterisk.d<- c("")
  }
  t<- matrix(c("Leemis’ m", round(value.m, digits=3), asterisk.m,"Cho-Gains’ d", round(value.d, digits=3), asterisk.d),nrow=2, ncol=3, byrow=TRUE)
  rownames(t)<- c("1", "2")
  colnames(t)<- c("Statistic", "Value", "Significance")
  return(as.table(t))
}
print.benfords()