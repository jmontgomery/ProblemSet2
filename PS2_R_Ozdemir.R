rm(list = ls()) #clear the workspace
results<- sample(1:100, 10, replace=TRUE)
m.fnc<- function(results){
  i<- as.numeric(substr(results, start=1, stop=1))
  for(n in 1:9){
  X<- (sum(results[i==n]))/(sum(results)-log(1+1/i))
  m<- sqrt(length(results))*max(X)
  }
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
}


rm(list = ls()) #clear the workspace
results<- sample(1:100, 10, replace=TRUE)
d.fnc<- function(results){
  i<- as.numeric(substr(results, start=1, stop=1))
  for(n in 1:9){
    Y<- sum((sum(results[i==n]))/(sum(results)-log(1+1/i))^2)
    d<- sqrt(length(results))*sqrt(Y)
    }
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
}
