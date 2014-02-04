rm(list = ls()) #clear the workspace
results<- numeric(5)
m.fnc<- function(results){
  i<- as.numeric(substr(results, start=1, stop=1))
  for(n in 1:9){
  X<- (sum(results[i==n]))/(sum(results)-log(1+1/i))
  m<- max(X)
  }
  if(m>=0.851 & m<0.967){
    print("Fraud at 0.10 significance level")
  }
  if(m>=0.967 & m<1.212){
    print("Fraud at 0.05 significance level")
  }
  if(m>=1.212){
    print("Fraud at 0.01 significance level")
  }
  if(m<0.851){
    print("No fraud")
  }
}
trial<- c(10,20,25,30,40)
m.fnc(trial)

rm(list = ls()) #clear the workspace
results<- numeric(5)
d.fnc<- function(results){
  i<- as.numeric(substr(results, start=1, stop=1))
  for(n in 1:9){
    Y<- sum((sum(results[i==n]))/(sum(results)-log(1+1/i))^2)
    d<- sqrt(Y)
    }
  if(d>=1.212 & d<1.330){
    print("Fraud at 0.10 significance level")
  }
  if(d>=1.330 & d<1.569){
    print("Fraud at 0.05 significance level")
  }
  if(d>=1.569){
    print("Fraud at 0.01 significance level")
  }
  if(d<1.212){
    print("No fraud")
  }
}
trial<- c(10,20,25,30,40)
d.fnc(trial)