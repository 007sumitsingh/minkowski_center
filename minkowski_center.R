X=c(1, 4, 15, -11, 12.6, 17,8, 18.9, -22, 13) #Input the data of which the minkowski center in required. 
sorted_X=sort(X)
range=max(X)-min(X)
lambda=0.1*range
sum=0
value=0
p<<-2 #global variable 
Dpc=function(c,p){
 for(i in 1:length(X)){
   value=abs(X[i]-c)^p
   sum=sum+value
 }
   return(sum)
}
Y=vector(mode="numeric", length=length(sorted_X))
for(i in 1:length(sorted_X)){
 Y[i]=Dpc(sorted_X[i],p)
}
a=which.min(Y)
czero=sorted_X[a]

Dpcdash=function(t,p){
  Z=vector(mode="numeric", length=length(sorted_X))
  for(i in 1:length(sorted_X)){
  Z[i]=t-sorted_X[i]
  }
  for(i in 1:length(sorted_X)){
  if (Z[i]>=0){
    Z[i]=Z[i]
    Qplus=vector()
    Qplus=which(Z>=0)
  }
  if (Z[i]<0){
    Z[i]=-Z[i]
    Qminus=vector()
    Qminus=which(Z<0)
  }
  }
  sum2=0
  value2=0
  for(i in Qplus){
    value2=(Z[i])^(p-1)
    sum2=sum2+value2
  }
  
  value3=0
  for(i in Qminus){
    
    value3=-(Z[i])^(p-1)
    sum2=sum2+value3
  }
  return(p*sum2)
}

u=vector()
u=which(sorted_X<czero)
y_one_dash=max(u[Dpc(u,p)>Dpc(czero,p)])

v=vector()
v=which(sorted_X>czero)
y_two_dash=min(v[Dpc(v,p)>Dpc(czero,p)])

c_one=czero-lambda*Dpcdash(czero,p)
while(!(c_one>y_one_dash & c_one<y_two_dash)){
  lambda=0.9*lambda
  c_one=czero-lambda*Dpcdash(czero,p)
}
if (abs(c_one-czero)<0.1){
  sprintf(" The value of Minkowski centre is %f", c_one)
  
} else if (Dpc(c_one,p)<=Dpc(czero,p)){
  czero=c_one
  Dpc(czero,p)=Dpc(c_one,p)

  while(!(c_one>y_one_dash & c_one<y_two_dash)){
    lambda=0.9*lambda
    c_one=czero-lambda*Dpcdash(czero,p)
  }
  sprintf(" The value of Minkowski centre is %f", c_one)
} else {
  
  lambda=0.9*lambda
  while(!(c_one>y_one_dash & c_one<y_two_dash)){
    lambda=0.9*lambda
    c_one=czero-lambda*Dpcdash(czero,p)
  }
  
  sprintf(" The value of Minkowski centre is %f", c_one)
  
}
 







 