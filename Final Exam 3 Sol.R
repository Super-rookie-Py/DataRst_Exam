## Data Structure Final Exam
## Problem 3

# 3.1
Prime_Number_1<-function(n){
  a1=3:n
  a2=2
  k=1
  for(i in 3:n){
    Flag=F
    for(j in 1:k){
      if((i %% a2[j])==0){
        Flag=T
        break
      }
    }
    if(Flag==F){
      k=k+1
      a2[k]=i
    }
  }
  return(a2)
}

Prime_Number_2<-function(n){
  a1=2:n
  a2=2
  k=2
  for(i in 2:n){
    a1<-a1[which((a1%%a1[1])!=0)]
    a2[k]=a1[1]
    k=k+1
    if(length(a1)==1){
      break
    }
  }
  return(a2)
}

c(system.time(Prime_Number_1(50000))[3],system.time(Prime_Number_2(50000))[3])

# 3.2
data_9_9<-matrix(NA,nrow=8,ncol=9)
for(i1 in 1:8){
  for(i2 in 1:9){
    data_9_9[i1,i2]=(i1+1)*i2
  }
}

# 3.2.1 (use while)
data_9_9<-matrix(NA,nrow=8,ncol=9)
i1=1
while(i1 < 9){
  i2=1
  while(i2 < 10){
    data_9_9[i1,i2]=(i1+1)*i2
    i2=i2+1
  }
  i1=i1+1
}

# 3.2.1 (use repeat)
data_9_9<-matrix(NA,nrow=8,ncol=9)
i1=1
repeat{
  i2=1
  repeat{
    data_9_9[i1,i2]=(i1+1)*i2
    if(i2==9){
      break
    }
    i2=i2+1
  }
  if(i1==8){
    break
  }
  i1=i1+1
}

# 3.3
set.seed(1234)
data_9<-data.frame(x1=0,x2=0,x3=0,x4=0,x5=0,x6=0,Expectation=0)

temp_data=sample(1:6,1000,replace=T)
for(i in 1:6){
  data_9[,i]=length(temp_data[temp_data==i])
  data_9$Expectation=data_9$Expectation+i*data_9[,i]
}
data_9$Expectation=data_9$Expectation/1000

# 3.4
data_10<-c(1,3,5,7,10)

temp_value<-data_10[5]
data_10[5]<-data_10[3]
data_10[3]<-temp_value
