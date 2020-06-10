## Data Structure Final Exam
## Problem 3
# 2020/06/10 Keonwoo Park

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

## 속도 체크
Simulation_Results <- data.frame(Prime_1=rep(0,1),
                                 Prime_2=rep(0,1))

for(i1 in 1:2){
  T1<-Sys.time()
  if(i1==1){
    Prime_Number_1(100000)
  }else if(i1==2){
    Prime_Number_2(100000)
  }
  T2<-Sys.time()
  Simulation_Results[1,i1]=as.numeric(difftime(T2,T1,units="secs"))
}
print(Simulation_Results)



# 3.2
data_9_9<-matrix(NA,nrow=8,ncol=9)
for(i1 in 1:8){
  for(i2 in 1:9){
    data_9_9[i1,i2]=(i1+1)*i2
  }
}
data_9_9

# 3.2.1 (use while)
data_9_9<-matrix(NA,nrow=8,ncol=9)
r_count = 1
c_count = 1
while(r_count<9){
  while(c_count<10){
    data_9_9[r_count,c_count] = (r_count+1)*c_count
    
    c_count = c_count + 1
  }
  r_count = r_count + 1
  c_count = 1
}
data_9_9

# 3.2.1 (use repeat)
data_9_9<-matrix(NA,nrow=8,ncol=9)
r_count = 1
c_count = 1

repeat{
  repeat{
    data_9_9[r_count,c_count] = (r_count+1)*c_count
    c_count = c_count+1
    if(c_count == 10)break
  }
  r_count = r_count +1
  c_count = 1
  if(r_count == 9)break
}
data_9_9  



# 3.3
set.seed(1234)
data_9<-data.frame(x1=0,x2=0,x3=0,x4=0,x5=0,x6=0,Expectation=0)
a=sample(6,1000,replace=TRUE)
for(i in 1:7){
  if(i<7){
    data_9[,i] = length(a[which(a==i)])
  }else{
    data_9[,i] = mean(a)
  }
}
print(data_9)

# 3.4
data_10<-c(1,3,5,7,10)

temp_data =data_10[3]
data_10[3]= data_10[5]
data_10[5]= temp_data

print(data_10)
