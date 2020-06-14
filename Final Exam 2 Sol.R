## Data Structure Final Exam
## Problem 2

set.seed(2345)
x=runif(10,0,100)
y=runif(10,0,100)
data_distance<-matrix(nrow=10,ncol=10)
for(i1 in 1:9){
  for(i2 in (i1+1):10){
    if(runif(1)<0.8){
      data_distance[i1,i2]=((x[i1]-x[i2])^2+(y[i1]-y[i2])^2)^(1/2)
      data_distance[i2,i1]=data_distance[i1,i2]
    }else{
      data_distance[i1,i2]=Inf
      data_distance[i2,i1]=Inf 
    }
  }
}
write.csv(data_distance,"data.csv", row.names=F)

# 2.1
data_5<-as.matrix(read.csv("data.csv"))

# 2.2
data_6<-data.frame(Origin=1:10,Index=rep(0,time=5))
Index_distance<-NULL

temp_distance<-data.frame(Origin=0,Destination=0,Distance=0)
i1=0
pre_i=0
for(i in 1:10){
  for(j in 1:10){
    if(is.finite(data_5[i,j])){
      temp_distance$Origin=i
      temp_distance$Destination=j
      temp_distance$Distance<-data_5[i,j]
      Index_distance<-rbind(Index_distance,temp_distance)
      ## index table
      i1=i1+1
      if(pre_i!=i){
        data_6$Index[i]=i1
        pre_i=i
      }
    }
  }
}

# 2.3
set.seed(1234)
temp_data=NULL
for(i in 1:10){
  Sequence=sample(1:10)
  distance=0
  temp_data=rbind(temp_data,c(Sequence,distance))
}
data_7<-as.data.frame(temp_data)
names(data_7)=c(paste0("x",1:10),"distance")

for(i1 in 1:10){
  distance=0
  for(i2 in 1:9){
    distance=distance+data_5[data_7[i1,i2],data_7[i1,i2+1]]
  }
  data_7$distance[i1]=distance+data_5[data_7[i1,10],data_7[i1,1]]
}

# 2.4
Selection_Sort_DF<-function(d,col_num=1,decreasing=FALSE){
  size_d<-length(d[,1])
  if(decreasing==FALSE){
    for(i1 in 1:(size_d-1)){
      min_value=d[i1,col_num]
      min_index=i1
      for(i2 in (i1+1):size_d){
        if(d[i2,col_num]<min_value){
          min_value=d[i2,col_num]
          min_index=i2
        }
      }
      ### Swap
      temp_value=d[i1,]
      d[i1,]=d[min_index,] # min
      d[min_index,]=temp_value
    }
  }else{
    for(i1 in 1:(size_d-1)){
      max_value=d[i1,col_num]
      max_index=i1
      for(i2 in (i1+1):size_d){
        if(d[i2,col_num]>max_value){
          max_value=d[i2,col_num]
          max_index=i2
        }
      }
      ### Swap
      temp_value=d[i1,]
      d[i1,]=d[max_index,] # max
      d[max_index,]=temp_value
    }
  }
  return(d)
}

data_8<-Selection_Sort_DF(data_7,11)

