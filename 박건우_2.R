## Data Structure Final Exam
## Problem 2
# 2020/06/10 Keonwoo Park


# 2.1
data_csv <- read.csv("data.csv",
                     header=T)

data_csv
data_5 <- as.matrix(data_csv)

data_5

# 2.2

data_6<-data.frame(Origin=1:10,Index=rep(0,time=5))
temp_data <- data.frame(Origin= 0, Destination = 0, Distance = 0)
Index_distance<-NULL

prei = 0
i1 = 0
for(i in 1:10){
  for(j in 1:10){
    if(is.finite(data_5[i,j])){
      temp_data$Origin = i
      temp_data$Destination = j
      temp_data$Distance = data_5[i,j]
      Index_distance = rbind(Index_distance,temp_data)
      i1 = i1 +1
      # index table
      if(prei!=i){
        data_6$Index[i]=i1
        prei=i
      }
    }
    
  }
}
data_6
Index_distance


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





# 2.4



