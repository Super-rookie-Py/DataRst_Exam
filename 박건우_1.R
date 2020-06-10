## Data Structure Final Exam
## Problem 1

# 2020/06/10 Keonwoo Park


set.seed(1234)
Year=rep(2011:2020,each=12*30)
Month=rep(1:12,each=30,time=10)
Age=sample(1:100,10*12*30,replace=T)
Height=Age+rnorm(10*12*30,5,1)
Weight=Height/2+rnorm(10*12*30,5,1)

# 1.1
data_set <- data.frame(Year, Month, Age, Height, Weight)
data_set

# 1.2

write.csv(data_set,"data_1.csv",row.names=FALSE)

# 1.3

for (i in 2011:2020){
  temp_data<-data_set[which(data_set$Year==i),]
  write.csv(temp_data,paste0("data_",i,".csv"),row.names = FALSE)
}

# 1.4

data_2<-data.frame(Year=2011:2020,Count=rep(0,time=10))

for(i in 2011:2020){
  data_2$Count[i-2010]=length(data_set[which(data_set$Year==i & data_set$Height>50),1])
}
data_2

# 1.5

data_3<-data.frame(Age_Level=1:10,Mean_Weight=rep(0,time=10),SD_Weight=rep(0,time=10))

for(i in c(1:10*10)){
  interval = data_set$Weight[which(data_set$Age<(i+1)& data_set$Age>(i-10))]
  data_3$Mean_Weight[i/10] = mean(interval)
  data_3$SD_Weight[i/10]=sd(interval)
}
data_3
# 1.6

data_4<-data.frame(Age_Level=1:10,Mean_Height=rep(0,time=10),SD_Height=rep(0,time=10))



for(i in c(1:10*10)){
  interval = data_set$Height[which(data_set$Age<(i+1) & data_set$Age>(i-10) & data_set$Month %in% c(3,4,5,9,10,11))]
  data_4$Mean_Height[i/10] = mean(interval)
  data_4$SD_Height[i/10]=sd(interval)
}
data_4

