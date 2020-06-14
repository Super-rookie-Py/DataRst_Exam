## Data Structure Final Exam
## Problem 1

set.seed(1234)
Year=rep(2011:2020,each=12*30)
Month=rep(1:12,each=30,time=10)
Age=sample(1:100,10*12*30,replace=T)
Height=Age+rnorm(10*12*30,5,1)
Weight=Height/2+rnorm(10*12*30,5,1)

# 1.1

data_1<-data.frame(Year,Month,Age,Height,Weight)

# 1.2

write.csv(data_1,"data_1.csv",row.names=F)

# 1.3

for(i in 2011:2020){
  write.csv(data_1[which(data_1$Year==i),],paste0("data_",i,".csv"),row.names=F)
}

# 1.4

data_2<-data.frame(Year=2011:2020,Count=rep(0,time=10))
for(i in 1:10){
  data_2$Count[i]=length(data_1$Height[which(data_1$Height>50 & data_1$Year==(2010+i))])
}
data_2
# 1.5

data_3<-data.frame(Age_Level=1:10,Mean_Weight=rep(0,time=10),SD_Weight=rep(0,time=10))
for(i in 1:10){
  data_3$Mean_Weight[i]=mean(data_1$Weight[which(data_1$Age/10<=i & data_1$Age/10>(i-1))])
  data_3$SD_Weight[i]=sd(data_1$Weight[which(data_1$Age/10<=i & data_1$Age/10>(i-1))])
}
data_3
# 1.6

data_4<-data.frame(Age_Level=1:10,Mean_Height=rep(0,time=10),SD_Height=rep(0,time=10))
for(i in 1:10){
  data_4$Mean_Height[i]=
    mean(data_1$Height[which(data_1$Age/10<=i & data_1$Age/10>(i-1) & 
                               data_1$Month %in% c(3,4,5,9,10,11))])
  data_4$SD_Height[i]=
    sd(data_1$Height[which(data_1$Age/10<=i & data_1$Age/10>(i-1) &
                             data_1$Month %in% c(3,4,5,9,10,11))])
}

data_4
