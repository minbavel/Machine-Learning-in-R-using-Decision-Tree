#1
outlook <- c("rain","overcast","rain","sunny","rain","rain","sunny","overcast","overcast","overcast","sunny","sunny","rain","rain","overcast","sunny","overcast","overcast","sunny","sunny","sunny","overcast")
humidity <- c(79,74,80,60,65,79,60,74,77,80,71,70,80,65,70,56,80,70,56,70,71,77)
windy <- c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE)
play <- c(FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE)
data1 <- data.frame(outlook,humidity,windy,play)
View(data1)

#2
library(tree)
outlook <- c("rain","overcast","rain","sunny","rain","rain","sunny","overcast","overcast","overcast","sunny","sunny","rain","rain","overcast","sunny","overcast","overcast","sunny","sunny","sunny","overcast")
humidity <- c(79,74,80,60,65,79,60,74,77,80,71,70,80,65,70,56,80,70,56,70,71,77)
windy <- c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,TRUE,TRUE)
play <- c(FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE)
data1 <- data.frame(outlook,humidity,windy,play)
decitree<- tree(play~outlook+windy+humidity,method = "class",data=data1)
plot(decitree)
text(decitree ,all=TRUE,pretty = TRUE,splits = TRUE, cex = 1)
decitree

#3
install.packages("rattle") 
library(rattle) 
df<-data.frame(weather)
View(weather)
summary(weather)
str(weather)

library(tree)
tree1 <- tree(RainTomorrow~Cloud3pm+Sunshine,method = "class",data=weather)
plot(tree1)
text(tree1 ,all=TRUE,pretty = TRUE,splits = TRUE, cex = 1)
summary(tree1)

#The Plot indicates that it wont rain if the cloud at 3pm value is less than 6.5
#It also indicates that it wont rain if sunshine value is greater than 9.3
#It also indicates that it will only rain if the sunshine is greater than 8.75 and 9.3

library(party)
ctree1 <- ctree(RainTomorrow~Cloud3pm+Sunshine,data=weather)
plot(ctree1)

#The Difference between both the plot is it shows there is 0.7 probability that 
#it will rain if the value is greater than 7 
#It is easier in ctree but it does not provide the exact values like tree

#4
library(rattle) 
df<-data.frame(weather)

rain<- ctree(RainTomorrow ~ Sunshine + Pressure9am + Cloud9am, data =weather)
plot(rain)

#5
library(rattle) 
df<-data.frame(weather)
rain<- ctree(RainTomorrow ~ Sunshine + Pressure9am + Cloud9am, data =weather)

predict1 <- predict(rain,weather)
table(predict1,weather$RainTomorrow)
plot(predict1)

