
# Using R to Perform a K-Means Analysis

library(ggplot2) 
data <- data.frame(x = c(1,0.9, 0.85 , 0.8 ,5.1,0.7, 1.4, 4.7, 5.4, 4.5, 5.1, 4.7, 4.8, 5), 
                 y = c(1,1.2, 0.95, 1.3, 3.6, 1.2, 0.8, 3.5, 3.8, 4, 4.1, 3.9, 4.2, 4.3) ) 
data(ruspini)
df
ggplot(df, aes(x , y )) + geom_point() 

km=kmeans(df, 2, nstart=25)
km

index=km$cluster
plot(df[which(index==1),], aes(x, y ),xlim=c(0,5),ylim=c(0,5), pch=5,cex=2,col="blue")
lines(df[which(index==2),], pch=5,cex=2,type="p",col="red")
lines(df[which(index==3),],pch=16,cex=2, type="p",col="green")

df = as.data.frame(kmdata_orig[,2:4])
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)



# install packages, if necessary
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)

#import the student grades
grade_input = as.data.frame(read.csv("C:/Users/kavit/Desktop/BigDataAnalytics(IME780AN)/Data_Code/grades_km_input.csv"))

kmdata_orig = as.matrix(grade_input[,c("Student","English", "Math","Science")])
kmdata <- kmdata_orig[,2:4]
kmdata[1:10,]

wss <- numeric(15) 
for (k in 1:15) wss[k] <- sum(kmeans(kmdata, centers=k, nstart=25)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares") 

km = kmeans(kmdata,3, nstart=25)
km

df = as.data.frame(kmdata_orig[,2:4])
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)







#prepare the student data and clustering results for plotting
df = as.data.frame(kmdata_orig[,2:4])
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)

g1= ggplot(data=df, aes(x=English, y=Math, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=English,y=Math, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g2 =ggplot(data=df, aes(x=English, y=Science, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=English,y=Science, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g3 = ggplot(data=df, aes(x=Math, y=Science, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=Math,y=Science, color=as.factor(c(1,2,3))), 
             size=30, alpha=.3, show.legend=FALSE)

tmp = ggplot_gtable(ggplot_build(g1)) 

grid.arrange(arrangeGrob(g1 + theme(legend.position="none"),
                         g2 + theme(legend.position="none"),
                         g3 + theme(legend.position="none"),
                         top ="High School Student Cluster Analysis", ncol=1))









### a 5-dimensional example: 
## generate data set with two groups of data:
set.seed(1)
x <- rbind(matrix(rbinom(200, 2, 0.25), ncol = 4), 
           matrix(rbinom(200, 2, 0.75), ncol = 4)) 
colnames(x) <- c("a", "b", "c", "d") 
x

library(klaR)
library(questionr)
## run algorithm on x: 
cl <- kmodes(x, 2) 
cl

## visualize with some jitter for a-b plots: 
plot(jitter(x), col = cl$cluster) 
points(cl$modes, col = 1:4, pch = 8) 








