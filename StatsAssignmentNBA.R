setwd("C:/Users/samue/Desktop/Fall 19 Semester/Stats/Assignment")
install.packages('ggfortify')
library(ggfortify)

# Load in three different per game statistics,we will filter by games played and minutes played to eliinate players without enough data
data.2019 = read.csv("2019-20 data NBA")
data.2019advanced = read.csv("advancedstats2019")
data.2013 = read.csv("2013-14 NBA")
data.1997 = read.csv("1997-98 NBA")


# Remove some variables that are not necessary for our analysis
# (Rank, Team, games played and games started,Personal fouls,age and 3point% to account for dividing by zero)

data.2019advancedupdated= data.2019advanced[,c(2,24)]

data.2019updated = data.2019[,c(2,3,6,8:13,15:28,30)]
final2019 = merge(data.2019updated,data.2019advancedupdated, by = "Player")

# remove name, games played and position and Filtering by minutes played and games played
final2019 = final2019[which(final2019$G >10 & final2019$MP >14),]
predictors2019 = final2019[,c(4:9,14:24)]

# PCA
pca2019 = princomp(predictors2019,cor = T)
plot(pca2019, type = 'l')
summary(pca2019)
# 3 Principal components explain ~78% of the variance


final2019$wsfactor = ifelse(final2019$WS > .135, "2", ifelse(final2019$WS >.059, "1", "0"))
summary(final2019$WS.48)
# Used 1st Quartile and 3rd quartile for partioning
install.packages("pca3d")
library(pca3d)
pca3d(pca2019, group =final2019$wsfactor)
pca2d(pca2019, group = final2019$wsfactor, legend = 'topleft')

pca2019$loadings

# Clustering with 9 variables based on player type
# first we want to remove all the variables except for ones that contribute to playing style
cl2019 = final2019[,c(9,11,15,19:23)]
dim(cl2019)
apply(cl2019,2,min)
apply(cl2019,2,max)

plot(c(0,10),c(0,20),type="n",xlab=var,ylab="Value",main="Profile Plot")
for (k in (1:306))
{
  points(1:8,cl2019[k,],type="l")
}
#standardize data
scaled2019<-scale(cl2019,center=TRUE,scale=TRUE)
var(scaled2019)
apply(scaled2019,2,min)
apply(scaled2019,2,max)

plot(c(0,10),c(-2,6),type="n",xlab=var,ylab="Value",main="Profile Plot")
for (k in (1:306))
{
  points(1:8,scaled2019[k,],type="l")
}
# Andrews plot
install.packages("andrews")
library(andrews)
andrews(scaled2019,type = 1,clr =1, ymax = 5)


# Trim filter and scale 1997 and 2013 data
data1997.updated = data.1997[which(data.1997$G >10 & data.1997$MP >14),]
dim(data1997.updated)
data1997.trim = data1997.updated[,c(13,16,20,24:28)]
data1997.scaled<-scale(data1997.trim,center=TRUE,scale =TRUE)

data2013.updated = data.2013[which(data.2013$G >10 & data.2013$MP >14),]
dim(data2013.updated)
data2013.trim = data2013.updated[,c(13,16,20,24:28)]
data2013.scaled<-scale(data2013.trim,center=TRUE,scale =TRUE)

# Do clustering on 2 principal components of the scaled data
library(cluster)
nba2019.pca <- princomp(scaled2019)
nba2019.pkcl <- kmeans(nba2019.pca$scores,5,30)
par(mfrow=c(1,1))
plot(nba2019.pca$scores[,1:2], col = nba2019.pkcl$cluster, main = "2019 Season")
points(nba2019.pkcl$centers[,c(1,2)], col = 1:2, pch ="+", cex=2)


#create scores now based on 2019 loadings for 2013 and 1997 data
nba2019.loadings = loadings(nba2019.pca)[]
nba2019.loadings[,1:2]
nba2013.scores = data2013.scaled%*%nba2019.loadings
nba1997.scores = data1997.scaled%*%nba2019.loadings

#Use first two PC components of 2019 for 1997 and 2013 data

par(mfrow=c(1,1))
nba2013.pkcl <- kmeans(nba2013.scores,5,30)
plot(nba2013.scores[,1:2], col = nba2013.pkcl$cluster, main = "2013 Season")
points(nba2013.pkcl$centers[,c(1,2)], col = 1:2, pch ="+", cex=2)

par(mfrow=c(1,1))
nba1997.pkcl <- kmeans(nba1997.scores,5,30)
plot(nba1997.scores[,1:2], col = nba1997.pkcl$cluster, main = "1997 Season")
points(nba1997.pkcl$centers[,c(1,2)], col = 1:2, pch ="+", cex=2)


autoplot(kmeans(nba2019.pca$scores,5), data = nba2019.pca$scores, scale = 0)
autoplot(pam(nba2019.pca$scores, 5), frame = TRUE, frame.type = 'norm', scale = 0)

autoplot(kmeans(nba1997.scores,5), data = nba1997.scores, scale = 0)
autoplot(pam(nba1997.scores, 5), frame = TRUE, frame.type = 'norm')

autoplot(kmeans(nba2013.scores,5), data = nba2013.scores, scale = 0)
autoplot(pam(nba2013.scores, 5), frame = TRUE, frame.type = 'norm')


