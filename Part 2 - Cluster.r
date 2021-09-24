# Alexandre Lebas - Océane Deletrez 

iris2 <- iris
iris2$Species <- NULL
(kmeans.result <- kmeans(iris2, 3))

table(iris$Species, kmeans.result$cluster)

plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3,
pch = 8, cex=2)

install.packages("fpc")

library(fpc)

pamk.result <- pamk(iris2)
pamk.result$nc

table(pamk.result$pamobject$clustering, iris$Species)

layout(matrix(c(1,2),1,2))
#plot(pamk.result$pamobject)
layout(matrix(1)) 

plot(pamk.result$pamobject)

install.packages("cluster")

library(cluster)

pam.result <- pam(iris2, 3)
table(pam.result$clustering, iris$Species)

layout(matrix(c(1,2),1,2))
#plot(pam.result)
layout(matrix(1))

plot(pam.result)

idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample

irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")

plot(hc, hang = -1, labels=iris$Species[idx])
rect.hclust(hc, k=3)
groups = cutree(hc, k=3)

hcFull <- hclust(dist(iris2), method="ave")

plot(hcFull, hang = -1, labels=iris$Species)
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)

 iris2 <- iris[-5] # remove class tags
iris2

 ds <- dbscan(iris2, eps=0.42, MinPts=5)
table(ds$cluster, iris$Species)

plot(ds, iris2)

plot(ds, iris2[c(1,4)])

plotcluster(iris2, ds$cluster)

set.seed(435)

idx <- sample(1:nrow(iris), 10)
newData <- iris[idx,-5]
newData <- newData + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)
myPred <- predict(ds, iris2, newData)
plot(iris2[c(1,4)], col=1+ds$cluster)
points(newData[c(1,4)], pch="*", col=1+myPred, cex=3)
table(myPred, iris$Species[idx])


