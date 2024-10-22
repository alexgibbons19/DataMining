# Alex Gibbons
# Classwork 4/25
# Iris k-clustering

library("datasets")
head(iris)

# Drop species col
df <- scale(iris[-5])
head(df)

set.seed(1234)
nc <- NbClust(df, min.nc=2,max.nc=10,method="kmeans",distance="euclidean",index="alllong")
table(nc$Best.n[1,])

# nbclust suggests we use 2 clusters, however 3 is very close to the same number of indices
#   we will use 3 as there are 3 species of iris in our dataset

set.seed(1234)
fit.km <- kmeans(df,3,nstart=25)
print(fit.km$size)

# from results, first cluster contains 50 data points, second 53, and third 47

print(fit.km$centers)

print(aggregate(iris[-5],by=list(cluster=fit.km$cluster),mean))

ct.km <- table(iris$Species,fit.km$cluster)
print(ct.km)

library("flexclust")
print(randIndex(ct.km))

# Aggregate rank index is .62 which isnt the best but is still acceptable

data.frame(iris$Species,fit.km$cluster)

par(mfrow=c(2,1))
plot(iris[c("Petal.Length","Petal.Width")],col=fit.km$cluster,main="Based on Clusters generated")
plot(iris[c("Petal.Length","Petal.Width")],col=iris$Species, main="Based on actual types")

# While we knew to choose 3 clusters because we knew there were 3 species of iris'
#   From the plots we can infer why NbClust recommended 2
#   Clusters 2 and 3 have much overlap

