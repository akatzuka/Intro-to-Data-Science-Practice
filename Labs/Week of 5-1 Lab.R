data <- read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/sample-grades.csv")

set.seed(1)
evals = c()
i = 0

cluste = kmeans(data, num_clusters)

for (i in c(2:40))
{
  num_clusters = i
  fitq = kmeans(data, num_clusters)
  eval = fitq$betweenss / fitq$totss
  evals <- c(evals, eval)
}

plot(evals, x = c(2:40), xlab = "k (Cluster Size)")

num_clusters = 4
cluster = kmeans(data, num_clusters)
cluster$betweens/cluste$totss

library(cluster)
sil = silhouette(cluster$cluster, dist(data))

# plot the silhouette
plot(sil)

# average silhouette width
sw = mean(sil[,3])
sw

sils = c()
for (i in c(2:40))
{
  num_clusters = i
  fitq = kmeans(data, num_clusters)
  sil = silhouette(fitq$cluster, dist(data))
  sw = mean(sil[,3])
  sils <- c(sils, sw)
}

plot(sils, x = c(2:40), xlab = "Cluster Size", ylab = "Avg. Silhouette Width")

hcluster_data <- read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/sample-grades.csv")

# you probably want to scale data before this step
hcluster_data <- scale(hcluster_data)
hc = hclust(dist(hcluster_data), method="complete")
plot(hc)

num_clusts = 3
clusters = cutree(hc, num_clusts)

hc2 = hclust(dist(hcluster_data), method="average")
plot(hc2)

q <- hclust(dist(dat),method = "average")
