library(kernlab)
library(mlbench)
par(mfrow = c(2,3))

##################################### Spiral example ###############################
obj = mlbench.spirals(100,1,0.025)
my.data = 4 * obj$x

# spectral clustering
plot(my.data, main = "<<Sprial Example>>")

sc = specc(my.data, centers=2)
plot(my.data, col=sc, pch=4, main  = "spectral clustering")

# kmeans
clusters = kmeans(my.data, 2, iter.max = 10, nstart = 1, algorithm = "Lloyd", trace=FALSE)
plot(my.data, col = clusters$cluster, main  = "k-means")

###############################3#### Two clumps example ############################
x = rbind(matrix(rnorm(100, sd = 0.2), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.2), ncol = 2))
colnames(x) = c("x", "y")
plot(x, main = "<<Two clumps example>>")

# spectral clustering
sc = specc(x, centers = 2)
plot(x, col = sc, main = "spectral clustering")

# kmeans
clusters = kmeans(x, 2)
plot(x, col = clusters$cluster, main = "k-means")