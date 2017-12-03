library(png)
library(dplyr)

photo = readPNG("raccoon.png")

get_kmeans_centers = function(locs, k){
  # choose two random centers to start
  centers = sample(locs, k)
  
  counter = 1 # just curious how many iterations it takes to converge
  
  avgdiff = 1
  while(avgdiff > 0.01){
    # find distance
    dist = list()
    
    for (i in 1:k){
      center = centers[i]
      dist[paste0("group",i)] = list(sapply(locs, function(pixel) abs(pixel-center)))
    }
    
    dist_m = bind_rows(dist) # distance matrix
    clusters = apply(dist_m, 1, which.min) # cluster assignments
    
    new_locs = as.data.frame(cbind(locs, clusters))
    new_centers = tapply(new_locs$locs, new_locs$clusters, mean)
    
    avgdiff = sum(abs(centers-new_centers))/k
    
    locs = new_locs$locs
    centers = new_centers
    
    counter = counter + 1
  }
  
  res = list("cluster" = clusters, "centers" = centers)
  
  return (res) # returns a list of group assignments and mean for each group
}

compress = function(k, data){
  v = as.vector(data)
  km = get_kmeans_centers(v, k)
  
  # quantization
  compressed_m = matrix(km$cluster, ncol = dim(data)[2])
  book = km$centers
  
  print (book*256) # print the centers in terms of 256 scale for reference
  
  # plotting
  compressed_v = as.vector(compressed_m)
  new_v = rep(0,length(v))
  for (i in 1:length(v)){
    new_v[i] = book[compressed_v[i]]
  }
  new_m = matrix(new_v, ncol = dim(photo)[2])
  
  # rotating clockwise (R plots this weird)
  new_m = apply(new_m, 2, rev)
  image(1:1024, 1:768, t(new_m), col = gray((0:32)/32), useRaster = TRUE)
}

original = readPNG("raccoon.png")
original = apply(original, 2, rev)
image(1:1024, 1:768, t(original), col = gray((0:32)/32), useRaster = TRUE)
title(main = "Original")

# testing k = 2
compress(2, photo); title(main = "k = 2")

# testing k = 4
compress(4, photo); title(main = "k = 4")