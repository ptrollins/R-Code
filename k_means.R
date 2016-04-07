# Implement K-means clustering algorithm
# https://en.wikipedia.org/wiki/K-means_clustering


assignClusterToPoints <- function(Xcoord, Ycoord, K) {
  Points <- data.frame(Xcoord, Ycoord)
  ptslength <- nrow(Points)
  Xcenter <- NULL   # Initiate vector for x coordinates of K centroids
  Ycenter <- NULL   # Initiate vector for y coordinates of K centroids
  result <- rep(0, ptslength) # Initiate vector to maintain cluster number for each point
  iteration <- 0
  
  # Create K random points bound by the min and max of the supplied X and Y vectors
  for (i in 1:K) {
    Xcenter <- c(Xcenter,sample(min(Points[1]):max(Points[1]),1))
    Ycenter <- c(Ycenter,sample(min(Points[2]):max(Points[2]),1))
  }
  
  # Repeat until two itterations produce same Mean for each cluster
  repeat{
    Xcopy <- Xcenter   # Create copy to compare to new mean for repeat
    Ycopy <- Ycenter       
    X <- rep(0, K)     # Initiate vector for 
    Y <- rep(0, K)         
    Count <- rep(0, K) # 
    iteration <- (iteration + 1)
    
    print(Xcopy)
    print(Ycopy)
    
    # Loop through all given points to find which centroid they are closest to
    for (i in 1:ptslength) {
      # Set minimum distance to the Euclidean distance to the first centroid
      mindist = sqrt((Xcoord[i]-Xcenter[1])^2 + (Ycoord[i]-Ycenter[1])^2)
      Pointclass <- 1 # Initiate classification for that point to 1
      # Loop through other centroids to determine if they are closer
      for (j in 2:K){
        # Calculate the Euclidean distance to other centroids
        dist = sqrt((Xcoord[i]-Xcenter[j])^2 + (Ycoord[i]-Ycenter[j])^2)
        # Compare distance to current centroid to distance to current closest centroid
        # If current centroid is closer make it the min distance
        if (dist < mindist){
          mindist <- dist  # If current centroid is closer make it the min distance
          Pointclass <- j; # and set the classification to that centroids class
        }
      }
      # Once classification is completed the points X and Y coords are added to their
      # respective vectors for that class and the point count is incremented for that class
      X[Pointclass] <- (X[Pointclass]+Xcoord[i])
      Y[Pointclass] <- (Y[Pointclass]+Ycoord[i])
      Count[Pointclass] <- (Count[Pointclass] +1)
      # The classification is added to the results vector
      result[i] <- Pointclass
    }
    # Each class vector is looped through to calculate the mean for that cluster
    # which then becomes the new centroid for that cluster
    for (j in 1:K){
      # If a random center does not end up with any points then the function is called again
      if (Count[j]!=0){
        Xcenter[j] <- (X[j]/Count[j])
        Ycenter[j] <- (Y[j]/Count[j])
      } else {
        return(assignClusterToPoints(Xcoord, Ycoord, K))
      }
    }
    print(Xcenter) 
    print(Ycenter)
    # The newly calculated centroid is compared to the previous centroid
    # if they are the same the loop is exited otherwise it is repeated
    if(all(Xcopy == Xcenter) && all(Ycopy == Ycenter)){
      break
    }
  }
  cat(sprintf('%i iterations to converge\n', iteration))
  return(result) # The vector of classifications for each point is returned
}

Xcoord <- c(1,3,1,1,4,5,7,8,9,4,5,1,2)
Ycoord <- c(2,4,5,9,6,5,7,9,7,5,6,1,1)

K <- 3

clusters = rep(1, length(Xcoord))
plot(Xcoord, Ycoord, col=clusters)
clusters = assignClusterToPoints(Xcoord, Ycoord, K)
Points <- data.frame(Xcoord, Ycoord)

# Plot the cluster
plot(Points, col=clusters)

# Print a summary of points and their cluster
for (i in 1:nrow(Points)){
  cat(sprintf("(%i,%i) is in cluster %i\n", Points[i,1], Points[i,2], clusters[i]))
}
