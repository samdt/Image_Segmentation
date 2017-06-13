# MRI Image segmentation
# Let's try this with an MRI image of the brain
setwd("C:\\Users\\Vaibhav\\Desktop\\Chaps\\ML\\Clustering")
healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
par(mfrow=c(1,1))
str(healthyMatrix)
image(healthyMatrix, axes=FALSE)


# Hierarchial clustering
healthyVector = as.vector(healthyMatrix)

#try using heirarchical clustering, we'll get error since the data size is very big (number of rows )
length(healthyVector)#365636 data points to be clustered - so we'll use kmeans clustering

# we get an error, difficult to find 365636C2 distances

distance = dist(healthyVector, method = "euclidean")
# We have an error - why?
#we can't find the distance with such a large vector. This is the limitation of heirarchical clustering. So v ll use k-means clustering.

str(healthyVector)

# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))


# K means clustering
#Randomly Specify number of clusters to start with
k = 5

# Run k-means
set.seed(1)
KMC = kmeans(healthyVector, centers = 5, iter.max = 50)
?kmeans
#first argument is a vector, 2nd argument is centers = 5, 
#so here we are assuming in 50 interations the clusters will converge

str(KMC)
#KMC is a list of 9
#KMC$withinss (xi - xbar)^2
#KMC$betweenss (xbar - x grand mean)^2

healthyClusters<-KMC$cluster

# Extract clusters
healthyClusters = KMC$cluster
length(healthyClusters)
KMC$centers[2]

# Plot the image with the clusters. For that first convert into matrix
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
healthyClusters=matrix(healthyClusters, nrow(healthyMatrix), ncol(healthyMatrix))

#Image 
image(healthyClusters,axes=FALSE,col = rainbow(k))
image(healthyClusters,axes=FALSE,col = grey(seq(0,1, length=5)))
# Scree Plots
set.seed(1)

?sapply #applies function on an array so, we'll apply function (x) for k = 2 to 10  
#that function should calculate the sum of withinss for each k
NumClusters = seq(2,10,1)
#or 
NumClusters=2:10
SumWithinss = sapply(2:10, function(k) sum(kmeans(healthyVector, centers=k, iter.max=1000)$withinss))
#Plot each sum of withinss with number of clusters
plot(NumClusters,SumWithinss,type="b")
#by default no type specified will give u scatter plot
#type = l will give line
#type = b stands for both
#We want to find the number of clusters for which increasing the number of 
#   clusters further does not significantly help reduce the within-cluster sum-of-squares
#   so looks like 5 is a good choice

#Detecting Tumor 
#Previously we found the healthy brain image. Now we want to identify patients with tumour.
#Tumour is generally found in the front lobe of the brain


tumor<-read.csv("https://storage.googleapis.com/dimensionless/Analytics/tumor.csv",header=FALSE)
str(tumor)
tumorMatrix<-as.matrix(tumor)
image(tumorMatrix, col=grey(seq(0,1, length=5)))
tumorVector<-as.vector(tumorMatrix)
image(tumorMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

#now we'll use supervised learning
#  we'll apply the kmeans clustering results that we found using the healthy brain image 

library(flexclust)
#kcca - k centroid clusters analysis
#now we need to convert the info from clustering algorithm to an object of the class KCCA
#This conversion is needed before v can use the predict function on the test set tumour vector
KMC.kcca = as.kcca(KMC, healthyVector)
#healthyvector has intensity values and kmc is the object of type kmeans with k=5 applied on healthyVector
#so we have created the kcca object using kmeans 
KMC.kcca
KMC$size

summary(KMC.kcca)
#output shows: col 1 - the size of each cluster
# col 2 - average distance between the points in each cluster
# col 3 - max distance in between points in each cluster
# col 4 - seperation???

tumorClusters<-predict(KMC.kcca,newdata=tumorVector)
summary(tumorClusters)
tumorClusters#so tumorVector has been assigned 5 clusters as per the kcca object
str(tumorClusters)
table(tumorClusters)

# Segmented Image
#converting into matrix to form the image
dim(tumorClusters)<-c(nrow(tumorMatrix),ncol(tumorMatrix))
dim(tumorMatrix)

par(mfrow=c(1,2))
image(tumorClusters,axes=FALSE,col=rainbow(k))
image(healthyMatrix, axes=FALSE, col=rainbow(k))

image(tumorClusters,axes=FALSE,col=grey(seq(0,1,length=5))) #so there is a big tumour
image(healthyMatrix, axes=FALSE, col=grey(seq(0,1,length=5)))
