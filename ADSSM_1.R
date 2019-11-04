#question 1
A <- matrix(c(1,1,1,1,2,3,0,-1,-3,4,1,2,1,2,-1,1),nrow = 4,byrow = T)
B <- c(13,-1,10,1)
solve(A,B)

#x=2,y=0,z=6,w=5

#question 2
#a
food <- read.csv("food.csv")
Foodstuff <- food$Foodstuff
food$Foodstuff <- NULL
food_centred <- scale(food,center = TRUE,scale = FALSE)
#centralise the data

#b
#find the covariance matrix
food_cov <- cov(food_centred)
food_eig <- eigen(food_cov)
food_eig$values
#solve the eigenvalue of covariance matrix
#eigenvalue is 844816.7657  17440.5141   7848.1552   819.1166

i <- 1
Expect <- sum(food_eig$values[1:i])/sum(food_eig$values)
while(Expect < 0.95)
  i <- i + 1
  Eexpect <- sum(food_eig$values[1:i])/sum(food_eig$values)
print(i)
#find the minimum number of eigenvalues to explain at least 95% variation in the data
#the minimum number of it is 1

#c
food.ruduced_1 <- food_centred %*% food_eig$vectors[,1:1]
#Use PCA to first principal component
colnames(food.ruduced_1) <- c("PC1")
food.ruduced_1 <- as.data.frame(food.ruduced_1)
food.ruduced_1$Foodstuff <- Foodstuff
library(ggplot2)
ggplot(data = food.ruduced_1, aes(x = PC1,y = PC1,color=Foodstuff)) +
  geom_point()
#plot the result of first component analysis


food.country <- t(food_centred)
cov_food.country <- cov(food.country)
eig_food.country <- eigen(cov_food.country)
food.ruduced_2 <- food.country %*% eig_food.country$vectors[,1:2]
#use PCA onto its two principal component 
colnames(food.ruduced_2) <- c("PC1","PC2")
food.ruduced_2 <- as.data.frame(food.ruduced_2)
country <- colnames(food)
food.ruduced_2$country <- country
library(ggplot2)
ggplot(data = food.ruduced_2, aes(x=PC1,y=PC2)) +
  geom_point(aes(colour=country))
#plot the result of two principal component analysis
#the patteens of the data: 
#the north ireland appears to be differnent from others


#d
#the cereals and soft drinks might caust the observed pattern


#problem 3
#a
library(jpeg)
Galaxy <- readJPEG("gg.jpg")
Red <- Galaxy[,,1]
Green <- Galaxy[,,2]
Blue <- Galaxy[,,3]
#separate the Red,Green and Blue colour channels

Red_pca <- prcomp(Red,center=FALSE)
Green_pca <- prcomp(Green,center=FALSE)
Blue_pca <- prcomp(Blue,center=FALSE)
#perform PCA

#b
Red_compressed <- Red_pca$x[,1:1] %*% t(Red_pca$rotation[,1:1])
Green_compressed <- Green_pca$x[,1:1] %*% t(Green_pca$rotation[,1:1])
Blue_compressed <- Blue_pca$x[,1:1] %*% t(Green_pca$rotation[,1:1])
#construct the compressed version of each colour channel


#c
Galaxy_1 = sapply(list(Red_compressed,Green_compressed,Blue_compressed),identity,simplify = "array")
#combin the three matrix into a array
writeJPEG(Galaxy_1,paste("Galaxy_1.jpg"))
file.info("Galaxy_1.jpg")$size/file.info("Galaxy.jpg")$size*100
#output a image

Red.compressed_50 <- Red_pca$x[,1:50] %*% t(Red_pca$rotation[,1:50])
Green.compressed_50 <- Green_pca$x[,1:50] %*% t(Green_pca$rotation[,1:50])
Blue.compressed_50 <- Blue_pca$x[,1:50] %*% t(Green_pca$rotation[,1:50])
Galaxy_50 = sapply(list(Red.compressed_50,Green.compressed_50,Blue.compressed_50),identity,simplify = "array")
writeJPEG(Galaxy_50,paste("Galaxy_50.jpg"))
file.info("Galaxy_50.jpg")$size/file.info("Galaxy.jpg")$size*100

Red.compressed_500 <- Red_pca$x[,1:500] %*% t(Red_pca$rotation[,1:500])
Green.compressed_500 <- Green_pca$x[,1:500] %*% t(Green_pca$rotation[,1:500])
Blue.compressed_500 <- Blue_pca$x[,1:500] %*% t(Green_pca$rotation[,1:500])
Galaxy_500 = sapply(list(Red.compressed_500,Green.compressed_500,Blue.compressed_500),identity,simplify = "array")
writeJPEG(Galaxy_500,paste("Galaxy_500.jpg"))
file.info("Galaxy_500.jpg")$size/file.info("Galaxy.jpg")$size*100

Red.compressed_1400 <- Red_pca$x[,1:1400] %*% t(Red_pca$rotation[,1:1400])
Green.compressed_1400 <- Green_pca$x[,1:1400] %*% t(Green_pca$rotation[,1:1400])
Blue.compressed_1400 <- Blue_pca$x[,1:1400] %*% t(Green_pca$rotation[,1:1400])
Galaxy_1400 = sapply(list(Red.compressed_1400,Green.compressed_1400,Blue.compressed_1400),identity,simplify = "array")
writeJPEG(Galaxy_1400,paste("Galaxy_1400.jpg"))
file.info("Galaxy_1400.jpg")$size/file.info("Galaxy.jpg")$size*100
