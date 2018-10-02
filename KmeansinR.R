library(data.table)

library(ggplot2)

library(fpc)

water_data <- read.table("G:/DataScience/Filtered Topics/Clustering/water-treatment.data.txt",sep = ",",header = F,na.strings = c("?"))
setDT(water_data)
head(water_data)

colSums(is.na(water_data))

for(i in colnames(water_data)[!(colnames(water_data) %in% c("V1"))])
  set(x = water_data,i = which(is.na(water_data[[i]])), j = i, value = median(water_data[[i]], na.rm = T))

scaled_wd <- scale( water_data[,-c("V1"),with=F])

d <- dist(scaled_wd, method = "euclidean")

kclust <- kmeans(scaled_wd,centers = 4,iter.max = 100)
tunek <- kmeansruns(scaled_wd,krange = 1:10,criterion = "ch") 
tunek$bestk #3
tunekw <- kmeansruns(scaled_wd,krange = 1:10,criterion = "asw") 
tunekw$bestk #4


set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
data <- scaled_wd
wss <- sapply(1:k.max, 
              function(k){kmeans(scaled_wd, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)
