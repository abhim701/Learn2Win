#http://www.sthda.com/english/articles/30-advanced-clustering/105-dbscan-density-based-clustering-essentials/

library(factoextra)
library(datasets)
data("multishapes")
df <- multishapes[, 1:2]
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df,  geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

install.packages("fpc")
install.packages("dbscan")
install.packages("factoextra")

# Load the data 
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

# Compute DBSCAN using fpc package
library("fpc")
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, scale = TRUE, MinPts = 5, method = c("hybrid", "raw", "dist"))

# Plot DBSCAN results
library("factoextra")
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
print(db)



clusters <- as.data.frame(db$cluster)
df_labels <- cbind(df, clusters)

dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)


# Cluster predictions with DBSCAN algorithm
# The function predict.dbscan(object, data, newdata) [in fpc package] can be used to predict the clusters for the points in newdata. 
# For more details, read the documentation (?predict.dbscan).