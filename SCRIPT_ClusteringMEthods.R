library(ggthemes)


dtEnv= read.csv("MetalsInAbEnv.csv", header = TRUE)
str(dtEnv)

colnames(dtEnv) = c("Riv","Reg", "RepID", "HgS", "CdS", "PbS","CrS","CoS","NiS", "CuS", "ZnS","AsS", 
                    "CdW", "PbW", "CrW", "CoW", "NiW", "CuW", "ZnW", "AsW",
                    "Clay", "OM", "pH", "Temp", "Cond", "O2", "Hardness")
dtmet = dtEnv[, c(3:20)]
head(dtmet)

## dataset with all replicates kept
dtAll = textshape::column_to_rownames(dtmet, loc = 1)
dtAll.s = scale(dtAll) ### both water and sediment results in 1 dataframe is not correct
                      ### because results for a rep 1 for water and sediment come from differenent sample

dtSed =  dtEnv[, c(3:12)]
dtAllSed = textshape::column_to_rownames(dtSed, loc = 1)
dtAllSed.s = scale(dtAllSed)

dtWater =  dtEnv[, c(3, 13:20)]
dtAllWater = textshape::column_to_rownames(dtWater, loc = 1)
dtAllWater.s = scale(dtAllWater)


### dataset with median values ####
dtmet1 = dtEnv[, c(1, 4:20)]
library(dplyr)
dtMed = dtmet1 %>%
  group_by(Riv) %>%
  summarise_at(vars("HgS":"AsW"),median)

dtMed = data.frame(dtMed)
dtMed = textshape::column_to_rownames(dtMed, 1)

dtMed.s = scale(dtMed)

### dataset with mean values ####
dtmet1 = dtEnv[, c(1, 4:20)]
library(dplyr)
dtMean= dtmet1 %>%
  group_by(Riv) %>%
  summarise_at(vars("HgS":"AsW"),mean)

dtMean = data.frame(dtMean)
dtMean = textshape::column_to_rownames(dtMean, 1)

dtMean.s = scale(dtMean)



########## FUZZY C MEANS CLUSTERING ####### https://www.datanovia.com/en/lessons/fuzzy-clustering-essentials/fuzzy-c-means-clustering-algorithm/

## all replicates water and sediment
fviz_nbclust(dtAll.s, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

FCM_All = e1071::cmeans(dtAll.s, centers = 4, iter.max = 100)

p.FCM_All  = factoextra::fviz_cluster(list(data = dtAll.s, cluster=FCM_All$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette = "jco",
             ggtheme = theme_economist(),
             main = "FCM - all reps")


## all replicates  sediment only
fviz_nbclust(dtAllSed.s, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

FCM_AllSed = e1071::cmeans(dtAllSed.s, centers = 5, iter.max = 100)

p.FCM_AllSed  = factoextra::fviz_cluster(list(data = dtAllSed.s, cluster=FCM_AllSed$cluster), 
                                      ellipse.type = "norm",
                                      ellipse.level = 0.68,
                                      palette = "jco",
                                      ggtheme = theme_economist(),
                                      main = "FCM - all reps Sediment")
## all replicates  water only
fviz_nbclust(dtAllWater.s, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

FCM_AllWater = e1071::cmeans(dtAllWater.s, centers = 5, iter.max = 10000)

p.FCM_AllWater  = factoextra::fviz_cluster(list(data = dtAllWater.s, cluster=FCM_AllWater$cluster), 
                                         ellipse.type = "norm",
                                         ellipse.level = 0.68,
                                         palette = "jco",
                                         ggtheme = theme_economist(),
                                         main = "FCM - all reps Water")

## median water and sediment
fviz_nbclust(dtMed.s, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

FCM_Med = e1071::cmeans(dtMed.s, centers = 4, iter.max = 100)

p.FCM_Med =  factoextra::fviz_cluster(list(data = dtMed.s, cluster=FCM_Med$cluster), 
                         ellipse.type = "norm",
                         ellipse.level = 0.95,
                         palette = "jco",
                         ggtheme = theme_economist(),
                         main = "FCM - Medians")

## mean water and sediment
fviz_nbclust(dtMean.s, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

FCM_Mean = e1071::cmeans(dtMean.s, centers = 4, iter.max = 100, dist = "euclidean",
                         m = 2)

p.FCM_Mean = factoextra::fviz_cluster(list(data = dtMean.s, cluster=FCM_Mean$cluster), 
                         ellipse.type = "norm",
                         ellipse.level = .95,
                         geom = c("text", "point"),
                         palette = "jco",
                         ggtheme = theme_economist(),
                         show.clust.cent = F,
                         main = "FuzzyCmeans - Mean")

corrplot(FCM_Mean$membership, is.corr = F)

factoextra::fviz_cluster(list(data = dtMean.s, cluster=FCM_Mean$cluster), 
                         axes = c(1,3),
                         ellipse.type = "norm",
                         ellipse.level = .95,
                         geom = c("text", "point"),
                         palette = "jco",
                         ggtheme = theme_economist(),
                         show.clust.cent = F,
                         main = "FCM- Means") 

factoextra::fviz_cluster(list(data = dtMean.s, cluster=FCM_Mean$cluster), 
                         axes = c(2,3),
                         ellipse.type = "norm",
                         ellipse.level = .95,
                         geom = c("text", "point"),
                         palette = "jco",
                         ggtheme = theme_economist(),
                         show.clust.cent = F,
                         main = "FCM- Means") 



## test with fanny###
fanny_mean = cluster::fanny(dtMean.s, 4, memb.exp = 1.2, maxit = 100)

p.fanny_Mean = factoextra::fviz_cluster(fanny_mean, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right",
             main = "Fanny (FuzzyCmeans) with means")





########### K MEANS CLUSTERING ########## https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/
### all replicates
dtAll.s
fviz_nbclust(dtAll.s,FUNcluster = kmeans, method = "wss")

kmeans_All <- kmeans(dtAll.s, 4, iter.max = 100, nstart = 100)
p.kmeans_All = factoextra::fviz_cluster(list(data = dtAll.s, cluster=kmeans_All$cluster), 
                         ellipse.type = "norm",
                         ellipse.level = 0.68,
                         palette = "jco",
                         ggtheme = theme_economist(),
                         main = "kmeans - all replicates")


### All reps sediment data only 
fviz_nbclust(dtAllSed.s,FUNcluster = kmeans, method = "wss")

kmeans_AllSed <- kmeans(dtAllSed.s, 5, iter.max = 100, nstart = 100)
p.kmeans_AllSed = factoextra::fviz_cluster(list(data = dtAllSed.s, cluster=kmeans_AllSed$cluster), 
                                        ellipse.type = "norm",
                                        ellipse.level = 0.68,
                                        palette = "jco",
                                        ggtheme = theme_economist(),
                                        main = "kmeans - all replicates Sediment data")

### All reps water data only 
fviz_nbclust(dtAllWater.s,FUNcluster = kmeans, method = "wss")

kmeans_AllWater <- kmeans(dtAllWater.s, 5, iter.max = 100, nstart = 100)
p.kmeans_AllWater = factoextra::fviz_cluster(list(data = dtAllWater.s, cluster=kmeans_AllWater$cluster), 
                                           ellipse.type = "norm",
                                           ellipse.level = 0.68,
                                           palette = "jco",
                                           ggtheme = theme_economist(),
                                           main = "kmeans - all replicates Water data")

### median values ####
dtMed.s
fviz_nbclust(dtMed.s,FUNcluster = kmeans, method = "wss")

kmeans_Median <- kmeans(dtMed.s, 4, iter.max = 100, nstart = 100)
p.kmeans_Median = factoextra::fviz_cluster(list(data = dtMed.s, cluster=kmeans_Median$cluster), 
                         ellipse.type = "norm",
                         ellipse.level = 0.95,
                         palette = "jco",
                         ggtheme = theme_economist(),
                         main = "kmeans - median values")

### mean values ####
dtMean.s
fviz_nbclust(dtMean.s,FUNcluster = kmeans, method = "wss")

kmeans_Mean<- kmeans(dtMean.s, 4, iter.max = 100, nstart = 100)
p.kmeans_Mean =  factoextra::fviz_cluster(list(data = dtMean.s, cluster=kmeans_Mean$cluster), 
                         ellipse.type = "norm",
                         ellipse.level = 0.95,
                         palette = "jco",
                         ggtheme = theme_economist(),
                         main = "kmeans - with mean " )



########### PAM CLUSTERING ##########

PAM_Mean = cluster::pam(dtMean.s, 4, metric = "euclidean", stand = FALSE)

p.PAM_Mean =  factoextra::fviz_cluster(list(data = dtMean.s, cluster=PAM_Mean$cluster), 
                         ellipse.type = "norm",
                         ellipse.level = 0.95,
                         palette = "jco",
                         ggtheme = theme_economist(),
                         main = "PAM clustering - with mean " )


###### hierchical k-means clustering #####

## All data
HiKmeans = factoextra::hkmeans(dtMean.s,k=4, iter.max = 100 )

p.HiKmeans = factoextra::fviz_cluster(HiKmeans, cex = 0.6, 
                         ellipse.type = "norm",
                         ellipse.level = 0.95,
                         pallet = "jco",
                         main = "Hierarchical kmeans -  with mean")

fviz_dend(HiKmeans, cex = 0.6, palette = "jco", 
             rect = TRUE, rect_border = "jco", rect_fill = TRUE)

## water

dtAllWater.s
HiKmeans_water = hkmeans(dtAllWater.s,k=4, iter.max = 100 )

fviz_dend(HiKmeans_water, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

## sediment

dtAllSed.s
HiKmeans_Sed = hkmeans(dtAllSed.s,k=4, iter.max = 100 )

fviz_dend(HiKmeans_Sed, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)


###### DBSCAN clustering #####
db_Mean <- fpc::dbscan(dtMean.s, eps = 0.15, MinPts = 4)




######

pdf("Cluster_AllReplicatesSedData.pdf", width = 7,  height = 7)
grid.arrange(p.FCM_AllSed, p.kmeans_AllSed, 
             nrow = 1, ncol = 2)
dev.off()

pdf("Cluster_AllReplicatesWaterData.pdf", width = 7,  height = 7)
grid.arrange(p.FCM_AllWater, p.kmeans_AllWater, 
             nrow = 1, ncol = 2)
dev.off()


pdf("Comparison of clustering algorithms_Medians.pdf", width = 7,  height = 7)
grid.arrange(p.FCM_Med, p.kmeans_Median, 
             nrow = 1, ncol = 2)
dev.off()

pdf("Comparison of clustering algorithms_Mean.pdf", width = 7,  height = 7)
grid.arrange(p.FCM_Mean, p.kmeans_Mean, 
             nrow = 1, ncol = 2)
dev.off()




#### FINAL techniques tested ####

## FCM 
fviz_nbclust(dtMean.s, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

FCM_Mean = e1071::cmeans(dtMean.s, centers = 4, iter.max = 100, dist = "euclidean",
                         m = 2)

p.FCM_Mean = factoextra::fviz_cluster(list(data = dtMean.s, cluster=FCM_Mean$cluster), 
                                      ellipse.type = "norm",
                                      ellipse.level = .95,
                                      geom = c("text", "point"),
                                      palette = "jco",
                                      ggtheme = theme_economist(),
                                      show.clust.cent = F,
                                      main = "FuzzyCmeans - Mean")

factoextra::fviz_silhouette(FCM_Mean)


## K means clustering
dtMean.s
fviz_nbclust(dtMean.s,FUNcluster = kmeans, method = "wss")

kmeans_Mean<- kmeans(dtMean.s, 4, iter.max = 100, nstart = 100)
p.kmeans_Mean =  factoextra::fviz_cluster(list(data = dtMean.s, cluster=kmeans_Mean$cluster), 
                                          ellipse.type = "norm",
                                          ellipse.level = 0.95,
                                          palette = "jco",
                                          ggtheme = theme_economist(),
                                          main = "kmeans - with mean " )
factoextra::fviz_silhouette(kmeans_Mean)


### Hierarcichal k means

HiKmeans = factoextra::hkmeans(dtMean.s,k=4, iter.max = 100 )

p.HiKmeans = factoextra::fviz_cluster(HiKmeans, cex = 0.6, 
                                      ellipse.type = "norm",
                                      ellipse.level = 0.95,
                                      pallet = "jco",
                                      main = "Hierarchical kmeans -  with mean")

fviz_dend(HiKmeans, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

factoextra::fviz_silhouette(HiKmeans)


### PAM CLUSTERING 

PAM_Mean = cluster::pam(dtMean.s, 4, metric = "euclidean", stand = FALSE)

p.PAM_Mean =  factoextra::fviz_cluster(list(data = dtMean.s, cluster=PAM_Mean$cluster), 
                                       ellipse.type = "norm",
                                       ellipse.level = 0.95,
                                       palette = "jco",
                                       ggtheme = theme_economist(),
                                       main = "PAM clustering - with mean " )

factoextra::fviz_silhouette(PAM_Mean)
# Silhouette width of observations
sil <- res.hc$silinfo$widths[, 1:4]

# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]



pdf("Comparison of clustering algorithms_Mean.pdf", width = 7,  height = 7)
p.FCM_Mean
p.fanny_Mean
p.kmeans_Mean
p.PAM_Mean
p.HiKmeans
dev.off()


dtclusters = cbind(dtEnv, FCMCluster = FCM_All$cluster )
dtclusters = cbind(dtclusters, kmeansCluster = kmeans_All$cluster )
head(dtclusters)
dtclusters2 = dtclusters[,c(1,3, 28,29)]

