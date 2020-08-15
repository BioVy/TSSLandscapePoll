library(dplyr)
library(ade4)
library(FactoMineR)
library(factoextra)

dtEnv= read.csv("MetalsInAbEnv.csv", header = TRUE)
str(dtEnv)

colnames(dtEnv) = c("Riv","Reg", "RepID", "HgS", "CdS", "PbS","CrS","CoS","NiS", "CuS", "ZnS","AsS", 
                    "CdW", "PbW", "CrW", "CoW", "NiW", "CuW", "ZnW", "AsW",
                    "Clay", "OM", "pH", "Temp", "Cond", "O2", "Hardness")
head(dtEnv)

#1____fuzzy PCA on categorical variables__________ #####
#### prepare data to do fuzzy on categorical variables #####
x = dtEnv[,1:20]

dHgS = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(HgS<=quantile(dtEnv$HgS, 0.25)),
            M = sum(HgS>quantile(dtEnv$HgS, 0.25) & HgS<=quantile(dtEnv$HgS, 0.50)),
            H = sum(HgS>quantile(dtEnv$HgS, 0.50) & HgS<=quantile(dtEnv$HgS, 0.75)),
            VH = sum(HgS>quantile(dtEnv$HgS, 0.75)))

dHgS = data.frame(dHgS)
colnames(dHgS) = c("Reg", "Riv", "HgS_L", "HgS_M", "HgS_H", "HgS_VH" )

dCdS = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(CdS<=quantile(dtEnv$CdS, 0.25)),
            M = sum(CdS>quantile(dtEnv$CdS, 0.25) & CdS<=quantile(dtEnv$CdS, 0.50)),
            H = sum(CdS>quantile(dtEnv$CdS, 0.50) & CdS<=quantile(dtEnv$CdS, 0.75)),
            VH = sum(CdS>quantile(dtEnv$CdS, 0.75)))

dCdS = data.frame(dCdS)
colnames(dCdS) = c("Reg", "Riv", "CdS_L", "CdS_M", "CdS_H", "CdS_VH" )


dPbS = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(PbS<=quantile(dtEnv$PbS, 0.25)),
            M = sum(PbS>quantile(dtEnv$PbS, 0.25) & PbS<=quantile(dtEnv$PbS, 0.50)),
            H = sum(PbS>quantile(dtEnv$PbS, 0.50) & PbS<=quantile(dtEnv$PbS, 0.75)),
            VH = sum(PbS>quantile(dtEnv$PbS, 0.75)))

PbS = data.frame(dPbS)
colnames(dPbS) = c("Reg", "Riv", "PbS_L", "PbS_M", "PbS_H", "PbS_VH" )


dCrS = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(CrS<=quantile(dtEnv$CrS, 0.25)),
            M = sum(CrS>quantile(dtEnv$CrS, 0.25) & CrS<=quantile(dtEnv$CrS, 0.50)),
            H = sum(CrS>quantile(dtEnv$CrS, 0.50) & CrS<=quantile(dtEnv$CrS, 0.75)),
            VH = sum(CrS>quantile(dtEnv$CrS, 0.75)))

dCrS = data.frame(dCrS)
colnames(dCrS) = c("Reg", "Riv", "CrS_L", "CrS_M", "CrS_H", "CrS_VH" )


dCoS = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(CoS<=quantile(dtEnv$CoS, 0.25)),
            M = sum(CoS>quantile(dtEnv$CoS, 0.25) & CoS<=quantile(dtEnv$CoS, 0.50)),
            H = sum(CoS>quantile(dtEnv$CoS, 0.50) & CoS<=quantile(dtEnv$CoS, 0.75)),
            VH = sum(CoS>quantile(dtEnv$CoS, 0.75)))

dCoS = data.frame(dCoS)
colnames(dCoS) = c("Reg", "Riv", "CoS_L", "CoS_M", "CoS_H", "CoS_VH" )


dNiS = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(NiS<=quantile(dtEnv$NiS, 0.25)),
            M = sum(NiS>quantile(dtEnv$NiS, 0.25) & NiS<=quantile(dtEnv$NiS, 0.50)),
            H = sum(NiS>quantile(dtEnv$NiS, 0.50) & NiS<=quantile(dtEnv$NiS, 0.75)),
            VH = sum(NiS>quantile(dtEnv$NiS, 0.75)))

dNiS = data.frame(dNiS)
colnames(dNiS) = c("Reg", "Riv", "NiS_L", "NiS_M", "NiS_H", "NiS_VH" )


dCuS = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(CuS<=quantile(dtEnv$CuS, 0.25)),
            M = sum(CuS>quantile(dtEnv$CuS, 0.25) & CuS<=quantile(dtEnv$CuS, 0.50)),
            H = sum(CuS>quantile(dtEnv$CuS, 0.50) & CuS<=quantile(dtEnv$CuS, 0.75)),
            VH = sum(CuS>quantile(dtEnv$CuS, 0.75)))

dCuS = data.frame(dCuS)
colnames(dCuS) = c("Reg", "Riv", "CuS_L", "CuS_M", "CuS_H", "CuS_VH" )


dZnS = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(ZnS<=quantile(dtEnv$ZnS, 0.25)),
            M = sum(ZnS>quantile(dtEnv$ZnS, 0.25) & ZnS<=quantile(dtEnv$ZnS, 0.50)),
            H = sum(ZnS>quantile(dtEnv$ZnS, 0.50) & ZnS<=quantile(dtEnv$ZnS, 0.75)),
            VH = sum(ZnS>quantile(dtEnv$ZnS, 0.75)))

dZnS = data.frame(dZnS)
colnames(dZnS) = c("Reg", "Riv", "ZnS_L", "ZnS_M", "ZnS_H", "ZnS_VH" )


dAsS = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(AsS<=quantile(dtEnv$AsS, 0.25)),
            M = sum(AsS>quantile(dtEnv$AsS, 0.25) & AsS<=quantile(dtEnv$AsS, 0.50)),
            H = sum(AsS>quantile(dtEnv$AsS, 0.50) & AsS<=quantile(dtEnv$AsS, 0.75)),
            VH = sum(AsS>quantile(dtEnv$AsS, 0.75)))

dAsS = data.frame(dAsS)
colnames(dAsS) = c("Reg", "Riv", "AsS_L", "AsS_M", "AsS_H", "AsS_VH" )


dCdW = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(CdW<=quantile(dtEnv$CdW, 0.25)),
            M = sum(CdW>quantile(dtEnv$CdW, 0.25) & CdW<=quantile(dtEnv$CdW, 0.50)),
            H = sum(CdW>quantile(dtEnv$CdW, 0.50) & CdW<=quantile(dtEnv$CdW, 0.75)),
            VH = sum(CdW>quantile(dtEnv$CdW, 0.75)))

dCdW = data.frame(dCdW)
colnames(dCdW) = c("Reg", "Riv", "CdW_L", "CdW_M", "CdW_H", "CdW_VH" )


dPbW = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(PbW<=quantile(dtEnv$PbW, 0.25)),
            M = sum(PbW>quantile(dtEnv$PbW, 0.25) & PbW<=quantile(dtEnv$PbW, 0.50)),
            H = sum(PbW>quantile(dtEnv$PbW, 0.50) & PbW<=quantile(dtEnv$PbW, 0.75)),
            VH = sum(PbW>quantile(dtEnv$PbW, 0.75)))

dPbW = data.frame(dPbW)
colnames(dPbW) = c("Reg", "Riv", "PbW_L", "PbW_M", "PbW_H", "PbW_VH" )


dCrW = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(CrW<=quantile(dtEnv$CrW, 0.25)),
            M = sum(CrW>quantile(dtEnv$CrW, 0.25) & CrW<=quantile(dtEnv$CrW, 0.50)),
            H = sum(CrW>quantile(dtEnv$CrW, 0.50) & CrW<=quantile(dtEnv$CrW, 0.75)),
            VH = sum(CrW>quantile(dtEnv$CrW, 0.75)))

dCrW = data.frame(dCrW)
colnames(dCrW) = c("Reg", "Riv", "CrW_L", "CrW_M", "CrW_H", "CrW_VH" )


dCoW = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(CoW<=quantile(dtEnv$CoW, 0.25)),
            M = sum(CoW>quantile(dtEnv$CoW, 0.25) & CoW<=quantile(dtEnv$CoW, 0.50)),
            H = sum(CoW>quantile(dtEnv$CoW, 0.50) & CoW<=quantile(dtEnv$CoW, 0.75)),
            VH = sum(CoW>quantile(dtEnv$CoW, 0.75)))

dCoW = data.frame(dCoW)
colnames(dCoW) = c("Reg", "Riv", "CoW_L", "CoW_M", "CoW_H", "CoW_VH" )


dNiW = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(NiW<=quantile(dtEnv$NiW, 0.25)),
            M = sum(NiW>quantile(dtEnv$NiW, 0.25) & NiW<=quantile(dtEnv$NiW, 0.50)),
            H = sum(NiW>quantile(dtEnv$NiW, 0.50) & NiW<=quantile(dtEnv$NiW, 0.75)),
            VH = sum(NiW>quantile(dtEnv$NiW, 0.75)))

dNiW = data.frame(dNiW)
colnames(dNiW) = c("Reg", "Riv", "NiW_L", "NiW_M", "NiW_H", "NiW_VH" )


dCuW = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(CuW<=quantile(dtEnv$CuW, 0.25)),
            M = sum(CuW>quantile(dtEnv$CuW, 0.25) & CuW<=quantile(dtEnv$CuW, 0.50)),
            H = sum(CuW>quantile(dtEnv$CuW, 0.50) & CuW<=quantile(dtEnv$CuW, 0.75)),
            VH = sum(CuW>quantile(dtEnv$CuW, 0.75)))

dCuW = data.frame(dCuW)
colnames(dCuW) = c("Reg", "Riv", "CuW_L", "CuW_M", "CuW_H", "CuW_VH" )


dZnW = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(ZnW<=quantile(dtEnv$ZnW, 0.25)),
            M = sum(ZnW>quantile(dtEnv$ZnW, 0.25) & ZnW<=quantile(dtEnv$ZnW, 0.50)),
            H = sum(ZnW>quantile(dtEnv$ZnW, 0.50) & ZnW<=quantile(dtEnv$ZnW, 0.75)),
            VH = sum(ZnW>quantile(dtEnv$ZnW, 0.75)))

dZnW = data.frame(dZnW)
colnames(dZnW) = c("Reg", "Riv", "ZnW_L", "ZnW_M", "ZnW_H", "ZnW_VH" )

dAsW = x %>%
  group_by(Reg,Riv)%>%
  summarise(L = sum(AsW<=quantile(dtEnv$AsW, 0.25)),
            M = sum(AsW>quantile(dtEnv$AsW, 0.25) & AsW<=quantile(dtEnv$AsW, 0.50)),
            H = sum(AsW>quantile(dtEnv$AsW, 0.50) & AsW<=quantile(dtEnv$AsW, 0.75)),
            VH = sum(AsW>quantile(dtEnv$AsW, 0.75)))

dAsW = data.frame(dAsW)
colnames(dAsW) = c("Reg", "Riv", "AsW_L", "AsW_M", "AsW_H", "AsW_VH" )

dtx = do.call("cbind", list(dHgS,dCdS[,3:6],dPbS[,3:6],dCrS[,3:6],dCoS[,3:6],dNiS[,3:6], dCuS[,3:6],
                            dZnS[,3:6], dAsS[,3:6], dCdW[,3:6], dPbW[,3:6], dCrW[,3:6],dCoW[,3:6],
                            dNiW[,3:6], dCuW[,3:6], dZnW[,3:6], dAsW[,3:6]))

##### pca and clustering on fuzzy coded data ####
dim(dtx)
dtx2 = dtx[,2:70]
dtx3 = textshape::column_to_rownames(dtx2, loc = 1) #use rivernames as rownames

Var.names = names(dtEnv[,4:20])
Riv.names = unique(dtEnv$Riv)
var.blocks = c(HgS = 4, CdS = 4, PbS = 4, CrS = 4, CoS = 4, NiS = 4, CuS = 4,
                   ZnS = 4, AsS = 4, CdW = 4, PbW = 4, CrW = 4, CoW = 4, NiW = 4, 
                   CuW = 4, ZnW = 4, AsW = 4)


dtx4 = list(Riv.names = Riv.names,
            metaldata = dtx3,
            var.blocks = var.blocks, 
            var.names =  Var.names )

FuzzyCodD1 = prep.fuzzy.var(dtx3, col.blocks = var.blocks) 
attributes(FuzzyCodD1)
FuzzyCodD1.s = scale(FuzzyCodD1)


##
fpca1 = dudi.fpca(FuzzyCodD1, scann = F, nf = 5)
summary(fpca1)
fpca1$co
score(fpca1)
fviz_screeplot(fpca1, addlabels = TRUE )
s.label(fpca1$li, 
        xax = 1,     # Dimension 1
        yax = 2)  

fviz_nbclust(fpca1$tab, kmeans, method = "wss",
             nboot=100, k.max = 20) +
  labs(subtitle = "") # to see how many clusters I need

Clust_cod1 = kmeans(fpca1$tab, 4, iter.max = 100, nstart = 100)
class(Clust_cod1)
saveRDS(Clust_cod1, "kmeansClustering.RData", )

pca1_ind = fviz_pca_ind(fpca1, repel = F,
         axes =  c(1,2),
         habillage=Clust_cod1$cluster,
         col.var = "#2E9FDF", 
         addEllipses = F,
         label = "all",
         title ="fuzzy pca - replicates assigned to pollution categories")+
  theme(legend.position = "top")

pca1_var = fviz_pca_var(fpca1, repel = F,
             axes =  c(1,2),
             col.var = "#2E9FDF", 
             addEllipses = F,
             label = "all",
             title ="fuzzy pca - replicates assigned to pollution categories")+
  theme_bw()

jpeg(file="Figures/FuzzyPCA1.jpeg", res=150, width = 1800, height = 800, units='px')
gridExtra::grid.arrange(
  pca1_ind,
  pca1_var, ncol = 2, nrow = 1
)
dev.off()





###################################################
#2____fuzzy PCA on continuous variables (values of 5 replicates per site)__________  #######
# prepare data: change the format of the dataframe####
names(dtEnv)
head(dtEnv[,1:20])
dtMP.L = melt(dtEnv[,1:20], id.vars=c("Reg","Riv","RepID" ))# convert from wide to long 
dim(dtMP.L)
levels(dtMP.L$variable)
dtMP.L$ID = rep(rep(c(1:5),21),17)
dtMP.L$VariableID = paste(dtMP.L$variable,dtMP.L$ID, sep ="_")
head(dtMP.L)

dtMP.Wid =dcast(dtMP.L, Reg + Riv  ~ VariableID, value.var="value")# convert back to wide format
dim(dtMP.Wid)
head(dtMP.Wid[1:21,1:6])
dtMP.Wid2 = dtMP.Wid[2:87]

dtMP.Wid2 = textshape::column_to_rownames(dtMP.Wid2, loc = 1) #use rivernames as rownames
names(dtMP.Wid2)
head(dtMP.Wid2)
dim(dtMP.Wid2)


### prepare fuzzy coded dataframe ####
MVar = c(AsS = 5, AsW = 5, CdS = 5, CdW = 5, CoS = 5, CoW = 5, CrS = 5,
               CrW = 5, CuS = 5, CuW = 5, HgS = 5, NiS = 5, NiW = 5, PbS = 5, 
               PbW = 5, ZnS = 5, ZnW = 5)

FuzzyCodD2 = prep.fuzzy.var(dtMP.Wid2, col.blocks = MVar, row.w = rep(1, nrow(dtMP.Wid2))) 
attributes(FuzzyCodD2)


## PCA and kmeans clustering ####
fpca2 = dudi.fpca(FuzzyCodD2, scann = F, nf = 3)
summary(fpca2)
fpca2$tab

s.label(fpca2$li, 
        xax = 1,     # Dimension 1
        yax = 2)  


fviz_nbclust(fpca2$tab, kmeans, method = "wss",
             nboot=100, k.max = 20) +
  labs(subtitle = "") # to see how many clusters I need

Clust_cod2 = kmeans(fpca2$tab[,1:8], 8, iter.max = 100, nstart = 100)


fviz_pca_ind(fpca2, repel = F,
             axes =  c(1,2),
             habillage=Clust_cod2$cluster,
             col.var = "#2E9FDF", 
             addEllipses = F,
             label = "all",
             title ="fuzzy pca - on continuous data (replicate values)")+
  theme_bw()













