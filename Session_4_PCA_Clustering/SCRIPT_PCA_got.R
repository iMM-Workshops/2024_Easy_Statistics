# go get english on my computer
Sys.setenv(LANG='en')

# packages loading --------------------------------------------------------
library(Rcmdr)
library(FactoMineR)
library(RcmdrPlugin.FactoMineR)

# data importation --------------------------------------------------------
got<-read.csv2("data_PCA_got.csv",row.names = "Name",stringsAsFactors = TRUE)
got

summary(got)

# PCA ---------------------------------------------------------------------
got.PCA<-got[, c("Nb_chp_narr", "Gender", "Married", "Age", 
                 "Nb_death_relationships", "Popularity")]

#PCA 
res<-PCA(got.PCA, scale.unit=TRUE, ncp=5, graph = 
           FALSE)
      #individual cloud
      plot(res,choix="ind")
      #variable cloud
      plot(res,choix="var")
      
      summary(res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")

#help to analyse the dimensions
      dimdesc(res, axes = 1:3, proba = 0.05)      

# clustering --------------------------------------------------------------
#clutering and plots from HCPC
res.hcpc<-HCPC(res ,nb.clust=-1,consol=FALSE,min=3,max=10,graph=TRUE)

  plot(res.hcpc)
  plot(res.hcpc,choice="tree")

# approach with factoshiny ------------------------------------------------

  #Factoshiny
library(Factoshiny)

  #directly on the dataset
RES<-PCAshiny(as.data.frame(got.PCA))
RES

RES<-PCAshiny(as.data.frame(got))
RES

  #on the results of a pca
resshiny = PCAshiny(res)


