# CorrPlot Script
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
#http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software#compute-correlation-matrix

#Load Libraries
library(corrplot)
library(readxl)
library(RColorBrewer)


#Import Data
Page15Data <- read_excel("/Users/tylergimple/Downloads/Shared With Tyler/Correlation Project/Base Data.xlsx")
View(Page15Data)

#Changing Row Names
  sampleNames <- Page15Data[[1]]
  DataFile <- Page15Data[,2:ncol(Page15Data)]
  row.names(DataFile) <- sampleNames
  View(DataFile)
  Data_File.M <- as.matrix(DataFile)

  
#Perform Correlation, Calcuate pvalues
library("Hmisc")  
Hmisc.Correlation.Matrix <- rcorr(Data_File.M, type = "pearson")
Significance <- Hmisc.Correlation.Matrix$P
View(Significance)

#Export Files
write.table(Hmisc.Correlation.Matrix$r, file="Hmisc.correlation.txt", sep = "\t")

#Rearrange the table so that variables are side-by-side
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
#Create/Print Flattend Table
Flattend_Matric<-flattenCorrMatrix(Hmisc.Correlation.Matrix$r, Hmisc.Correlation.Matrix$P)
write.table(Flattend_Matric,file="FlatHmisc.correlation.txt", sep = "\t")
