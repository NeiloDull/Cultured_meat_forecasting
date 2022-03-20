#Clear R
rm(list=ls())
library(readstata13)
library(corrplot)
CM <- read.dta13("~/Downloads/CM_Data_corr.dta")


CM2 <- CM[,-1]
rownames(CM2) <- CM[,1]
names(CM2) = c(cow_100_2031 = "100K cow-meat 2031",
               cow_100_2036 = "100K cow-meat 2036",
               cow_100_2031_10bucks ="100K $10 cow-meat 2031",
               cow_100_2036_10bucks ="100K $10 cow-meat 2036",
               meat_100_2031 ="100K 2031",
               meat_100_2031_10bucks ="100K $10/kg 2031",
               meat_100_2036 ="100K 2036",
               meat_100_2051 ="100K 2051",
               meat_1m_2031 ="1M 2031",
               meat_1m_2036 ="1M 2036",
               meat_1m_2051 ="1M 2051",
               meat_10m_2036 ="10M 2036",
               meat_10m_2051 ="10M 2051",
               meat_50m_2051 ="50M 2051",
               protest ="Protest",
               survey ="willing to try",
               funding_biotech ="Biotech VC funding",
               funding_us ="US funding",
               funding_eu ="EU funding",
               funding_china ="China funding",
               researchers =">250 researchers",
               eaa_plant ="Better than plant-based",
               eaa_op ="Better than animal welfare",
               growth_factors ="Growth factors",
               amino_acids ="Amino acids",
               discounters ="Discount retail",
               quickservice ="QSRs")

res <- cor(CM2, method = c("spearman"))
round(res, 2)
cor(CM2, use = "complete.obs")
library("Hmisc")
res2 <- rcorr(as.matrix(CM2))
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
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
library(Hmisc)
res2<-rcorr(as.matrix(CM2[,1:28]))
flattenCorrMatrix(res2$r, res2$P)

symnum(res, abbr.colnames = FALSE)

library(corrplot)
corrplot(res, type = "lower",  group.order=c("100K cow-meat 2031",
                                             "100K cow-meat 2036",
                                             "100K $10 cow-meat 2031",
                                             "100K $10 cow-meat 2036",
                                             "100K 2031",
                                             "100K $10/kg 2031",
                                             "100K 2036",
                                             "100K 2051",
                                             "1M 2031",
                                             "1M 2036",
                                             "1M 2051",
                                             "10M 2036",
                                             "10M 2051",
                                             "50M 2051",
                                             "Protest",
                                             "willing to try",
                                             "Biotech VC funding",
                                             "US funding",
                                             "EU funding",
                                             "China funding",
                                             ">250 researchers",
                                             "Better than plant-based",
                                             "Better than animal welfare",
                                             "Growth factors",
                                             "Amino acids",
                                             "Discount retail",
                                             "QSRs"
),
tl.col = "black", tl.srt = 45)



# Insignificant correlations are leaved blank
corrplot(res2$r,method = 'ellipse',
         type="lower", group.order=c("100K cow-meat 2031",
                                     "100K cow-meat 2036",
                                     "100K $10 cow-meat 2031",
                                     "100K $10 cow-meat 2036",
                                     "100K 2031",
                                     "100K $10/kg 2031",
                                     "100K 2036",
                                     "100K 2051",
                                     "1M 2031",
                                     "1M 2036",
                                     "1M 2051",
                                     "10M 2036",
                                     "10M 2051",
                                     "50M 2051",
                                     "Protest",
                                     "willing to try",
                                     "Biotech VC funding",
                                     "US funding",
                                     "EU funding",
                                     "China funding",
                                     ">250 researchers",
                                     "Better than plant-based",
                                     "Better than animal welfare",
                                     "Growth factors",
                                     "Amino acids",
                                     "Discount retail",
                                     "QSRs"
         ),
         tl.cex = 0.75,
         tl.col = 'black',  cl.ratio = 0.2, tl.srt = 90, col = COL2('PuOr', 10),
         p.mat = res2$P, sig.level = 0.01, insig = "blank", )

###
cor_5 <- rcorr(as.matrix(CM2))
M <- cor_5$r
p_mat <- cor_5$P

corrplot(M, type = "lower", group.order=c("100K cow-meat 2031",
                                          "100K cow-meat 2036",
                                          "100K $10 cow-meat 2031",
                                          "100K $10 cow-meat 2036",
                                          "100K 2031",
                                          "100K $10/kg 2031",
                                          "100K 2036",
                                          "100K 2051",
                                          "1M 2031",
                                          "1M 2036",
                                          "1M 2051",
                                          "10M 2036",
                                          "10M 2051",
                                          "50M 2051",
                                          "Protest",
                                          "willing to try",
                                          "Biotech VC funding",
                                          "US funding",
                                          "EU funding",
                                          "China funding",
                                          ">250 researchers",
                                          "Better than plant-based",
                                          "Better than animal welfare",
                                          "Growth factors",
                                          "Amino acids",
                                          "Discount retail",
                                          "QSRs"
),
p.mat = p_mat, sig.level = 0.05, insig = "blank",
tl.cex = 0.75,tl.col = 'black',col = COL2('PuOr', 10))

