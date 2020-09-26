rm(list=ls())
# Load required libraries
library(haven)
library(dplyr)
library(ggplot2)
library(cluster)
library(reshape2)
setwd("~/Documents/Writing/Trump Supporter/")
# Load the data and choose required columns
cces_ts <- readRDS("Data/cces_2018_data.RDS")
# cces_ts <- read_dta(file = "./cces18_common_vv.dta")
# saveRDS(cces_ts,"./cces_2018_data.RDS")
cces <- cces_ts %>% select(caseid=caseid,
                           trumpapprove=CC18_308a,
                           birthyr,
                           gender,
                           sexuality,
                           educ,
                           race,
                           ideo5,
                           pew_churatd,
                           newsint,
                           union,
                           investor,
                           tvuse=CC18_300_2 ,
                           newspaper=CC18_300_3,
                           radiouse=CC18_300_4,
                           policespending=CC18_426_4,
                           # videoabtpolitics=CC18_300d_3,
                           # fwdpolitics=CC18_300d_5,
                           alwaysallowabortion=CC18_321a,
                           allowemployersdenyabortion=CC18_321d,
                           allabortionsillegal=CC18_321f,
                           borderspendingpluswall=CC18_322a,
                           daca=CC18_322b,
                           # legalimmigration=CC18_322c_new,
                           cutcorporatetax=CC18_325a,
                           medicareforall=CC18_327a,
                           repealobamacare=CC18_327c,
                           partialrepeallargeempl=CC18_327d,
                           selfideology=CC18_334A,
                           trumpideology=CC18_334C,
                           demideology=CC18_334D,
                           repideology=CC18_334E,
                           russiacollusion=CC18_335,
                           transban=CC18_417_d,
                           trumpapprovepost=CC18_app_dtrmp_post,
                           agreewhiteprivilege=CC18_422a,
                           womenunreasonablesexism=CC18_422c) %>% filter(!is.na(trumpapprove),!is.na(trumpapprovepost))
cces <- cces %>% filter(trumpapprove %in% c(1,2) | trumpapprovepost %in% c(1,2))
colSums(is.na(cces))
cces <- cces %>% na.exclude()
# Create a df for clustering, selecting only a few columns
cces_cluster <- cces %>% dplyr::select(caseid,trumpideology,russiacollusion,borderspendingpluswall,
                                       womenunreasonablesexism,cutcorporatetax,policespending) %>% mutate_all(as.character())
rownames(cces_cluster) <- cces$caseid
cces_cluster$caseid <- NULL
cces_cluster$trumpideology <- as_factor(cces_cluster$trumpideology)
cces_cluster$russiacollusion <- as_factor(cces_cluster$russiacollusion)
cces_cluster$borderspendingpluswall <- as_factor(cces_cluster$borderspendingpluswall)
cces_cluster$womenunreasonablesexism <- as_factor(cces_cluster$womenunreasonablesexism)
cces_cluster$cutcorporatetax <- as_factor(cces_cluster$cutcorporatetax)
cces_cluster$policespending <- as_factor(cces_cluster$policespending)

# My system crashes if I try to create a gower dissimilarity matrix with 22k respondents.
# Longer run solution: selecting fewer variables. Short-run solution: sampling 10k rows and working with it.
seq <- 1:nrow(cces_cluster)
set.seed(124)
selectseq <- sample(x = seq,size = 10000,replace = T)
cces_cluster1 <- cces_cluster[selectseq,]
# creating distance matrix
distmatrix <- daisy(x = cces_cluster1,metric = "gower")
plot(hclust(distmatrix))
hc <- hclust(distmatrix)
cces_cluster1$ClusterAssigned <- cutree(hc,k=5)
#Trying to profile these clusters created
table(cces_cluster1$policespending,cces_cluster1$ClusterAssigned)