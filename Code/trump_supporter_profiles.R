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
                           # birthyr,
                           # gender,
                           # sexuality,
                           # educ,
                           # race,
                           # ideo5,
                           # pew_churatd,
                           # newsint,
                           # union,
                           # investor,
                           # tvuse=CC18_300_2 ,
                           # newspaper=CC18_300_3,
                           # radiouse=CC18_300_4,
                           policespending=CC18_426_4,
                           # videoabtpolitics=CC18_300d_3,
                           # fwdpolitics=CC18_300d_5,
                           alwaysallowabortion=CC18_321a,
                           exceptionabortion=CC18_321b,
                           allowemployersdenyabortion=CC18_321d,
                           allabortionsillegal=CC18_321f,
                           borderspendingpluswall=CC18_322a,
                           daca=CC18_322b,
                           reducelegalimmigration=CC18_322c_new,
                           dacawallandreducelegal=CC18_322d_new,
                           reentryprison=CC18_322f,
                           cutcorporatetax=CC18_325a,
                           cuttaxeslt500k=CC18_325e_new,
                           cuttaxesgt500k=CC18_325f_new,
                           medicareforall=CC18_327a,
                           repealobamacare=CC18_327c,
                           partialrepeallargeempl=CC18_327d,
                           chinatariff=CC18_331a,
                           allowkeystone=CC18_332b,
                           withdrawparis=CC18_332c,
                           withdrawtpp = CC18_332e,
                           selfideology=CC18_334A,
                           trumpideology=CC18_334C,
                           demideology=CC18_334D,
                           repideology=CC18_334E,
                           russiacollusion=CC18_335,
                           epaco2=CC18_415a,
                           environprot = CC18_415d,
                           muslimban=CC18_417_c,
                           blackswithoutfavours=CC18_422e,
                           blacksgottenlessthandeserve=CC18_422g,
                           blackstryharder=CC18_422h,
                           whiteprivilege=CC18_422a,
                           racismrare = CC18_422b,
                           feministreasonable=CC18_422d,
                           welfarespend=CC18_426_1,
                           # pew_bornagain,
                           banassault=CC18_320c,
                           backgroundchecks=CC18_320a,
                           easierconcealed=CC18_320d,
                           transban=CC18_417_d,
                           trumpapprovepost=CC18_app_dtrmp_post,
                           womenunreasonablesexism=CC18_422c) %>% filter(!is.na(trumpapprove),!is.na(trumpapprovepost))
cces <- cces %>% filter(trumpapprove %in% c(1,2))
colSums(is.na(cces))
cces <- cces %>% na.exclude()
ggplot(cces,aes(x=trumpideology - repideology))+ geom_histogram(aes(y=..density..))
# Create a df for clustering, selecting only a few columns
cces_cluster <- cces %>% dplyr::select(-trumpapprove) %>%  #dplyr::select(caseid,selfideology,borderspendingpluswall,cutcorporatetax) %>% 
  mutate_all(as.character())
rownames(cces_cluster) <- cces$caseid
cces_cluster$caseid <- NULL
for(i in 1:ncol(cces_cluster))
{
  cces_cluster[,i] <- as_factor(cces_cluster[,i])
}

# My system crashes if I try to create a gower dissimilarity matrix with 22k respondents.
# Longer run solution: selecting fewer variables. Short-run solution: sampling 10k rows and working with it.
seq <- 1:nrow(cces_cluster)
set.seed(124)
selectseq <- sample(x = seq,size = 10000,replace = T)
cces_cluster1 <- cces_cluster[selectseq,]
# creating distance matrix
distmatrix <- daisy(x = cces_cluster1,metric = "gower")
plot(hclust(distmatrix,method = "complete"))
hc <- hclust(distmatrix,method="complete")
cces_cluster1$caseid <- rownames(cces_cluster1)
cces_cluster1$ClusterAssigned <- cutree(hc,k=7)
#Trying to profile these clusters created
clusterideolog <- cces_cluster1 %>% group_by(ClusterAssigned,selfideology) %>% dplyr::summarise(Responses=length(unique(caseid)))
clusterideolog$ResponsesTotal <- ave(clusterideolog$Responses,clusterideolog$ClusterAssigned,FUN=sum)
clusterideolog$ResponsePC <- clusterideolog$Responses/clusterideolog$ResponsesTotal
clusterideolog <- clusterideolog %>% dplyr::select(selfideology,ClusterAssigned,ResponsePC) %>% 
  dcast(selfideology~ClusterAssigned)
table(cces_cluster1$,cces_cluster1$ClusterAssigned)
