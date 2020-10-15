rm(list=ls())
# Load required libraries
library(haven)
library(dplyr)
library(ggplot2)
library(cluster)
library(reshape2)
library(mclust)
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
                           # trumpideology=CC18_334C,
                           # demideology=CC18_334D,
                           # repideology=CC18_334E,
                           russiacollusion=CC18_335,
                           epaco2=CC18_415a,
                           environprot = CC18_415d,
                           finchoiceact = CC18_416,
                           repealcleanpower=CC18_417_a,
                           withdrawirandeal=CC18_417_b,
                           contactpublicoff = CC18_417a_5,
                           muslimban=CC18_417_c,
                           blackswithoutfavours=CC18_422e,
                           blacksgottenlessthandeserve=CC18_422g,
                           blackstryharder=CC18_422h,
                           whiteprivilege=CC18_422a,
                           racismrare = CC18_422b,
                           feministreasonable=CC18_422d,
                           welfarespend=CC18_426_1,
                           healthcarespend=CC18_426_2,
                           educationspend = CC18_426_3,
                           # pew_bornagain,
                           banassault=CC18_320c,
                           backgroundchecks=CC18_320a,
                           easierconcealed=CC18_320d,
                           transban=CC18_417_d,
                           trumpapprovepost=CC18_app_dtrmp_post,
                           womenunreasonablesexism=CC18_422c) %>% filter(!is.na(trumpapprove),!is.na(trumpapprovepost))
cces <- cces %>% filter(trumpapprove == 2)
colSums(is.na(cces))
cces <- cces %>% na.exclude()
# ggplot(cces,aes(x=trumpideology - repideology))+ geom_histogram(aes(y=..density..))
# Create a df for clustering, selecting only a few columns
cces_cluster <- cces %>% dplyr::select(-trumpapprove) %>%  #dplyr::select(caseid,selfideology,borderspendingpluswall,cutcorporatetax) %>% 
  mutate_all(as.character())
caseids <- cces$caseid
cces_cluster$caseid <- NULL
for(i in 1:ncol(cces_cluster))
{
  cces_cluster[,i] <- as_factor(cces_cluster[,i])
}

cces_cluster1 <- cces_cluster
# creating distance matrix
distmatrix <- daisy(x = cces_cluster1,metric = "gower")
set.seed(423)
plot(hclust(distmatrix,method = "complete"))
hc <- hclust(distmatrix,method="complete")
cces_cluster1$caseid <- caseids
cces_cluster1$ClusterAssigned <- cutree(hc,k=4)
cces_cluster1 %>% group_by(ClusterAssigned) %>% dplyr::summarise(Responses = length(unique(caseid))) %>% 
  mutate(ResponsesPC=Responses/sum(Responses))
# CCES Profile variables
cces_profile <- cces_ts %>% select(caseid=caseid,
                           birthyr,
                           gender,
                           sexuality,
                           region,
                           edloan,
                           employ,
                           educ,
                           race,
                           ideo5,
                           numchildren,
                           pew_churatd,
                           pew_bornagain,
                           pew_prayer,
                           internethome,
                           internetwork,
                           pid7,
                           urbancity,
                           nationaleconomy=CC18_301,
                           faminc_new,
                           newsint,
                           postedabtpolitics = CC18_300d_1,
                           householdincomelastyear=CC18_302,
                           union,
                           investor,
                           tvuse=CC18_300_2 ,
                           newspaper=CC18_300_3,
                           radiouse=CC18_300_4,
                           voted2018midterm=CC18_401)
for(i in 1:ncol(cces_profile))
{
  cces_profile[,i] <- as_factor(cces_profile[,i])
}

cces_cluster2 <- cces_cluster1 %>% mutate(caseid=as.numeric(caseid)) %>% left_join(cces_profile)

cols <- colnames(cces_cluster2)
cols <- cols[!cols %in% c("caseid","ClusterAssigned")]
cluster_summary <- function(df,clusterNo)
{
  clust <- df %>% filter(ClusterAssigned == clusterNo)
  responses <- data.frame(stringsAsFactors = F)
  for(col in cols)
  {
    t <- data.frame(table(clust[col]),stringsAsFactors = F) %>% mutate(FreqPC = Freq/sum(Freq),
                                                                       Question = col)
    responses <- rbind(responses,t)
  }
  return(responses)
}

caseids <- cces_cluster2$caseid
distmatrix <- daisy(x = cces_cluster2[,-45],metric = "gower")
set.seed(423)
plot(hclust(distmatrix,method = "complete"))
hc <- hclust(distmatrix,method="complete")
cces_cluster2$caseid <- caseids
cces_cluster2$ClusterAssigned <- cutree(hc,k=3)
cces_cluster2 %>% group_by(ClusterAssigned) %>% dplyr::summarise(Responses = length(unique(caseid))) %>% 
  mutate(ResponsesPC=Responses/sum(Responses))

clust1 <- cluster_summary(cces_cluster2,clusterNo = 1) %>% rename(C1Freq=Freq,C1FreqPC=FreqPC)
clust2 <- cluster_summary(cces_cluster2,clusterNo = 2) %>% rename(C2Freq=Freq,C2FreqPC=FreqPC)
clust3 <- cluster_summary(cces_cluster2,clusterNo = 3) %>% rename(C3Freq=Freq,C3FreqPC=FreqPC)
clust4 <- cluster_summary(cces_cluster2,clusterNo = 4) %>% rename(C4Freq=Freq,C4FreqPC=FreqPC)

clusters <- clust1 %>% full_join(clust2) %>% full_join(clust3)# %>% full_join(clust4)

# analysing & visualising the entire base first
c1 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==1),aes(x=faminc_new)) + geom_histogram(stat="count")
c2 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==2),aes(x=faminc_new)) + geom_histogram(stat="count")
c3 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==3),aes(x=faminc_new)) + geom_histogram(stat="count")
# c4 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==4),aes(x=faminc_new)) + geom_histogram(stat="count")
gridExtra::grid.arrange(c1,c2,c3,c4,ncol=1)
