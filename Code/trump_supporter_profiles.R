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
                           policespending=CC18_426_4,
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
# set.seed(72)
distmatrix <- daisy(x = cces_cluster1,metric = "gower")
plot(hclust(distmatrix,method = "complete"))
set.seed(72)
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
                           demideology=CC18_334D,
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

clust1 <- cluster_summary(cces_cluster2,clusterNo = 1) %>% rename(C1Freq=Freq,TrueBelievers=FreqPC)
clust2 <- cluster_summary(cces_cluster2,clusterNo = 2) %>% rename(C2Freq=Freq,Conservatives=FreqPC)
clust3 <- cluster_summary(cces_cluster2,clusterNo = 3) %>% rename(C3Freq=Freq,SocialConservMod=FreqPC)
clust4 <- cluster_summary(cces_cluster2,clusterNo = 4) %>% rename(C4Freq=Freq,LowPartisanMod=FreqPC)

clusters <- clust1 %>% full_join(clust2) %>% full_join(clust3) %>% full_join(clust4)


# Self-reported ideology
clust <- clusters %>% filter(Question == "selfideology") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                           LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
library(showtext)
ggplot(clust,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Very Liberal" = "#2196f3","Liberal"="#a1a3e9","Somewhat Liberal"="#d0b9df",
                               "Middle of the Road" = "#e1d7de","Somewhat Conservative" = "#f1a8c3","Conservative"="#fe7589",
                               "Very Conservative" = "#f44336","Not sure" = "#788385","skipped"="dark grey",
                               "not asked" = "medium grey"),
                    name = "Ideology")+  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                            breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Did he collude with Russia?
clustcoll <- clusters %>% filter(Question == "russiacollusion") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                              LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustcoll,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Yes" = "#2196f3",
                               "Not sure" = "dark grey","No" = "#f44336","skipped"="#e1d7de",
                               "not asked" = "steelblue"),
                    name = "Opinion on \nCollusion")+  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                            breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))
# News interest
clustnews <- clusters %>% filter(Question == "newsint") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                  LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustnews,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Most of the time" = "#003366","Some of the time"="#006299","Only now and then"="#0094c5",
                               "Hardly at all"="#00c9e8","DK"="#00ffff","skipped"="dark grey",
                               "not asked" = "steelblue"),name = "Political Interest")+
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Cut taxes on income brackets > $500k.
clusttaxrich <- clusters %>% filter(Question == "cuttaxesgt500k") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                             LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clusttaxrich,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Oppose" = "#2196f3","Support" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Cut Taxes \non >$500k")+  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                          breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Cut corporate taxes
clusttaxcorp <- clusters %>% filter(Question == "cutcorporatetax") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                    LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clusttaxcorp,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Oppose" = "#2196f3","Support" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Cut Corporate \nIncome Tax")+  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                         breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Medicare for all
clustmedicareforall <- clusters %>% filter(Question == "medicareforall") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                     LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustmedicareforall,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Oppose" = "#f44336","Support" = "#2196f3","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Medicare \nFor All")+  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                              breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Repeal Obamacare
clustobamacare <- clusters %>% filter(Question == "repealobamacare") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                           LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustobamacare,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Oppose" = "#2196f3","Support" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Repeal \nObamacare")+  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                      breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Withdrawal from Paris
clustparis <- clusters %>% filter(Question == "withdrawparis") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                       LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustparis,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Oppose" = "#2196f3","Support" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Withdraw from \nParis Agreement")+  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                      breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

#Strengthen EPA enforcement
clustepa <- clusters %>% filter(Question == "environprot") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                 LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustepa,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Support" = "#2196f3","Oppose" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Strengthen EPA \nEnforcement")+  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                                   breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Withdrawal from TPP
clusttpp <- clusters %>% filter(Question == "withdrawtpp") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                             LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clusttpp,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Oppose" = "#2196f3","Support" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Withdraw \nfrom TPP")+  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                       breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Border Wall
clustwall <- clusters %>% filter(Question == "borderspendingpluswall") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                             LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustwall,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Oppose" = "#2196f3","Support" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Increase Border \nSpending & Build Wall")+  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                       breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Reduce Legal immigration
clustlegal <- clusters %>% filter(Question == "reducelegalimmigration") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                         LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustlegal,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Oppose" = "#2196f3","Support" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Reduce Legal \nImmigration") +  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                                           breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Blacks should work hard without favours.
clustblackfavour <- clusters %>% filter(Question == "blackswithoutfavours") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                          LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustblackfavour,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Strongly disagree" = "#2196f3","Somewhat disagree"="#9CBBEC",
                               "Neither agree  nor disagree" = "#e2e2e2","Somewhat agree" = "#F89A89",
                               "Strongly  agree" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Blacks should work \nwithout favours") +  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                               breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Blacks should just work harder.
clustblackworkharder <- clusters %>% filter(Question == "blackstryharder") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                              LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustblackworkharder,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Strongly disagree" = "#2196f3","Somewhat disagree"="#9CBBEC",
                               "Neither agree  nor disagree" = "#e2e2e2","Somewhat agree" = "#F89A89",
                               "Strongly  agree" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Blacks should work \nwithout favours") +  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                                         breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# Blacks should just work harder.
clustblackstryharder <- clusters %>% filter(Question == "blackstryharder") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                             LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustblackstryharder,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Strongly disagree" = "#2196f3","Somewhat disagree"="#9CBBEC",
                               "Neither agree  nor disagree" = "#e2e2e2","Somewhat agree" = "#F89A89",
                               "Strongly  agree" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Blacks should \njust try harder") +  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                                         breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))


# Throw re-entry immigrants into prison
clustreentry <- clusters %>% filter(Question == "reentryprison") %>% dplyr::select(Var1,TrueBelievers,Conservatives,SocialConservMod,
                                                                                          LowPartisanMod) %>% 
  rename(Answer = Var1) %>% melt(id.vars="Answer") %>% rename(Cluster=variable)
ggplot(clustreentry,aes(x=Cluster,y=value, fill = as.factor(Answer))) + geom_col(width=0.5) + coord_flip()+
  scale_fill_manual(values = c("Oppose" = "#2196f3","Support" = "#f44336","skipped"="dark grey",
                               "not asked" = "steelblue"),
                    name = "Jail for \ndeport re-entry") +  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                                                                               breaks = seq(0,1,0.1))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="Montserrat"),
        axis.text = element_text(family="Montserrat"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

##### More hacky code ######
########################################################################################################################
########################################################################################################################

# analysing & visualising the entire base first
c1 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==1),aes(x=birthyr)) + geom_histogram(stat="count")+xlim(1920,2000)
c2 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==2),aes(x=birthyr)) + geom_histogram(stat="count")+xlim(1920,2000)
c3 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==3),aes(x=birthyr)) + geom_histogram(stat="count")+xlim(1920,2000)
c4 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==4),aes(x=birthyr)) + geom_histogram(stat="count")+xlim(1920,2000)
gridExtra::grid.arrange(c1,c2,c3,c4,ncol=1)
gridExtra::grid.arrange(c1,c2,c3,ncol=1)

f1 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==1,
                                      !is.na(faminc_new)),aes(x=faminc_new)) + geom_histogram(stat="count")
f2 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==2),aes(x=faminc_new)) + geom_histogram(stat="count")
f3 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==3),aes(x=faminc_new)) + geom_histogram(stat="count")
f4 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==4),aes(x=faminc_new)) + geom_histogram(stat="count")
gridExtra::grid.arrange(f1,f2,f3,f4,ncol=1)
gridExtra::grid.arrange(f1,f2,f3,ncol=1)

d1 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==1,
                                      !is.na(newsint)),aes(x=newsint)) + geom_histogram(stat="count")
d2 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==2,
                                      !is.na(newsint)),aes(x=newsint)) + geom_histogram(stat="count")
d3 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==3,
                                      !is.na(newsint)),aes(x=newsint)) + geom_histogram(stat="count")
d4 <- ggplot(cces_cluster2 %>% filter(ClusterAssigned==4,
                                      !is.na(newsint)),aes(x=newsint)) + geom_histogram(stat="count")
gridExtra::grid.arrange(d1,d2,d3,d4,ncol=1)

ide <- cces_cluster2 %>% group_by(ClusterAssigned,selfideology,demideology) %>% dplyr::summarise(Responses=length(unique(caseid)))
ggplot(ide,aes(x=selfideology,y=demideology,colour=as.factor(ClusterAssigned))) + geom_point()
