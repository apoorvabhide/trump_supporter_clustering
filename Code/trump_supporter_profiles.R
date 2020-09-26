rm(list=ls())
library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)
setwd("~/Documents/Writing/Trump Supporter/")
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
                           # videoabtpolitics=CC18_300d_3,
                           # fwdpolitics=CC18_300d_5,
                           alwaysallowabortion=CC18_321a,
                           allowemployersdenyabortion=CC18_321d,
                           allabortionsillegal=CC18_321f,
                           borderspendingpluswall=CC18_322a,
                           daca=CC18_322b,
                           legalimmigration=CC18_322c_new,
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
