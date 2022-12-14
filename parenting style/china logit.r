library(tidyverse)
library(plyr)
library(haven)
library(DescTools)
library(stargazer)
library(plm)
library(car)
library(PerformanceAnalytics)


dat_c <- 
  read_dta("ecfps2010child_201906.dta")

dat_a <- 
  read_dta("ecfps2010adult_202008.dta")

dat_conf <- 
  read_dta("ecfps2010famconf_202008.dta")

dat_econ <- 
  read_dta("ecfps2010famecon_202008.dta")

dat_person <- 
  read_dta("ecfps2018person_202012.dta")

dat_au <- 
  dat_a %>%
  distinct(fid, .keep_all=TRUE)

dat_au_filt <- dat_au[c("pid", "fid", "qm401", "qm402", "qc1", "tb4_a_s", "qa2", "qa201acode",
                        "income", "qk601", "qe1_best", "qa101", "qa5code", "qa7_s_1")]

dat_econ_filt <- 
  dat_econ[c("fid", "faminc_net", "ff601", "fe3", "fe1", "fa6")] %>%
  distinct(fid, .keep_all=TRUE)

nchild <- 
  dat_c %>%
  group_by(fid) %>%
  tally()

dat_ch <- 
  dat_c %>%
  distinct(fid, .keep_all=TRUE)

dat_ch_filt <- dat_ch[c("pid", "fid", "we101", "we102", "we103", "we104", 
                        "we105", "we106", "we107", "wd2", "wd3", "we201", 
                        "we202", "we203", "we204", "we205", "we206", "wf805", "wd501")]

dat_merge <- 
  dat_ch_filt %>%
  left_join(dat_au_filt, by="fid") 
dat_merge <- dat_merge[order(dat_merge$fid),]
dat_merge <-
  dat_merge %>%
  left_join(nchild, by="fid")
colnames(dat_merge)[33] <- "nChild"
dat_merge <-
  dat_merge %>% 
  left_join(dat_econ_filt, by="fid")
dat_merge <- na.omit(dat_merge)

score_calc <- function(dat_col){
  score <- c()
  for(i in 1:length(dat_col)){
    score <- c(score, dat_col[i]/max(dat_col))
  }
  return(score)
}

score_pre <- cbind(score_calc(dat_merge$we101), 
                   (1-score_calc(dat_merge$we103)),
                   score_calc(dat_merge$we104),
                   (1-score_calc(dat_merge$we105)),
                   (1-score_calc(dat_merge$we106)),
                   score_calc(dat_merge$we107),
                   score_calc(dat_merge$wd2),
                   score_calc(dat_merge$wd3),
                   (score_calc(dat_merge$we201)),
                   (score_calc(dat_merge$we202)),
                   (score_calc(dat_merge$we203)),
                   (1-score_calc(dat_merge$we204)),
                   (1-score_calc(dat_merge$we205)),
                   (1-score_calc(dat_merge$we206)),
                   score_calc(dat_merge$wf805))

dat_merge <- 
  dat_merge %>%
  mutate(Q_ans = rowSums(1.1 > score_pre & 0 < score_pre))
dat_score <- subset(dat_merge, Q_ans>0)
score_pre <- score_pre[rowSums(1.1 > score_pre & 0 < score_pre)> 0 , ]

y_score <- c()
for(i in 1:nrow(score_pre)){
  y_score <- c(y_score, 
               sum(score_pre[i,][which(score_pre[i,] >=0 &score_pre[i,] < 1.1)]))
}

y_score2 <- c()
for(i in 1:nrow(score_pre2)){
  y_score2 <- c(y_score2, 
               sum(score_pre2[i,][which(score_pre2[i,] >=0 &score_pre2[i,] < 1.1)]))
}

#subset(df, Score <0.55 & Score >0.5)$Score %>% hist


#######GINI COEFFICIENT BY PLACE OF CURRENT HUKOU############

gini_1 <- c()
for(i in unique(df$qa201acode)){
  gini_1 <- c(gini_1, subset(df, df$qa201acode == i)$income %>% Gini())
}
temp <- data.frame(gini_1, unique(df$qa201acode))
colnames(temp)[2] <- "qa201acode"


gc <- c()
for(i in 1:nrow(df)){
  for(j in 1:nrow(temp)){
    if(df$qa201acode[i]==temp$qa201acode[j]){
      gc <- c(gc, temp$gini_1[j])
    }
  }
}

#############Gini for entire dataset#############
gini_2 <- c()
unique(dat_au_filt$qa201acode)[1]
for(i in unique(dat_au_filt$qa201acode)){
  gini_2 <- 
    c(gini_2, subset(dat_au_filt, dat_au_filt$qa201acode == i
                     & dat_au_filt$income >= 0)$income %>% Gini())
}
temp2 <- data.frame(gini_2, unique(dat_au_filt$qa201acode))
colnames(temp2)[2] <- "qa201acode"
temp2 <- temp2[-c(31,32),]

gc2 <- c()
for(i in 1:nrow(df)){
  for(j in 1:nrow(temp2)){
    if(df$qa201acode[i]==temp2$qa201acode[j]){
      gc2 <- c(gc2, temp2$gini_2[j])
    }
  }
}

dat_final <-
  dat_score %>%
  mutate(Score = y_score/Q_ans, 
         tiger = as.integer(Score > 0.85))
df <- subset(dat_final, 
             !rowSums(
               dat_final[c("qm401", "qm402", "qc1", "tb4_a_s", "qa2", "qa201acode", "qa101",
                           "income", "qk601", "qe1_best", "qa5code", "qa7_s_1", "nChild")] <0) &
               dat_final$qa2 %in% c(1,3))

df$qa5code[df$qa5code != 1] <- 0
df$qa7_s_1[df$qa7_s_1 != 1 & 6] <- 0
df$fe1[df$fe1 != 1] <- 0

df <- 
  df %>%
  mutate(Gini=gc,
         Gini_pop = gc2)


#########FINAL DATASET WE ARE WORKING WITH############
#View(df)
######################################################
subset(df, income <100000)$income %>% hist
##################BASELINE MODEL######################
model_1 <- 
  glm(tiger ~ nChild  + qa101 + as_factor(qa5code) +  as_factor(qa7_s_1) + 
        as_factor(qe1_best) + qk601 + faminc_net + faminc_net^2 +
        as_factor(qa2) + as_factor(tb4_a_s) + as_factor(qc1) + qm402 + qm401
        + fe1 + fe3 + fa6,
    family="binomial",
    data=df)

linear <- 
  lm(Score ~ nChild  + qa101 + as_factor(qa5code) +  as_factor(qa7_s_1) + 
       as_factor(qe1_best) + qk601 + faminc_net + faminc_net^2 +
       as_factor(qa2) + as_factor(tb4_a_s) + as_factor(qc1) + qm402 + qm401
     + fe1 + fe3 + fa6, data=df)

hist(df$Gini_pop, main="Local Gini Coefficient", xlab="Gini Index")


#PREDICTION
prob <- predict(model_logit, type="response") 
prediction <- ifelse(prob >0.5, 1, 0)

#prediction rate 
sum(prediction==df$tiger)/nrow(df)


####################MODEL WITH GINI###################
model_g <- 
  glm(tiger ~ Gini_pop + nChild + qa101 + as_factor(qa5code) +  as_factor(qa7_s_1) 
      + as_factor(qe1_best) + qk601 + faminc_net +  faminc_net^2 +
        as_factor(qa2) + as_factor(tb4_a_s) + as_factor(qc1) + qm402 + qm401 +
        fe1 + fe3 + fa6,
      family="binomial",
      data=df)


model_gp <- 
  glm(tiger ~ Gini_pop + nChild  +  qa101 + as_factor(qa5code) +  as_factor(qa7_s_1) 
      + as_factor(qe1_best) + qk601 + faminc_net +  faminc_net^2 +
        as_factor(qa2) + as_factor(tb4_a_s) + as_factor(qc1) + qm402 + qm401 +
        fe1 + fe3 + fa6,
      family="binomial"(link="probit"),
      data=df)

summary(model_g)
summary(model_gp)
plot(model_logit$fitted.values)

#PREDICTION
prob2 <- predict(model_g, type="response") 
prediction2 <- ifelse(prob2 >0.5, 1, 0)

#prediction rate 
sum(prediction2==df$tiger)/nrow(df)


#####################latex outputs###############################

stargazer(sumstat)

sumstat <- df[c("faminc_net", "nChild", "fa6", "qc1", "tb4_a_s", "fe3", "Gini")]
stargazer(attitude)

View(attitude)
View(sumstat)
summary(subset(sumstat, fa6>0))

sumstat <- sumstat[,-7]
sumstat
chart.Correlation(sumstat, main="Correlation Matrix")

?chart.Correlation

sd(na.omit(sumstat$Gini))


