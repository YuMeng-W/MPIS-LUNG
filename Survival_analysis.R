#########################################
##########Survival analysis##############


library(broom)
library(glmnet)
library(cutoff)
library(ggpubr)
library(survival)
library(tableone)
library(openxlsx)
library(survminer)

#Load data
data_gdph <- read.csv("clin_gdph.csv")

#cut-off 
cutpoint = median(data_gdph$mpis)

#Categorize variable 
tmp0 <- data_gdph$mpis > cutpoint
MPIS_gdph <- as.vector(ifelse(tmp0, "1", "0"))
data_gdph$MPIS <- MPIS_gdph

#Univariable analysis
data_gdph$MPIS <- factor(data_gdph$MPIS)    
res.cox <- coxph(Surv(OS_month, OS) ~MPIS, data = data_gdph)
sum.cox <- summary(res.cox)

#Multivariable analysis
#factors
fvars <- c('gender', 'age', 'smoking', 'AJCC_stage', 'tumor_site', 'tumor_differ','MPIS')     
data_gdph[fvars]<-lapply(data_gdph[fvars],factor)

#Model
f <- coxph(Surv(OS_month, OS) ~gender + smoking + tumor_differentiation + AJCC_stage, 
            data = data_gdph) 
f_step <- step(f)

