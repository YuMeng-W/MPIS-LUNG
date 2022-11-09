###################################
##########feature_selection#######


library(glmnet) 
library(survival)  

#Load data
df <- read.csv("data_gdph.csv")

#Scale
df_scale <- scale(df) 

#Feature selection(Lasso)
feat <- 0
i = 0 
while (i<10) {
  i = i + 1
  x <- as.matrix(df_scale[,-c(1,2,3)])  
  y <- Surv(df_scale$OS_month, df_scale$OS_YN)  
  fit <- glmnet(x, y, family="cox", alpha=1)
  # plot(fit, label = TRUE)
  # plot(fit, xvar = "lambda", label = TRUE)
  fitcv <- cv.glmnet(x, y, family="cox", alpha=1, nfolds = 10)
  s=fitcv$lambda.min
  # s=fitcv$lambda.1se
  fit_coef <- coef(fitcv, s)
  feat_sel = rownames(as.data.frame(which(coef(fitcv, s="lambda.min")[,1]!=0)))
  feat <- append(feat, feat_sel)
}

feat = feat[-1]
feature = as.data.frame(table(feat))
feature = feature[order(feature[, "Freq"], decreasing = TRUE),]

#coef
feats_sel = feature[0:5,]
data_lasso = data_ed[as.character(feats_sel[,1])]
data_lasso = merge(data[,c("OS_YN", "OS_month")], data_lasso)
row.names(data_lasso) = data_lasso[,1] 
data_lasso = data_lasso[,-1]
head(data_lasso)
write.csv(data_lasso,file = 'feat_sel.csv')

cox_res <- coxph(Surv(OS_month, OS_YN) ~., data = data_lasso)
weight = as.data.frame(cox_res$coef)
write.csv(weight,file = 'weight.csv')

