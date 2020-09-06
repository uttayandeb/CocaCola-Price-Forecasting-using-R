############### Packages Required #################
install.packages("rmarkdown")
install.packages("forecast")
install.packages("fpp")
install.packages("smooth")
install.packages("readxl")

library(rmarkdown)

library(forecast)

library(fpp)

library(smooth)

library(readxl)

CocaCola <-read.csv(file.choose()) # read the Airlines data
View(CocaCola) # Quarterly 4 months 
windows()
plot(Cocacola$Sales,type="o")


Q1 <-  ifelse(grepl("Q1",Cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",Cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",Cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",Cocacola$Quarter),'1','0')

# So creating 12 dummy variables 

CocacolaData<-cbind(Cocacola,Q1,Q2,Q3,Q4)
View(CocacolaData)
colnames(CocacolaData)
## [1] "Quarter" "Sales"   "Q1"      "Q2"      "Q3"      "Q4"
CocacolaData["t"]<- 1:42
View(CocacolaData)
CocacolaData["log_Sales"]<-log(CocacolaData["Sales"])
CocacolaData["t_square"]<-CocacolaData["t"]*CocacolaData["t"]
attach(CocacolaData)
## The following objects are masked _by_ .GlobalEnv:
## 
##     Q1, Q2, Q3, Q4
train<-CocacolaData[1:36,]

test<-CocacolaData[37:40,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
## 
## Call:
## lm(formula = Sales ~ t, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -485.47 -287.86  -17.94  163.36  803.06 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1537.244    118.024   13.03 9.01e-15 ***
## t             64.500      5.563   11.60 2.31e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 346.7 on 34 degrees of freedom
## Multiple R-squared:  0.7982, Adjusted R-squared:  0.7922 
## F-statistic: 134.4 on 1 and 34 DF,  p-value: 2.313e-13
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 644.018 and Adjusted R2 Vaue - 79.22%
## [1] 644.0188
######################### Exponential #################################

expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
## 
## Call:
## lm(formula = log_Sales ~ t, data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.21775 -0.09522 -0.01134  0.07156  0.32157 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 7.446255   0.041266  180.44  < 2e-16 ***
## t           0.023219   0.001945   11.94 1.04e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1212 on 34 degrees of freedom
## Multiple R-squared:  0.8074, Adjusted R-squared:  0.8017 
## F-statistic: 142.5 on 1 and 34 DF,  p-value: 1.038e-13
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 524.7351 and Adjusted R2 - 80.17 %
## [1] 524.7351
######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
## 
## Call:
## lm(formula = Sales ~ t + t_square, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -523.55 -220.14  -35.64  253.51  531.75 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2017.6741   150.8093  13.379    7e-15 ***
## t            -11.3573    18.7949  -0.604 0.549795    
## t_square       2.0502     0.4927   4.161 0.000213 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 285 on 33 degrees of freedom
## Multiple R-squared:  0.8676, Adjusted R-squared:  0.8596 
## F-statistic: 108.1 on 2 and 33 DF,  p-value: 3.238e-15
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 434.7185 and Adjusted R2 - 85.96 %
## [1] 434.7185
######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
## 
## Call:
## lm(formula = Sales ~ Q1 + Q2 + Q3 + Q4, data = train)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -924.1 -624.8 -163.8  576.8 1522.6 
## 
## Coefficients: (1 not defined because of singularities)
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2712.9      249.3  10.884 2.74e-12 ***
## Q11           -393.9      352.5  -1.117    0.272    
## Q21            238.6      352.5   0.677    0.503    
## Q31            225.5      352.5   0.640    0.527    
## Q41               NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 747.8 on 32 degrees of freedom
## Multiple R-squared:  0.1163, Adjusted R-squared:  0.03346 
## F-statistic: 1.404 on 3 and 32 DF,  p-value: 0.2596
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
## Warning in predict.lm(sea_add_model, newdata = test, interval = "predict"):
## prediction from a rank-deficient fit may be misleading
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 1785.135
## [1] 1785.135
######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)
## 
## Call:
## lm(formula = Sales ~ t + Q1 + Q2 + Q3 + Q4, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -476.34 -157.05  -67.11   67.65  617.56 
## 
## Coefficients: (1 not defined because of singularities)
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1435.20     124.21  11.554 9.13e-13 ***
## t              63.89       4.32  14.788 1.37e-15 ***
## Q11          -202.21     126.86  -1.594  0.12110    
## Q21           366.40     126.50   2.897  0.00686 ** 
## Q31           289.39     126.27   2.292  0.02887 *  
## Q41               NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 267.7 on 31 degrees of freedom
## Multiple R-squared:  0.8903, Adjusted R-squared:  0.8761 
## F-statistic: 62.89 on 4 and 31 DF,  p-value: 1.969e-14
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
## Warning in predict.lm(Add_sea_Linear_model, interval = "predict", newdata =
## test): prediction from a rank-deficient fit may be misleading
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 534.6979 and Adjusted R2 - 87.61
## [1] 534.6979
######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
## 
## Call:
## lm(formula = Sales ~ t + t_square + Q1 + Q2 + Q3 + Q4, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -280.05 -134.63   28.75   93.79  341.09 
## 
## Coefficients: (1 not defined because of singularities)
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1919.0149    99.3368  19.318  < 2e-16 ***
## t            -12.8328    10.6588  -1.204 0.238019    
## t_square       2.0735     0.2793   7.423 2.85e-08 ***
## Q11         -202.2097    76.5664  -2.641 0.012999 *  
## Q21          370.5450    76.3462   4.853 3.52e-05 ***
## Q31          293.5369    76.2125   3.852 0.000573 ***
## Q41                NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 161.6 on 30 degrees of freedom
## Multiple R-squared:  0.9613, Adjusted R-squared:  0.9549 
## F-statistic: 149.1 on 5 and 30 DF,  p-value: < 2.2e-16
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
## Warning in predict.lm(Add_sea_Quad_model, interval = "predict", newdata =
## test): prediction from a rank-deficient fit may be misleading
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 236.7075 and Adjusted R2 - 95.49%
## [1] 236.7075
######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
## 
## Call:
## lm(formula = log_Sales ~ Q1 + Q2 + Q3 + Q4, data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.37206 -0.21908 -0.03649  0.21230  0.45120 
## 
## Coefficients: (1 not defined because of singularities)
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.87651    0.08845  89.052   <2e-16 ***
## Q11         -0.15985    0.12509  -1.278    0.210    
## Q21          0.08161    0.12509   0.652    0.519    
## Q31          0.07542    0.12509   0.603    0.551    
## Q41               NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2653 on 32 degrees of freedom
## Multiple R-squared:  0.1315, Adjusted R-squared:  0.05006 
## F-statistic: 1.615 on 3 and 32 DF,  p-value: 0.2053
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
## Warning in predict.lm(multi_sea_model, newdata = test, interval =
## "predict"): prediction from a rank-deficient fit may be misleading
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 1871.203
## [1] 1871.203
######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) 
## 
## Call:
## lm(formula = log_Sales ~ t + Q1 + Q2 + Q3 + Q4, data = train)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.161001 -0.043058 -0.003783  0.033233  0.252528 
## 
## Coefficients: (1 not defined because of singularities)
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.417783   0.040219 184.434  < 2e-16 ***
## t            0.022936   0.001399  16.397  < 2e-16 ***
## Q11         -0.091041   0.041078  -2.216  0.03414 *  
## Q21          0.127486   0.040958   3.113  0.00397 ** 
## Q31          0.098357   0.040887   2.406  0.02230 *  
## Q41                NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08668 on 31 degrees of freedom
## Multiple R-squared:  0.9102, Adjusted R-squared:  0.8986 
## F-statistic: 78.56 on 4 and 31 DF,  p-value: 9.001e-16
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
## Warning in predict.lm(multi_add_sea_model, newdata = test, interval =
## "predict"): prediction from a rank-deficient fit may be misleading
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 335.1026 and Adjusted R2 - 89.86%
## [1] 335.1026
# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive Seasonality with Quadratic trend  has least RMSE value

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocacolaData)
new_model_pred<-data.frame(predict(new_model,newdata=CocacolaData,interval='predict'))
## Warning in predict.lm(new_model, newdata = CocacolaData, interval =
## "predict"): prediction from a rank-deficient fit may be misleading
new_model_fin <- new_model$fitted.values

View(new_model_fin)

# pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Quarter <- as.data.frame(CocacolaData$Quarter)

Final <- as.data.frame(cbind(Quarter,CocacolaData$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 


plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")


View(Final)


# plot(Final$new_model_fin,type="o")