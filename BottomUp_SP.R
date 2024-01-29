library(readr)
coildata <- coildata <- read_csv("coildata.csv")

library(lavaan);
modelData <- coildata
model<-"
! regressions 
   SP=~SP__SP1*SP1
   SP=~SP__SP2*SP2
   SP=~SP__SP3*SP3
   SP=~SP__SP4*SP4
   SP=~SP__SP5*SP5
! residuals, variances and covariances
   SP1 ~~ VAR_SP1*SP1
   SP2 ~~ VAR_SP2*SP2
   SP3 ~~ VAR_SP3*SP3
   SP4 ~~ VAR_SP4*SP4
   SP5 ~~ VAR_SP5*SP5
   SP ~~ 1.0*SP
! observed means
   SP1~1;
   SP2~1;
   SP3~1;
   SP4~1;
   SP5~1;
";
result1 <- lavaan(model, data=modelData, fixed.x=FALSE, estimator="ML", std.ov=TRUE);
summary(result, fit.measures=TRUE);

result2<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
summary(result2, fit.measures=TRUE);

lavTest(result1, test = "browne.residual.adf", output = "text")
lavTest(result2, test = "browne.residual.adf", output = "text")



library(dynamic)
estimated.model <- "SP =~ .855*SP1 + .079*SP2 + .723 *SP3 + .923 *SP4 + .715 *SP5"
cfaOne(model=estimated.model,n=65,manual=TRUE)