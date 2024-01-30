library(readr)
coildata <- coildata <- read_csv("coildata.csv")

# Especification for Specialization Dimension
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
result1 <- lavaan(model, data=modelData, estimator="ML", std.ov=TRUE);
summary(result, fit.measures=TRUE);

result2<-lavaan(model, data=modelData, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
summary(result2, fit.measures=TRUE);

lavTest(result1, test = "browne.residual.adf", output = "text")
lavTest(result2, test = "browne.residual.adf", output = "text")

pave <- lavTest(result1, test = "browne.residual.adf", output = "text")
pave$browne.residual.adf[2]

library(dynamic)
estimated.model <- "SP =~ .855*SP1 + .079*SP2 + .723 *SP3 + .923 *SP4 + .715 *SP5"
cfaOne(model=estimated.model,n=65,manual=TRUE, reps = 100)

# Especification for Credibility Dimension

model2<-"
! regressions 
   CR=~CR__CR1*CR1
   CR=~CR__CR2*CR2
   CR=~CR__CR3*CR3
   CR=~CR__CR4*CR4
   CR=~CR__CR5*CR5
! residuals, variances and covariances
   CR1 ~~ VAR_CR1*CR1
   CR2 ~~ VAR_CR2*CR2
   CR3 ~~ VAR_CR3*CR3
   CR4 ~~ VAR_CR4*CR4
   CR5 ~~ VAR_CR5*CR5
   CR ~~ 1.0*CR
! observed means
   CR1~1;
   CR2~1;
   CR3~1;
   CR4~1;
   CR5~1;
";
result3 <- lavaan(model2, data=modelData, estimator="ML", std.ov=TRUE);
summary(result3, fit.measures=TRUE);

result4 <- lavaan(model2, data=modelData, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
summary(result4, fit.measures=TRUE);

lavTest(result3, test = "browne.residual.adf", output = "text")
lavTest(result4, test = "browne.residual.adf", output = "text")

# Especification for Coordination Dimension

model3<-"
! regressions 
   CD=~CD__CD1*CD1
   CD=~CD__CD2*CD2
   CD=~CD__CD3*CD3
   CD=~CD__CD4*CD4
   CD=~CD__CD5*CD5
! residuals, variances and covariances
   CD1 ~~ VAR_CD1*CD1
   CD2 ~~ VAR_CD2*CD2
   CD3 ~~ VAR_CD3*CD3
   CD4 ~~ VAR_CD4*CD4
   CD5 ~~ VAR_CD5*CD5
   CD ~~ 1.0*CD
! observed means
   CD1~1;
   CD2~1;
   CD3~1;
   CD4~1;
   CD5~1;
"

result5 <- lavaan(model3, data=modelData, estimator="ML", std.ov=TRUE);
summary(result3, fit.measures=TRUE);

result6 <- lavaan(model3, data=modelData, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
summary(result4, fit.measures=TRUE);

lavTest(result5, test = "browne.residual.adf", output = "text")
lavTest(result6, test = "browne.residual.adf", output = "text")
