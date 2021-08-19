#Kiah Jane Jones
# 06.01.2021

##Globalisation and Democracy, SS2020
#Environmental Policy Support and Political Regime


# Packages
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("car")
install.packages("ggplot2")

install.packages("devtools") #graph
devtools::install_github("cardiomoon/ggiraphExtra") #graph

library(tidyverse)             
library(modelr)   
library(ggrepel)
library(car)
library(ggplot2)


# Dataset
dataset <- read.csv("~/Kiah/Glo.Dem.2020/dataset.csv") %>%
           filter(!is.na(vdem.mean), !is.na(qog), !is.na(hfi)) %>%
           select(-(grn.seat), -(r.rate), -(taxgap.shadow),
                  -(vdem.mean), -(reg.mean),
                  -(eps_mean)) %>%
           replace_na(list(mean.schl = 12, p_polity2 = 1))
summary(dataset)

# Simultaneous Multivariate Regression for all DV & IV & CV
mlm1 <- lm(cbind(ldi, qog) ~ v2x_polyarchy + v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + 
                             p_polity2 + fhi + fhi_polity2 + 
                             mean.schl + lit.rate + hfi + gdp.pc, data = dataset, na.action="na.exclude")
summary(mlm1)
head(resid(mlm1))
head(fitted(mlm1))
vcov(mlm1)
Anova(mlm1)

#fit the model
fit <- lm(cbind(ldi, qog) ~ v2x_polyarchy + v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + 
                            p_polity2 + fhi + fhi_polity2 + 
                            mean.schl + lit.rate + hfi + gdp.pc, data = dataset) 

summary(fit)
## grouping of IV & CV
ind.var <- dataset %>% select("v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem", 
                   "p_polity2", "fhi", "fhi_polity2")
con.var <- dataset %>% select("mean.schl", "lit.rate", "hfi", "gdp.pc")

## fit for QoG & all IV & CV
fit.qog <- lm(qog ~ v2x_polyarchy + v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + 
                    p_polity2 + fhi + fhi_polity2 + 
                    mean.schl + lit.rate + hfi + gdp.pc, data = dataset)
summary(fit.qog)
plot(fit.qog)
qqPlot(fit.qog,labels = row.names(dataset$cname), 
       simulate=TRUE, main = "QoG Plot")
outlierTest(fit.qog)
avPlots(fit.qog, ask=FALSE, id.method="identify")
influencePlot(fit.qog, id.method="identify", main="QoG Influence Plot", sub="Circle size is proportional to Cook’s distance")

## fit for LDI & all IV & CV
fit.ldi <- lm(ldi ~ v2x_polyarchy + v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + 
                    p_polity2 + fhi + fhi_polity2 + 
                    mean.schl + lit.rate + hfi + gdp.pc, data = dataset)
summary(fit.ldi)
plot(fit.ldi)
qqPlot(fit.ldi,labels = row.names(dataset$cname), 
       simulate=TRUE, main = "LDI Plot")
outlierTest(fit.ldi)
avPlots(fit.ldi, ask=FALSE, id.method="identify")
influencePlot(fit.ldi, id.method="identify", main="LDI Influence Plot", sub="Circle size is proportional to Cook’s distance")
