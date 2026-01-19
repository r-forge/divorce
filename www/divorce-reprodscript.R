### Replication script for "Existence and Uniqueness of Maximum Liklelihood Estimation in Categorical Resonse Models" by Sablica, Hornik and Rusch
### This script licensed as CC-BY https://creativecommons.org/licenses/by/4.0/



### Install Package
# install.packages("divoRce", repos="http://R-Forge.R-project.org")

### Load Package
library(divoRce)

### Load Data 
data(HDSS) #HDSS Data

### Mosaic Plot in Figure 1
## We use the cvd package mosaic plot
library(vcd)
vcd::mosaic(~ knowledge + trustSHI + WTSSHI, highlighting= 'WTSSHI', direction=c('h','v','h'), data = HDSS, zero_size=0)

### Fitting cumulative link models  with interaction that creates separation
## We fit with ordinal package
library(ordinal)

## Logistic Link (proportional odds model)
m1<- ordinal::clm(WTSSHI~trustSHI*knowledge+gender+chronic,data=HDSS,control=list(maxIter=1000)) 
summary(m1)

## C-Loglog Link
m2<- ordinal::clm(WTSSHI~trustSHI*knowledge+gender+chronic,data=HDSS,control=list(maxIter=1000),link="cloglog")
summary(m2)

### Extraction of model elements we need for the check for easy re-use
X1<-model.matrix(m1)$X
X2<-model.matrix(m2)$X
y<-HDSS$WTSSHI

### Checking for separation
## Linear program check
check_sep_cl(y,X1) #check separation for cumulative link logistic model; separation confirmed
check_sep_cl(y,X2) #check separation for cumulative link c-loglog model; separation confirmed

## More detailed diagnostics (same situation for both model types)
sd1<-diagsep_cl(y,X1) 
print(sd1,info="full")

sd2<-diagsep_cl(y,X2)
print(sd2,info="full")

### Now fitting models without interaction (so no separation anymore)
## Logistic Link
m01<- ordinal::clm(WTSSHI~trustSHI+knowledge+gender+chronic,data=HDSS,control=list(maxIter=1000))

# Model comparison
anova(m01,m1) #likelihood ratio test 
AIC(m01,m1) #AIC comparison 
BIC(m01,m1) #BIC comparison
# Based on these criteria we may simplify m1 to m01 and solve the separation issue that way.

## C-Log-Log Link
m02<- clm(WTSSHI~trustSHI+knowledge+gender+chronic,data=HDSS,control=list(maxIter=1000),link="cloglog")

# Model comparison
anova(m02,m2) #likelihood ratio test 
AIC(m02,m2) #AIC comparison 
BIC(m02,m2) #BIC comparison

# Based on these criteria we may not simplify m2 to m02 to solve separation issue (unless we use BIC). So we need to try something else.

### Removing observations that cause separation in the c-loglog cumulative link model (but keeping all the design matrix columns)
## identification of observations
offobs <- sepobs_cl(y,X2)
offobs

## removing the observations
m2Fixed<- ordinal::clm(WTSSHI~trustSHI*knowledge+gender+chronic,data=HDSS[-offobs$index,],link="cloglog")
summary(m2Fixed) # we fixed the separation issue now. We have singular design matrix now due to removal of observations (but that is not a problem per se). One may further sanitize by removing the offending columns as well which can be identified with sepcol_cl(y,X2) 
