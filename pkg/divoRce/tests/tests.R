## Test functions in divoRce

library(rcdd)
library(devtools)
load_all()

## TODO:
## * Test with standard numeric. set flag rational=FALSE. Tested fully.
## * Test with rational input, set flag rational=TRUE. Tested till nominal.
## * Test with standard numeric but rational=TRUE

rational <- TRUE

########################################################
############### Binary Data - BCL MODEL ################
########################################################
## quasi complete separation  -- CHECKS OUT!
library("detectseparation")
data("endometrial", package = "detectseparation")
endo_glm <- glm(HG ~ NV + PI + EH, family = binomial(), data = endometrial)
yqcs <- endometrial$HG
Xqcs <- model.matrix(endo_glm)

##rational
if(rational) Xqcs <- rcdd::d2q(Xqcs)

## check_sep
check_sep(yqcs,Xqcs,rational=rational) #works

##check_clvl
check_ovl(yqcs,Xqcs,rational=rational) #works


##### detect_sepcols_b
detect_sepcols_b(yqcs,Xqcs,rational=rational) #
detect_sepcols(yqcs,Xqcs,rational=rational) #

##### linearities
lis <- linearities(yqcs,Xqcs,rational=rational) #
lis

##### overlap_fc
overlap_fc(yqcs,Xqcs,frac=10,verbose=1,rational=rational)

##### rec_cone
rec_cone(yqcs,Xqcs,rational=rational) 

##### diagsep_b
diagsep_b(yqcs,Xqcs,rational=rational)
diagsep(yqcs,Xqcs,rational=rational) #TODO Diagsep returns Xstra rows and _x returns X rows. Which one to take? 
print.sepmod(diagsep_b(yqcs,Xqcs,rational=rational))
print.sepmod(diagsep_b(yqcs,Xqcs,rational=rational),"full")

##sepobs_b
sepobs_b(yqcs,Xqcs)

##quasi complete separation --- CHECKS OUT
load("./Data/Silvapulle.rda")
silv_glm <- glm(case~sex+ghq+sex:ghq, family = binomial(), data = Silvapulle)
silv_glm
ycs <- Silvapulle$case
Xcs <- model.matrix(silv_glm)

##rational
if(rational) Xcs <- rcdd::d2q(Xcs)

## check_sep
check_sep(ycs,Xcs,rational=rational) #works

##check_ovl
check_ovl(ycs,Xcs,rational=rational) #works

##### detect_sepcols_b
detect_sepcols_b(ycs,Xcs,rational=rational)
detect_sepcols(ycs,Xcs,rational=rational)

##### linearities
lis <- linearities(ycs,Xcs,rational=rational) 
lis

##### overlap_fc
overlap_fc(ycs,Xcs,rational=rational)

##### overlap_qc and separation_qc
overlap_qc(ycs,Xcs,rational=rational)
separation_qc(ycs,Xcs,rational=rational)

##### rec_cone
rec_cone(ycs,Xcs,rational=rational) #check out for overlap if this makes sense

##### diagsep_b
diagsep_b(ycs,Xcs,rational=rational)
diagsep(ycs,Xcs,rational=rational)
print.sepmod(diagsep(ycs,Xcs,rational=rational),"full")

## quasi complete separation example --- larger data set. CHECKS OUT
load("./Data/nsduh2019.rda")
nsduh_glm<- glm(her_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex, family = binomial, data = nsduh2019)
nsduh_glm
yqcs <- nsduh2019$her_lifetime
Xqcs <- model.matrix(nsduh_glm)

##rational
if(rational) Xqcs <- rcdd::d2q(Xqcs)

## check_sep
check_sep(yqcs,Xqcs,rational=rational) #works

##check_ovl
check_ovl(yqcs,Xqcs,rational=rational) #works

##### detect_sepcols_b
detect_sepcols_b(yqcs,Xqcs,rational=rational) #works
detect_sepcols(yqcs,Xqcs,rational=rational) #works

##### linearities
lis <- linearities(yqcs,Xqcs,rational=rational) 
lis

##### overlap_fc
overlap_fc(yqcs,Xqcs,frac=3,verbose=1,rational=rational)

##### overlap_qc and separation_qc
overlap_qc(yqcs,Xqcs,rational=rational) #works
separation_qc(yqcs,Xqcs,rational=rational) #works

##### rec_cone
rec_cone(yqcs,Xqcs,rational=rational) 

##### diagsep_b
diagsep_b(yqcs,Xqcs,rational=rational)
diagsep(yqcs,Xqcs,rational=rational)


## complete separation example
load("./Data/titanic3.rda")
tita_glm<- glm(Survived ~ Pclass + Sex, family = binomial, data = titanic3)
tita_glm
ycs <- tita_glm$y
Xcs <- model.matrix(tita_glm)

##rational 
if(rational) Xcs <- rcdd::d2q(Xcs)

## check_sep
check_sep(ycs,Xcs,rational=rational) #works

##check_ovl
check_ovl(ycs,Xcs,rational=rational) #works

##### detect_sepcols_b
detect_sepcols_b(ycs,Xcs,rational=rational) # TODO: not rational? Why
detect_sepcols(ycs,Xcs,rational=rational) # TODO: not rational? Why

##### linearities
lis <- linearities(ycs,Xcs,rational=rational)  # 
lis

##### overlap_fc
overlap_fc(ycs,Xcs,frac=10,rational=rational) #works

##### overlap_qc and separation_qc
overlap_qc(ycs,Xcs,rational=rational) #works
separation_qc(ycs,Xcs,rational=rational) #works

##### rec_cone
rec_cone(ycs,Xcs,rational=rational) # works

##### diagsep_b
diagsep_b(ycs,Xcs,rational=rational) #works
diagsep(ycs,Xcs,rational=rational) #works


##overlap 
data(aids,package="boot")
aids_glm <- glm(dud~time+year, family = binomial(), data = aids) #also separation if delay is included
yol1 <- aids$dud
Xol1 <- model.matrix(aids_glm)
summary(aids_glm)


##rational
if(rational) Xol1 <- rcdd::d2q(Xol1)

## check_sep
check_sep(yol1,Xol1,rational=rational) #works

##check_ovl
check_ovl(yol1,Xol1,rational=rational) #works

##### detect_sepcols_b
detect_sepcols_b(yol1,Xol1,rational=rational) 
detect_sepcols(yol1,Xol1,rational=rational) 
#detectseparation::detect_infinite_estimates(Xol1,yol1) ##detectseparation also says no infinity columns, but intercept is infinite. We are correct. 
#exp(coef(aids_glm))


##### linearities
lis <- linearities(yol1,Xol1,rational=rational) # works (does not list obs 22 23 24 25 26 48 49 50 51 71 75 76 78) 
lis

##### overlap_fc
overlap_fc(yol1,Xol1,rational=rational)

##### overlap_qc and separation_qc
overlap_qc(yol1,Xol1,rational=rational) #works
separation_qc(yol1,Xol1,rational=rational) #works

##### rec_cone
rec_cone(yol1,Xol1,rational=rational) #check out for overlap if this makes sense

##### diagsep_b
diagsep_b(yol1,Xol1,rational=rational)
diagsep(yol1,Xol1,rational=rational)

##overlap
data(CASchools,package="AER")
ca_glm <- glm(grades~teachers+calworks, family = binomial(), data = CASchools) #also separation if delay is included
yol2 <- CASchools$grades
Xol2 <- model.matrix(ca_glm)
summary(ca_glm)

##rational
if(rational) Xol2 <- rcdd::d2q(Xol2)

## check_sep
check_sep(yol2,Xol2,rational=rational) #works

##check_ovl
check_ovl(yol2,Xol2,rational=rational) #works

##### detect_sepcols_b
detect_sepcols_b(yol2,Xol2,rational=rational) # TODO: check this out for aids data - says no separation but has infinite value of Intercept.
detect_sepcols_b(yol2,Xol2)

##### linearities
lis <- linearities(yol2,Xol2,rational=rational) # works 
lis

##### overlap_fc
overlap_fc(yol2,Xol2,rational=rational)

##### overlap_qc and separation_qc
overlap_qc(yol2,Xol2,rational=rational) #works
separation_qc(yol2,Xol2,rational=rational) #works

##### rec_cone
rec_cone(yol2,Xol2,rational=rational) #check out for overlap if this makes sense

##### diagsep_b
diagsep_b(yol2,Xol2,rational=rational)
diagsep(yol2,Xol2,rational=rational)

#################################################
################### Multinomial Data - BCL MODEL
#################################################

## complete separation
load("./data/csepdatm.rda")
library(nnet)
csep_m <- multinom(y~x1+x2,data=csepdatm,model=TRUE)
ycs <- csepdatm$y
ycs <- model.response(csep_m$model)S
Xcs <- model.matrix(csep_m)
csep_m
summary(csep_m) #note 1::x2 is also separated

##rational
if(rational) Xcs <- rcdd::d2q(Xcs)

## check_sep
check_sep(ycs,Xcs,rational=rational) #works

##check_ovl
check_ovl(ycs,Xcs,rational=rational) #works

##### detect_sepcols_b
detect_sepcols_bcl(ycs,Xcs,rational=rational) # works
detect_sepcols(ycs,Xcs,rational=rational) # works

##### linearities
lis <- linearities_bcl(ycs,Xcs,rational=rational) # works
lis

##### overlap_fc
set.seed(1)
overlap_fc(ycs,Xcs,frac=3,verbose=1,rational=rational) #works

##### overlap_qc
overlap_qc(ycs,Xcs,rational=rational) #works
separation_qc(ycs,Xcs,rational=rational) #works


##### rec_cone
rec_cone(ycs,Xcs,rational=rational)  #TODO: just the one for offcols?

##### diagsep_bcl
diagsep_bcl(ycs,Xcs,rational=rational) # works
diagsep(ycs,Xcs,rational=rational) # works

###
sepobs_bcl(ycs,Xcs)

y <- yqcs
X <- Xqcs

## quasi complete separation
load("./data/qcsepdatm.rda")
library(nnet)
qcsep_bcl <- multinom(y~x1+x2,data=qcsepdatm)
yqcs <- qcsepdatm$y
Xqcs <- model.matrix(qcsep_bcl)
qcsep_bcl
summary(qcsep_bcl)


##rational
if(rational) Xqcs <- rcdd::d2q(Xqcs)

## check_sep
check_sep(yqcs,Xqcs,rational=rational) #works

##check_ovl
check_ovl(yqcs,Xqcs,rational=rational) #works

##### detect_sepcols_b
detect_sepcols_bcl(yqcs,Xqcs,rational=rational) # works
detect_sepcols(yqcs,Xqcs,rational=rational) # works

##### linearities
lis <- linearities(yqcs,Xqcs,rational=rational) # works
lis

##### overlap_fc 
overlap_fc(yqcs,Xqcs,frac=3,verbose=1,rational=rational) #works

##### overlap_qc
overlap_qc(yqcs,Xqcs,rational=rational) #works
separation_qc(yqcs,Xqcs,rational=rational) #works

##### rec_cone
rec_cone(yqcs,Xqcs,rational=rational)   

##### diagsep_bcl
diagsep_bcl(yqcs,Xqcs,rational=rational) # works
diagsep(yqcs,Xqcs,rational=rational)

## sepobs
sepobs_bcl(yqcs,Xqcs)
bcl_Xstar(yqcs,Xqcs)

## overlap separation
load("./Data/ovldatm.rda")
library(nnet)
ovl_bcl <- multinom(y~x1+x2,data=ovldatm,model=TRUE)
yol <- ovl_bcl$model$y
Xol <- model.matrix(ovl_bcl)
ovl_bcl

##rational
if(rational) Xol <- rcdd::d2q(Xol)

## check_sep
check_sep(yol,Xol,rational=rational) #works

##check_ovl
check_ovl(yol,Xol,rational=rational) #works

##### detect_sepcols_b
detect_sepcols_bcl(yol,Xol,rational=rational) # works
detect_sepcols(yol,Xol,rational=rational)

##### linearities
lis <- linearities(yol,Xol,rational=rational) # works
lis

##### overlap_fc 
overlap_fc(yol,Xol,frac=3,verbose=1,rational=rational) 

##### rec_cone
rec_cone(yol,Xol,rational=rational)   #works

##### diagsep_bcl
diagsep_bcl(yol,Xol,rational=rational) # works yet
diagsep(yol,Xol,rational=rational) # works yet

## 
sepobs_bcl(yol,Xol)

## Alligator Data - Overlap
load("./Data/Alligators2.rda")
library(nnet)
allgm1 <- nnet::multinom(foodchoice ~ size + lake + sex, data = Alligators2) #separation in interaction
allgm1
summary(allgm1)

y <- Alligators2$foodchoice
X <- model.matrix(allgm1)

##rational
if(rational) X <- rcdd::d2q(X)

## check_sep
check_sep(y,X,rational=rational) #works

##check_ovl
check_ovl(y,X,rational=rational) #works

##### overlap_fc 
overlap_fc(y,X,frac=1,verbose=1,rational=rational) #works
overlap_fc(y,X,frac=3,verbose=1,rational=rational)

##### detect_sepcols_b
detect_sepcols_bcl(y,X,rational=rational) # works
detect_sepcols(y,X,rational=rational) # works

#TODO: interestingly it is much quicker to do; I made new quick checks
system.time(any(isTRUE(detect_sepcols(y,X,rational=rational)$separated)))
system.time(check_sep(y,X,rational=rational))


##### linearities
lis <- linearities(y,X,rational=rational) # works
lis

##### rec_cone
rec_cone(y,X,rational=rational)   #works

##### diagsep_bcl
sd1 <- diagsep_bcl(y,X,rational=rational) # works
diagsep(y,X,rational=rational) # works
print.sepmod(sd1)


## Alligator Data - QC Separation
load("./Data/Alligators2.rda")
library(nnet)
allgm2 <- nnet::multinom(foodchoice ~ size + lake * sex, data = Alligators2) #separation in interaction
summary(allgm2)


y <- Alligators2$food
X <- model.matrix(allgm2)

##rational
if(rational) X <- rcdd::d2q(X)

## check_sep
check_sep(y,X,rational=rational) #works

##check_ovl
check_ovl(y,X,rational=rational) #works

##### detect_sepcols_b
detect_sepcols_bcl(y,X,rational=rational) # works
detect_sepcols(y,X,rational=rational)

##### linearities
lis <- linearities(y,X,rational=rational) # works
lis

##### overlap_fc 
overlap_fc(y,X,frac=1,verbose=1,rational=rational) #works
overlap_fc(y,X,frac=5,verbose=1,rational=rational) #works

## quick checks
overlap_qc(y,X,rational=rational)
separation_qc(y,X,rational=rational)


##### rec_cone
rec_cone(y,X,rational=rational)   #works

##### diagsep_bcl
sd1 <- diagsep_bcl(y,X,rational=rational) # works
sd1 <- diagsep(y,X,rational=rational) # works
sd1
print.sepmod(sd1,info="full")


## Alligator Data - QC Separation mlogit version; should be able to work with the binary check. Make it work.
load("./Data/Alligators2.rda")
library(mlogit)
Allig <- dfidx(Alligators2, shape = "wide", choice = "foodchoice")
allgm3 <- mlogit(foodchoice ~ 0 | size + lake * sex, data = Allig) #separation in interaction
summary(allgm3)

## test again
data(Alligators)
mAllig <- nnet::multinom(foodchoice ~ size + lake * sex, data = Alligators) # separation in interaction
y <- Alligators$foodchoice
X <- model.matrix(mAllig)


## linearities are the observations that are separated. So in overlap all are linearities. In complete separation, there are no linearities. In QC sep we have linearities between 0 and n.   
xst <- bcl_Xstar(y,X)
lis <- linearities_bcl(y,X,rational=rational) 
sepobs_bcl(y,X)

## y <- allgm3$foodchoice
## X <- model.matrix(allgm3)


## ##rational
## if(rational) X <- rcdd::d2q(X)

## ## check_sep
## check_sep(y,X) #works

## ##check_ovl
## check_ovl(y,X) #works

## ##### detect_sepcols_b
## detect_sepcols_bcl(y,X) # works

## ##### linearities
## lis <- linearities(y,X) # works
## lis

## ##### overlap_fc 
## overlap_fc(y,X,frac=1,verbose=1) #works

## ##### rec_cone
## rec_cone(y,X)   #works

## ##### diagsep_bcl
## sd1 <- diagsep_bcl(y,X) # works but the sepcol issue is still open. 
## str(sd1)
## print.sepmod(sd1,info="full")


## Alligator Data - QC Separation mclogit version
load("./Data/Alligators2.rda")
library(mclogit)
allgm4 <- mblogit(foodchoice ~ size + lake * sex, data = Alligators2) #separation in interaction
summary(allgm4)

y <- Alligators2$foodchoice
X <- model.matrix(allgm4)

##rational
if(rational) X <- rcdd::d2q(X)

## check_sep
check_sep(y,X,rational=rational) #works

##check_ovl
check_ovl(y,X,rational=rational) #works

##### detect_sepcols_b
detect_sepcols_bcl(y,X,rational=rational) 

##### linearities
lis <- linearities(y,X,rational=rational) 
lis

##### overlap_fc 
overlap_fc(y,X,frac=1,verbose=1,rational=rational) 
overlap_fc(y,X,frac=5,verbose=1,rational=rational) 

##### overlap_fc 
overlap_qc(y,X,rational=rational) #doesnt work
separation_qc(y,X,rational=rational) #doesnt work

##### rec_cone
rec_cone(y,X,rational=rational)   #works

##### diagsep_bcl
sd1 <- diagsep_bcl(y,X,rational=rational) # works but the sepcol issue is still open.
diagsep(y,X,rational=rational)
print.sepmod(sd1,info="full")


##########################################################
###############  ORDINAL DATA - CLM MODEL ################
###########################################################

##TODO: Check this also with only one predictor

#complete 
load("./Data/csepdato.rda")
library(ordinal)
csep_cl <- clm(y~x1+x2,data=csepdato)
summary(csep_cl)
y <- csepdato$y
X <- model.matrix(csep_cl)$X


##rational
if(rational) X <- rcdd::d2q(X)

## check_sep
check_sep(y,X,rational=rational) #works

##check_clvl
check_ovl(y,X,rational=rational) #works

##
rational <- FALSE
o1 <- cl_Xstar(y,X,rational=rational) #works
o2 <- cl_XstarOLD(y,X,rational=rational) #works
o1
o2

all.equal(o1,o2)

##### detect_sepcols
detect_sepcols_cl(y,X,rational=rational)  #works
detect_sepcols(y,X,rational=rational)  #works
 
##### linearities
lis <- linearities(y,X,rational=rational) #works
lis

##### overlap_fc 
overlap_fc(y,X,frac=3,verbose=1,rational=rational) #works

##### overlap_qc 
overlap_qc(y,X,rational=rational) #works
separation_qc(y,X,rational=rational) #works

##### rec_cone
rec_cone(y,X,rational=rational)   #works

##### diagsep_cl
diagsep_cl(y,X,rational=rational) ##
diagsep(y,X,rational=rational) ##

### sepobs_cl
sepobs_cl(y,X)

## quasi complete separation
load("./Data/qcsepdato.rda")
library(ordinal)
qcsep_cl <- clm(y~x1+x2,data=qcsepdato)
qcsep_cl
y <- qcsepdato$y
X <- model.matrix(qcsep_cl)$X



##rational
if(rational) X <- rcdd::d2q(X)

## check_sep
check_sep(y,X,rational=rational) #works

##check_ovl
check_ovl(y,X,rational=rational) #works

##
cl_Xstar(y,X,rational=rational)

##### detect_sepcols
detect_sepcols_cl(y,X,rational=rational) 
 
##### linearities
lis <- linearities(y,X,rational=rational) 
lis

##### overlap_fc 
overlap_fc(y,X,frac=3,verbose=1,rational=rational)

##### overlap_fc 
overlap_qc(y,X,rational=rational) 
separation_qc(y,X,rational=rational)

##### rec_cone
rec_cone(y,X,rational=rational)   

##### diagsep_cl
diagsep_cl(y,X,rational=rational)
diagsep(y,X,rational=rational)  

### sepobs_cl
sepobs_cl(y,X)
rownames(X) <- NULL


## overlap
load("./Data/ovldato.rda")
library(ordinal)
ovl_cl <- clm(y~x1+x2,data=ovldato)
ovl_cl
y <- ovldato$y
X <- model.matrix(ovl_cl)$X


##rational
if(rational) X <- rcdd::d2q(X)

## check_sep
check_sep(y,X,rational=rational) #works

##check_clvl
check_ovl(y,X,rational=rational) #works

##
cl_Xstar(y,X,rational=rational)

##### detect_sepcols
detect_sepcols_cl(y,X,rational=rational) #
detect_sepcols(y,X,rational=rational) #
 
##### linearities
lis <- linearities(y,X,rational=rational) 
lis

##### overlap_fc 
overlap_fc(y,X,frac=3,verbose=1,rational=rational)

##### 
overlap_qc(y,X,rational=rational) 
separation_qc(y,X,rational=rational)


##### rec_cone
rec_cone(y,X,rational=rational)   

##### diagsep_cl
diagsep_cl(y,X,rational=rational)  #
diagsep(y,X,rational=rational)

### sepobs_cl
sepobs_cl(y,X)

##############
########################REVEALED AND STATED PREFERENCES FOR HEALTH DATA SHARING##########################################

## library(car)
## library(ordinal)
## ## HDS data full
## load("./Data/HDSFull.rda")

## spineplot(HDSS$WTSSHI~HDSS$trustSHI)
## spineplot(HDSS$WTSSHI~HDSS$knowledge)
## spineplot(HDSS$WTSSHI~HDSS$chronic)

## ##subsample
## set.seed(666)
## ind <- sample(1:dim(HDS)[1],300)
## HDSS <- HDS[ind,]
## HDSS$ID <- row.names(HDSS) <- 1:300
## head(HDSS)
## HDSS <- HDSS[,c("WTSSHI","trustSHI","knowledge","chronic","Gender")]
## ##recode
## HDSS$WTSSHI <- (as.numeric(HDSS$WTSSHI)-5)*-1 #willing to share with Social Health Insurance SHI with higher value means less willing,so we flip
## HDSS$WTSSHI <- as.ordered(HDSS$WTSSHI)
## HDSS$trustSHI <- (as.numeric(HDSS$trustSHI)-6)*-1 #trust in Social Health Insurance SHI with higher value means less trust so we flip 
## HDSS$trustSHI <- as.factor(HDSS$trustSHI) #trust the SHI 
## HDSS$gender <- factor(HDSS$Gender,labels=c("m","f","x"))
## HDSS$Gender <- NULL
## HDSS$knowledge <- (as.numeric(HDSS$knowledge)-5)*-1 #originally 0 high knowledge, 4 no knowledge, so we flipped
## HDSS$knowledge <- as.factor(HDSS$knowledge) 
## HDSS$chronic <- as.factor(HDSS$chronic) #number of chronic conditions
## head(HDSS)
## summary(HDSS)
## save(HDSS,file="./Data/HDSS.rda")


load("./Data/HDSS.rda")
library(ordinal)
levels(HDSS$trustSHI) <- c("1","2","3","4","4")
HDSS$trustSHI
clmFitSHI8<-clm(WTSSHI ~ trustSHI*knowledge + chronic + gender, link = "logit", data = HDSS) #great to show that ,control=clm.control(maxIter=10000L,gradTol=1e-15))
summary(clmFitSHI8)


y <- as.ordered(HDSS$WTSSHI)
X <- model.matrix(clmFitSHI8)$X

head(cl_Xstar(y,X)) ## 
debug(cl_Xstar)


rational <- FALSE
o1 <- cl_Xstar(y,X,rational=rational) #works
o2 <- cl_XstarOLD(y,X,rational=rational) #works
all.equal(o1,o2)

nx <- cl_Xstar(y,X)

##rational
if(rational) X <- rcdd::d2q(X)

## check_sep 
check_sep(y,X,rational=rational) #works

##check_ovl
check_ovl(y,X,rational=rational) #works

##### overlap_fc 
overlap_fc(y,X,frac=1,verbose=1,rational=rational) 
overlap_fc(y,X,frac=3,verbose=1,rational=rational)

##### overlap_fc 
overlap_qc(y,X,rational=rational) #
separation_qc(y,X,rational=rational)

##### detect_sepcols_cl
detect_sepcols_cl(y,X,rational=rational) #Looks legit now.
coef(clmFitSHI8)
summary(clmFitSHI8)
detect_sepcols(y,X,rational=rational)
cbind(coef(clmFitSHI8)*detect_sepcols_cl(y,X,rational=rational)$separated,coef(clmFitSHI8))
##TODO: great example as trustSHI2:knowledge4 looks not separated but with more interations it alos is.

ox1 <- cl_Xstar(y,X,label=TRUE)
ox1[,"trustSHI5"]
colnames(ox1)
X[,"trustSHI5"]
cbind(ox1[,"trustSHI5"],X[,"trustSHI5"])

##### linearities
lis <- linearities(y,X,rational=rational) # works
lis

##### rec_cone
rec_cone(y,X,rational=rational)   #

##### diagsep_cl
sd1 <- diagsep_cl(y,X,rational=rational) #
sd1
diagsep(y,X,rational=rational)
print.sepmod(sd1)
print.sepmod(sd1,"full")

#### WINE TASTING
## TODO: Things are fucked up for this example with bottle. Why? Because of the singularities?
data(ordinal::wine)
#m1 <- clm(rating~temp+contact+bottle+judge,data=ordinal::wine) #estimation is all good, but thig sare fucked up. 
m1 <- clm(rating~temp+contact+bottle,data=ordinal::wine) #all good
m1

y <- ordinal::wine$rating
X <- model.matrix(m1)$X

head(cl_Xstar(y,X)) ## 

rational <- FALSE

##rational
if(rational) X <- rcdd::d2q(X)

## check_sep 
check_sep(y,X,rational=rational) #works

##check_ovl
check_ovl(y,X,rational=rational) #works

##### overlap_fc 
overlap_fc(y,X,frac=1,verbose=1,rational=rational) 
overlap_fc(y,X,frac=3,verbose=1,rational=rational)

##### overlap_fc 
overlap_qc(y,X,rational=rational) #
separation_qc(y,X,rational=rational)

##### detect_sepcols_cl
detect_sepcols_cl(y,X,rational=rational) #
detect_sepcols(y,X,rational=rational)

##### linearities
lis <- linearities(y,X,rational=rational) # works
lis

##### rec_cone
rec_cone(y,X,rational=rational)   #reccdim is 2 but no cone is returned. Becuase two singular columns in X. We now warn. TODO: We might also just correct reccdim-(dim(X)[2]-qr(X)$rank).

##### diagsep_cl
sd1 <- diagsep_cl(y,X,rational=rational) #All wrong here due to the singularity parts. We warn now.
sd1

## summary(m1)
## library(VGAM)
## m2 <- vglm(rating~temp+contact,family=acat(reverse = TRUE, parallel = FALSE),data=ordinal::wine)
## summary(m2)

## X <- model.matrix(m2)
## y <- ordinal::wine$rating


#### Trying with adjacent category models 
##  It should be so that we can check adjacent category with baseline-category, no? Looks like it works and tells which ones are responsible.  
library(ordinal)
m1 <- nnet::multinom(rating~temp+contact,data=ordinal::wine) #all good
m1
summary(m1)

y <- factor(ordinal::wine$rating,ordered=FALSE)
X <- model.matrix(m1)

rational <- TRUE

##rational
if(rational) X <- rcdd::d2q(X)

## check_sep 
check_sep(y,X,rational=rational) #works

##check_ovl
check_ovl(y,X,rational=rational) #works

##### overlap_fc 
overlap_fc(y,X,frac=1,verbose=1,rational=rational) 
overlap_fc(y,X,frac=3,verbose=1,rational=rational)

##### overlap_fc 
overlap_qc(y,X,rational=rational) #
separation_qc(y,X,rational=rational)

##### detect_sepcols_cl
detect_sepcols_bcl(y,X,rational=rational) #
detect_sepcols(y,X,rational=rational)

##### linearities
lis <- linearities(y,X,rational=rational) # works
lis

##### rec_cone
rec_cone(y,X,rational=rational)   

##### diagsep_bcl
sd1 <- diagsep_bcl(y,X,rational=rational) 
sd1

## summary(m1)
## library(VGAM)
## m2 <- vglm(rating~temp+contact,family=acat(reverse = TRUE, parallel = FALSE),data=ordinal::wine)
## summary(m2)

## X <- model.matrix(m2)
## y <- ordinal::wine$rating


##########################################################
###############  ORDINAL DATA - SL MODEL ################
###########################################################


#complete 
load("./Data/csepdato.rda")
library(ordinal)
csep_sl <- clm(y~x1+x2,data=csepdato)
summary(csep_sl)
y <- csepdato$y
X <- model.matrix(csep_sl)$X


##rational
if(rational) X <- rcdd::d2q(X)

## check_sep
check_sep(y,X,rational=rational) #works

##check_clvl
check_ovl(y,X,rational=rational) #works

##
rational <- FALSE
o1 <- cl_Xstar(y,X,rational=rational) #works
o2 <- cl_XstarOLD(y,X,rational=rational) #works
o1
o2

all.equal(o1,o2)

##### detect_sepcols
detect_sepcols_cl(y,X,rational=rational)  #works
detect_sepcols(y,X,rational=rational)  #works
 
##### linearities
lis <- linearities(y,X,rational=rational) #works
lis

##### overlap_fc 
overlap_fc(y,X,frac=3,verbose=1,rational=rational) #works

##### overlap_qc 
overlap_qc(y,X,rational=rational) #works
separation_qc(y,X,rational=rational) #works

##### rec_cone
rec_cone(y,X,rational=rational)   #works

##### diagsep_cl
diagsep_cl(y,X,rational=rational) ##
diagsep(y,X,rational=rational) ##

## quasi complete separation
load("./Data/qcsepdato.rda")
library(ordinal)
qcsep_cl <- clm(y~x1+x2,data=qcsepdato)
qcsep_cl
y <- qcsepdato$y
X <- model.matrix(qcsep_cl)$X



##rational
if(rational) X <- rcdd::d2q(X)

## check_sep
check_sep(y,X,rational=rational) #works

##check_ovl
check_ovl(y,X,rational=rational) #works

##
cl_Xstar(y,X,rational=rational)

##### detect_sepcols
detect_sepcols_cl(y,X,rational=rational) 
 
##### linearities
lis <- linearities(y,X,rational=rational) 
lis

##### overlap_fc 
overlap_fc(y,X,frac=3,verbose=1,rational=rational)

##### overlap_fc 
overlap_qc(y,X,rational=rational) 
separation_qc(y,X,rational=rational)

##### rec_cone
rec_cone(y,X,rational=rational)   

##### diagsep_cl
diagsep_cl(y,X,rational=rational)
diagsep(y,X,rational=rational)  

## overlap
load("./Data/ovldato.rda")
library(ordinal)
ovl_sl <- clm(y~x1+x2,data=ovldato)
ovl_sl
y <- ovldato$y
X <- model.matrix(ovl_sl)$X

rational <- FALSE

##rational
if(rational) X <- rcdd::d2q(X)

## check_sep
check_sep_sl(y,X,rational=rational) #works

##check_clvl
check_ovl(y,X,rational=rational) #not works

##### detect_sepcols
detect_sepcols_sl(y,X,rational=rational) #
detect_sepcols(y,X,rational=rational) #
 
##### linearities
linearities_sl(y,X,rational=rational) 
linearities_sl(y,X,rational=rational,reduced=FALSE) 

##### overlap_fc 
overlap_fc(y,X,frac=3,verbose=1,rational=rational)

##### 
overlap_qc(y,X,rational=rational) 
separation_qc(y,X,rational=rational)


##### rec_cone
reccone_sl(y,X,rational=rational)
reccone_sl(y,X,rational=rational,reduced=FALSE)   

##### diagsep_cl
diagsep_sl(y,X,rational=rational)  #
diagsep(y,X,rational=rational)




##########################################################
###############  ORDINAL DATA - ACL MODEL ################
###########################################################


## complete separation
load("./Data/csepdatm.rda")
library(nnet)
csep_m <- multinom(y~x1+x2,data=csepdatm,model=TRUE)
ycs <- csepdatm$y
ycs <- model.response(csep_m$model)
Xcs <- model.matrix(csep_m)
csep_m

## Xstar
acl_Xstar(ycs,Xcs)


## sepobs
sepobs_acl(ycs,Xcs) #checks out

## quasi complete separation
load("./Data/qcsepdatm.rda")
library(nnet)
qcsep_bcl <- multinom(y~x1+x2,data=qcsepdatm)
yqcs <- qcsepdatm$y
Xqcs <- model.matrix(qcsep_bcl)
qcsep_bcl
summary(qcsep_bcl)


## overlap separation
load("./Data/ovldatm.rda")
library(nnet)
ovl_bcl <- multinom(y~x1+x2,data=ovldatm,model=TRUE)
yol <- ovl_bcl$model$y
Xol <- model.matrix(ovl_bcl)
ovl_bcl

y <- yol
X <- Xol

acl_Xstar(yol,Xol)



##########################################################
###############  ORDINAL DATA - OSM MODEL ################
###########################################################

### test osm
library(clustord)
library(ordinal)
data(wine)
m1 <- clm(rating ~ temp * contact, data = wine)
summary(m1)
m2 <- osm(rating ~ temp * contact, data = wine)
summary(m2)
model.matrix(m2)
model.matrix(m1)$X

#OSM has the same parametrization as polr 



## overlap separation
load("./Data/ovldatm.rda")
library(nnet)
ovl_osm <- multinom(y~x1+x2,data=ovldatm,model=TRUE)
yol <- ovl_osm$model$y
Xol <- model.matrix(ovl_osm)
ovl_osm
yol <- as.ordered(yol)
yol
Xol 
library(devtools)
load_all()
osm_Xstar
osm_Xstar(yol,Xol)

## qc separation
load("./Data/qcsepdatm.rda")
library(nnet)
ovl_osm <- multinom(y~x1+x2,data=qcsepdatm,model=TRUE)
yol <- ovl_osm$model$y
Xol <- model.matrix(ovl_osm)
ovl_osm
yol <- as.ordered(yol)
yol
Xol 
library(devtools)
load_all()

#OSM Xstar
osm_Xstar(yol,Xol)

 
##############################################################
############# Test make_yx
## normal use case

load("./Data/nsduh2019.rda")
frml <- her_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex
dats <- nsduh2019
mk <- make_yx(frml,dats)
str(mk)
y <- mk$y
X <- mk$X
## rational artihmetic
yr <- as.character(as.numeric(mk$y))
Xr <- apply(mk$X,2,as.character)
datr1 <- data.frame(yr,Xr)
datr2 <- cbind(yr,Xr)
frmlr1 <- yr~X.Intercept.+ alc_agefirst + demog_sexFemale
frmlr2 <- yr~(Intercept)+ alc_agefirst + demog_sexFemale
mkr1 <- make_yx(frmlr1,datr1)
mkr2 <- make_yx(frmlr2,datr2)
str(mkr1)
str(mkr2)

## test rational creation of matrices
struc_vec(y,X) #given no rational and kept
struc_vec(yr,Xr) # given rational and kept 
struc_vec(y,X,rational=TRUE) #given no rational and converted
struc_vec(yr,Xr,rational=TRUE) #given rational and kept


# new bcl_Xstar

m2 <- bcl_Xstar(yqcs,Xqcs)
m2a <- bcl_Xstar(yqcs,Xqcs,rational=TRUE)
yqcs1 <- as.character(yqcs)
Xqcs2 <- apply(Xqcs,2,as.character)
rat_cols(Xqcs2)
Xqcs1 <- d2q(Xqcs)
Xqcs1
rat_cols(Xqcs1)

m3 <- bcl_Xstar(yqcs1,Xqcs1)
str(m3)
str(m1) 
str(m2)
str(m2a)
str(m3)

all.equal(m2,m3)

detect_sepcols_b(y,X,rational=TRUE)





