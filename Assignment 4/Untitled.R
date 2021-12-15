library(survey)

######################################################################
# Test code
######################################################################
library(survey) 
IceCr<-read.table("IceCream.txt") 
colnames(IceCr)=c("Obs","Grade","Spending","Group","Study","Alph","fpc1","fpc2","Income","Kids")
attach(IceCr)

pr<-1:40;
pr[Grade==7]<-20/1824;
pr[Grade==8]<- 9/1025;
pr[Grade==9]<-11/1151;
Wgt<-1/pr;

fpc <-1:40;
fpc[Grade==7]<-1824;
fpc[Grade==8]<-1025;
fpc[Grade==9]<-1151;

dcs <- svydesign(id=~Study,  strata =~Grade, data=IceCr, weights=~Wgt,fpc=~fpc,nest=TRUE)
summary(svyglm(Spending ~ Income + Kids + Income * Kids, design = dcs))

summary(svyglm(I(Group == "less") ~ Income + Kids + Income * Kids, design = dcs, family=binomial(link = "logit"))) 
summary(svyglm(I(Group == "less") ~ Income + Kids + Income * Kids, design = dcs, family=binomial(link = "probit"))) 

library(survey)
df <-read.table("cap2.txt", header = T) 
attach(df)
num_obs <- dim(df)[1]
fpc<-rep(800, num_obs)
dclus1 <- svydesign(id=~Dnum, data=df, fpc=~fpc) 
m <- svymean(~ Awards + Grth, dclus1)
ci <- confint(m, level = 0.9)
data.frame(m, ci)

######################################################################
dcs <- svydesign(id=~Study,  strata =~Grade, data=IceCr, weights=~Wgt,fpc=~fpc,nest=TRUE)

svytable(~Group + Kids, dcs)
svychisq(~Group + Kids, dcs,statistic = "Wald")
######################################################################
# My code
######################################################################
data <- read.table("cap3.txt", header = T)
attach(data)
n <- nrow(data)

#a)
fpc <- 1:n
fpc[stype == 'E'] <- 4421
fpc[stype == 'M'] <- 1018
fpc[stype == 'H'] <- 755 

sv <- svydesign(id=~ dnum, strata = stype, data = data, fpc =~ fpc, nest = TRUE)
svymean(~growth, sv)

#b)
summary(svyglm(growth ~ awards + meals + colgrad, design = sv))

#c)
summary(svyglm(I(grsign == 1) ~awards + meals + colgrad , design = sv, family=binomial(link = "logit"))) 

#d)
svytable(~grsign + awards, sv)
svychisq(~grsign + awards,, sv,statistic = "Wald")