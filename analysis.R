
data8<-read.csv("min8full.csv",header=T)
library(plm)
pdata8=pdata.frame(data8,c("compname","year"))
attach(pdata8)
library(perturb)
library(car)
library(ppcor)
library(nlme)

pairs(cbind(wpat,wnw,wnfa,wnwc))
cor(cbind(wpat,wnw,wnfa,wnwc))
pcor(cbind(wpat,wnw,wnfa,wnwc))

########################## Fixed Time Effects ###############
#######################Four ownership codes model######################
model8.re.4=lme(wpat~(wnw+wnwc+wnfa)*factor(oscode4)*factor(year),data=pdata8,random=~1|compname,correlation=corAR1(),na.action="na.omit")
options(max.print=5.5E5)
summary(model8.re.4)$tTable

#R-square
e.re.4=resid(model8.re.4)
denom.re.4=sum((pdata8$wpat-mean(pdata8$wpat))^2)
num.re.4=sum(e.re.4^2)
rsq.re.4=1-(num.re.4/denom.re.4)
rsq.re.4
#Adjusted R-square
num.re.4.adj=sum(e.re.4^2)/(6444-320-1)
deno.re.4.adj=denom.re.4/(6444-1)
rsq.re.4.adj=1-(num.re.4.adj/deno.re.4.adj)
rsq.re.4.adj

#Residual plots
plot(model8.re.4,main="Residual Plot: Fixed time effects")

#Normal probability plot
e.re.4<-residuals(model8.re.4)
std<-function(x) {(x-mean(x))/sqrt(var(x))}
e.std.re.4<-std(e.re.4)
qqnorm(e.std.re.4,main="Normal Q-Q plot: Fixed time effects")
qqline(e.std.re.4)

#Collinearity diagnostic
colldiag(model.matrix(model8.re.4),add.intercept=FALSE)

#Time coefficients plot
time.coefs<-model8.re.4$coef[8:26]
plot(c(1993:2011),time.coefs,col="Red",main="Time fixed effects: Plot",xlab="Time",ylab="Fixed effects",pch=15)


################Fixed time effect differences between consecutive years################
c((model8.re.4$coef$fixed['factor(year)1994'] -  
     model8.re.4$coef$['factor(year)1993']), 
  linearHypothesis(model8.re.4," factor(year)1994 = factor(year)1993")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)1995'] -  
     model8.re.4$coefficient['factor(year)1994']), 
  linearHypothesis(model8.re.4," factor(year)1995 = factor(year)1994")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)1996'] -  
     model8.re.4$coefficient['factor(year)1995']), 
  linearHypothesis(model8.re.4," factor(year)1996 = factor(year)1995")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)1997'] -  
     model8.re.4$coefficient['factor(year)1996']), 
  linearHypothesis(model8.re.4," factor(year)1997 = factor(year)1996")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)1998'] -  
     model8.re.4$coefficient['factor(year)1997']), 
  linearHypothesis(model8.re.4," factor(year)1998 = factor(year)1997")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)1999'] -  
     model8.re.4$coefficient['factor(year)1998']), 
  linearHypothesis(model8.re.4," factor(year)1999 = factor(year)1998")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2000'] -  
     model8.re.4$coefficient['factor(year)1999']), 
  linearHypothesis(model8.re.4," factor(year)2000 = factor(year)1999")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2001'] -  
     model8.re.4$coefficient['factor(year)2000']), 
  linearHypothesis(model8.re.4," factor(year)2001 = factor(year)2000")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2002'] -  
     model8.re.4$coefficient['factor(year)2001']), 
  linearHypothesis(model8.re.4," factor(year)2002 = factor(year)2001")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2003'] -  
     model8.re.4$coefficient['factor(year)2002']), 
  linearHypothesis(model8.re.4," factor(year)2003 = factor(year)2002")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2004'] -  
     model8.re.4$coefficient['factor(year)2003']), 
  linearHypothesis(model8.re.4," factor(year)2004 = factor(year)2003")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2005'] -  
     model8.re.4$coefficient['factor(year)2004']), 
  linearHypothesis(model8.re.4," factor(year)2005 = factor(year)2004")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2006'] -  
     model8.re.4$coefficient['factor(year)2005']), 
  linearHypothesis(model8.re.4," factor(year)2006 = factor(year)2005")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2007'] -  
     model8.re.4$coefficient['factor(year)2006']), 
  linearHypothesis(model8.re.4," factor(year)2007 = factor(year)2006")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2008'] -  
     model8.re.4$coefficient['factor(year)2007']), 
  linearHypothesis(model8.re.4," factor(year)2008 = factor(year)2007")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2009'] -  
     model8.re.4$coefficient['factor(year)2008']), 
  linearHypothesis(model8.re.4," factor(year)2009 = factor(year)2008")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2010'] -  
     model8.re.4$coefficient['factor(year)2009']), 
  linearHypothesis(model8.re.4," factor(year)2010 = factor(year)2009")$"Pr(>Chisq)")

c((model8.re.4$coefficient['factor(year)2011'] -  
     model8.re.4$coefficient['factor(year)2010']), 
  linearHypothesis(model8.re.4," factor(year)2011 = factor(year)2010")$"Pr(>Chisq)")





##################### Decadewise analysis ####################
model8.dec<-plm(wpat~factor(oscode4)*(wnw+wnwc+wnfa)*factor(dec),data=pdata8,model="random",effect="individual")
summary(model8.dec)

#Error plot
yhat.dec<-model8.dec$model[[1]] - model8.dec$residuals
e.dec<-model8.dec$residuals
std<-function(x) {(x-mean(x))/sqrt(var(x))}
e.std.dec<-std(e.dec)
plot(yhat.dec,e.std.dec,col="Blue",main="Residual plot: Decade wise analysis",ylab="Standardised residuals",xlab="Fitted values",pch=19)

#Normal probability plot
qqnorm(e.std.dec)
qqline(e.std.dec)

#Collinearity diagnostic
colldiag(model.matrix(model8.dec),add.intercept=FALSE)


############### Testing for differences in PAT values#############
############ Within decades ############
#Decade 1
c((model8.dec$coef['factor(oscode4)NFB'] -  
     model8.dec$coef['factor(oscode4)GOVT']), 
  linearHypothesis(model8.dec," factor(oscode4)NFB = factor(oscode4)GOVT")$"Pr(>Chisq)")

c((model8.dec$coef['factor(oscode4)NFB'] -  
     model8.dec$coef['factor(oscode4)MNC']), 
  linearHypothesis(model8.dec," factor(oscode4)NFB = factor(oscode4)MNC")$"Pr(>Chisq)")

c((model8.dec$coef['factor(oscode4)MNC'] -  
     model8.dec$coef['factor(oscode4)GOVT']), 
  linearHypothesis(model8.dec," factor(oscode4)MNC = factor(oscode4)GOVT")$"Pr(>Chisq)")


#Decade 2
c((model8.dec$coef['factor(oscode4)GOVT:factor(dec)1'] -  
     model8.dec$coef['factor(dec)1']), 
  linearHypothesis(model8.dec," factor(oscode4)GOVT:factor(dec)1 = factor(dec)1")$"Pr(>Chisq)")

c((model8.dec$coef['factor(oscode4)MNC:factor(dec)1'] -  
     model8.dec$coef['factor(dec)1']), 
  linearHypothesis(model8.dec," factor(oscode4)MNC:factor(dec)1 = factor(dec)1")$"Pr(>Chisq)")

c((model8.dec$coef['factor(oscode4)NFB:factor(dec)1'] -  
     model8.dec$coef['factor(dec)1']), 
  linearHypothesis(model8.dec," factor(oscode4)NFB:factor(dec)1 = factor(dec)1")$"Pr(>Chisq)")

c((model8.dec$coef['factor(oscode4)MNC:factor(dec)1'] -  
     model8.dec$coef['factor(oscode4)GOVT:factor(dec)1']), 
  linearHypothesis(model8.dec," factor(oscode4)MNC:factor(dec)1 = factor(oscode4)GOVT:factor(dec)1")$"Pr(>Chisq)")

c((model8.dec$coef['factor(oscode4)NFB:factor(dec)1'] -  
     model8.dec$coef['factor(oscode4)GOVT:factor(dec)1']), 
  linearHypothesis(model8.dec," factor(oscode4)NFB:factor(dec)1 = factor(oscode4)GOVT:factor(dec)1")$"Pr(>Chisq)")

c((model8.dec$coef['factor(oscode4)NFB:factor(dec)1'] -  
     model8.dec$coef['factor(oscode4)MNC:factor(dec)1']), 
  linearHypothesis(model8.dec," factor(oscode4)NFB:factor(dec)1 = factor(oscode4)MNC:factor(dec)1")$"Pr(>Chisq)")


############ Across decades ############
#GOVT
c((model8.dec$coef['factor(oscode4)GOVT:factor(dec)1'] -  
     model8.dec$coef['factor(oscode4)GOVT']), 
  linearHypothesis(model8.dec," factor(oscode4)GOVT:factor(dec)1 = factor(oscode4)GOVT")$"Pr(>Chisq)")

#MNC
c((model8.dec$coef['factor(oscode4)MNC:factor(dec)1'] -  
     model8.dec$coef['factor(oscode4)MNC']), 
  linearHypothesis(model8.dec," factor(oscode4)MNC:factor(dec)1 = factor(oscode4)MNC")$"Pr(>Chisq)")


#NFB
c((model8.dec$coef['factor(oscode4)NFB:factor(dec)1'] -  
     model8.dec$coef['factor(oscode4)NFB']), 
  linearHypothesis(model8.dec," factor(oscode4)NFB:factor(dec)1 = factor(oscode4)NFB")$"Pr(>Chisq)")



################ Testing for contributions to PAT #############
##### Diffs in utilization across decades########
### NW ####
#GOVT-GOVT
c(((model8.dec$coef['factor(oscode4)GOVT:wnw:factor(dec)1']) - (model8.dec$coef['factor(oscode4)GOVT:wnw'])),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnw:factor(dec)1=factor(oscode4)GOVT:wnw")$"Pr(>Chisq)")

#MNC-MNC
c((model8.dec$coef['factor(oscode4)MNC:wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnw']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnw:factor(dec)1=factor(oscode4)MNC:wnw")$"Pr(>Chisq)")

#NFB-NFB
c((model8.dec$coef['factor(oscode4)NFB:wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnw']),
  linearHypothesis(model8.dec,"factor(oscode4)NFB:wnw:factor(dec)1=factor(oscode4)NFB:wnw")$"Pr(>Chisq)")


##### NWC########
#GOVT-GOVT
c((model8.dec$coef['factor(oscode4)GOVT:wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)GOVT:wnwc']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnwc:factor(dec)1=factor(oscode4)GOVT:wnwc")$"Pr(>Chisq)")

#MNC-MNC
c((model8.dec$coef['factor(oscode4)MNC:wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnwc']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnwc:factor(dec)1=factor(oscode4)MNC:wnwc")$"Pr(>Chisq)")

#NFB-NFB
c((model8.dec$coef['factor(oscode4)NFB:wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnwc']),
  linearHypothesis(model8.dec,"factor(oscode4)NFB:wnwc:factor(dec)1=factor(oscode4)NFB:wnwc")$"Pr(>Chisq)")

##### NFA ########
#GOVT-GOVT
c((model8.dec$coef['factor(oscode4)GOVT:wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)GOVT:wnfa']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnfa:factor(dec)1=factor(oscode4)GOVT:wnfa")$"Pr(>Chisq)")

#MNC-MNC
c((model8.dec$coef['factor(oscode4)MNC:wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnfa']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnfa:factor(dec)1=factor(oscode4)MNC:wnfa")$"Pr(>Chisq)")

#NFB-NFB
c((model8.dec$coef['factor(oscode4)NFB:wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnfa']),
  linearHypothesis(model8.dec,"factor(oscode4)NFB:wnfa:factor(dec)1=factor(oscode4)NFB:wnfa")$"Pr(>Chisq)")


################ Within decade 1 ###############
################ Net Worth ##############
#GOVT-MNC
c((model8.dec$coef['factor(oscode4)GOVT:wnw'] - model8.dec$coef['factor(oscode4)MNC:wnw']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnw = factor(oscode4)MNC:wnw")$"Pr(>Chisq)")

#GOVT-NFB
c((model8.dec$coef['factor(oscode4)GOVT:wnw'] - model8.dec$coef['factor(oscode4)NFB:wnw']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnw = factor(oscode4)NFB:wnw")$"Pr(>Chisq)")

#MNC-NFB
c((model8.dec$coef['factor(oscode4)MNC:wnw'] - model8.dec$coef['factor(oscode4)NFB:wnw']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnw = factor(oscode4)NFB:wnw")$"Pr(>Chisq)")

####################### Net working capital ###################
#GOVT-MNC
c((model8.dec$coef['factor(oscode4)GOVT:wnwc'] - model8.dec$coef['factor(oscode4)MNC:wnwc']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnwc = factor(oscode4)MNC:wnwc")$"Pr(>Chisq)")

#GOVT-NFB
c((model8.dec$coef['factor(oscode4)GOVT:wnwc'] - model8.dec$coef['factor(oscode4)NFB:wnwc']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnwc = factor(oscode4)NFB:wnwc")$"Pr(>Chisq)")

#MNC-NFB
c((model8.dec$coef['factor(oscode4)MNC:wnwc'] - model8.dec$coef['factor(oscode4)NFB:wnwc']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnwc = factor(oscode4)NFB:wnwc")$"Pr(>Chisq)")

######################### Net fixed assets #######################
#GOVT-MNC
c((model8.dec$coef['factor(oscode4)GOVT:wnfa'] - model8.dec$coef['factor(oscode4)MNC:wnfa']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnfa = factor(oscode4)MNC:wnfa")$"Pr(>Chisq)")

#GOVT-NFB
c((model8.dec$coef['factor(oscode4)GOVT:wnfa'] - model8.dec$coef['factor(oscode4)NFB:wnfa']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnfa = factor(oscode4)NFB:wnfa")$"Pr(>Chisq)")

#MNC-NFB
c((model8.dec$coef['factor(oscode4)MNC:wnfa'] - model8.dec$coef['factor(oscode4)NFB:wnfa']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnfa = factor(oscode4)NFB:wnfa")$"Pr(>Chisq)")

##################### Within decade 2 ###############
################ Net Worth ##############
# FB-NFB
c((model8.dec$coef['wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"wnw:factor(dec)1 = factor(oscode4)NFB:wnw:factor(dec)1")$"Pr(>Chisq)")

#FB-GOVT
c((model8.dec$coef['wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)GOVT:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"wnw:factor(dec)1 = factor(oscode4)GOVT:wnw:factor(dec)1")$"Pr(>Chisq)")

#FB-MNC
c((model8.dec$coef['wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"wnw:factor(dec)1 = factor(oscode4)MNC:wnw:factor(dec)1")$"Pr(>Chisq)")

# GOVT-MNC
c((model8.dec$coef['factor(oscode4)GOVT:wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnw:factor(dec)1 = factor(oscode4)MNC:wnw:factor(dec)1")$"Pr(>Chisq)")

# GOVT-NFB
c((model8.dec$coef['factor(oscode4)GOVT:wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnw:factor(dec)1 = factor(oscode4)NFB:wnw:factor(dec)1")$"Pr(>Chisq)")

# MNC-NFB
c((model8.dec$coef['factor(oscode4)MNC:wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnw:factor(dec)1 = factor(oscode4)NFB:wnw:factor(dec)1")$"Pr(>Chisq)")

################ Net Working Capital ##############
# FB-NFB
c((model8.dec$coef['wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"wnwc:factor(dec)1 = factor(oscode4)NFB:wnwc:factor(dec)1")$"Pr(>Chisq)")

#FB-GOVT
c((model8.dec$coef['wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)GOVT:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"wnwc:factor(dec)1 = factor(oscode4)GOVT:wnwc:factor(dec)1")$"Pr(>Chisq)")

#FB-MNC
c((model8.dec$coef['wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"wnwc:factor(dec)1 = factor(oscode4)MNC:wnwc:factor(dec)1")$"Pr(>Chisq)")

# GOVT-MNC
c((model8.dec$coef['factor(oscode4)GOVT:wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnwc:factor(dec)1 = factor(oscode4)MNC:wnwc:factor(dec)1")$"Pr(>Chisq)")

# GOVT-NFB
c((model8.dec$coef['factor(oscode4)GOVT:wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnwc:factor(dec)1 = factor(oscode4)NFB:wnwc:factor(dec)1")$"Pr(>Chisq)")

# MNC-NFB
c((model8.dec$coef['factor(oscode4)MNC:wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnwc:factor(dec)1 = factor(oscode4)NFB:wnwc:factor(dec)1")$"Pr(>Chisq)")

################ Net Fixed Assets ##############
# FB-NFB
c((model8.dec$coef['wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"wnfa:factor(dec)1 = factor(oscode4)NFB:wnfa:factor(dec)1")$"Pr(>Chisq)")

#FB-GOVT
c((model8.dec$coef['wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)GOVT:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"wnfa:factor(dec)1 = factor(oscode4)GOVT:wnfa:factor(dec)1")$"Pr(>Chisq)")

#FB-MNC
c((model8.dec$coef['wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"wnfa:factor(dec)1 = factor(oscode4)MNC:wnfa:factor(dec)1")$"Pr(>Chisq)")

# GOVT-MNC
c((model8.dec$coef['factor(oscode4)GOVT:wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnfa:factor(dec)1 = factor(oscode4)MNC:wnfa:factor(dec)1")$"Pr(>Chisq)")

# GOVT-NFB
c((model8.dec$coef['factor(oscode4)GOVT:wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnfa:factor(dec)1 = factor(oscode4)NFB:wnfa:factor(dec)1")$"Pr(>Chisq)")

# MNC-NFB
c((model8.dec$coef['factor(oscode4)MNC:wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnfa:factor(dec)1 = factor(oscode4)NFB:wnfa:factor(dec)1")$"Pr(>Chisq)")


##################### Difference-in-difference####################
################ Net Worth ##############
# FB-NFB
c((model8.dec$coef['wnw'] - model8.dec$coef['factor(oscode4)NFB:wnw']) - 
    (model8.dec$coef['wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"wnw-factor(oscode4)NFB:wnw=wnw:factor(dec)1-factor(oscode4)NFB:wnw:factor(dec)1")$"Pr(>Chisq)")

#FB-GOVT
c((model8.dec$coef['wnw'] - model8.dec$coef['factor(oscode4)GOVT:wnw']) - 
    (model8.dec$coef['wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)GOVT:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"wnw-factor(oscode4)GOVT:wnw=wnw:factor(dec)1-factor(oscode4)GOVT:wnw:factor(dec)1")$"Pr(>Chisq)")

#FB-MNC
c((model8.dec$coef['wnw'] - model8.dec$coef['factor(oscode4)MNC:wnw']) - 
    (model8.dec$coef['wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"wnw-factor(oscode4)MNC:wnw=wnw:factor(dec)1-factor(oscode4)MNC:wnw:factor(dec)1")$"Pr(>Chisq)")

#GOVT-MNC
c((model8.dec$coef['factor(oscode4)GOVT:wnw'] - model8.dec$coef['factor(oscode4)MNC:wnw']) - 
    (model8.dec$coef['factor(oscode4)GOVT:wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnw -factor(oscode4)MNC:wnw=factor(oscode4)GOVT:wnw:factor(dec)1-factor(oscode4)MNC:wnw:factor(dec)1")$"Pr(>Chisq)")

#GOVT-NFB
c((model8.dec$coef['factor(oscode4)GOVT:wnw'] - model8.dec$coef['factor(oscode4)NFB:wnw']) - 
    (model8.dec$coef['factor(oscode4)GOVT:wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnw -factor(oscode4)NFB:wnw=factor(oscode4)GOVT:wnw:factor(dec)1-factor(oscode4)NFB:wnw:factor(dec)1")$"Pr(>Chisq)")

#MNC-NFB
c((model8.dec$coef['factor(oscode4)MNC:wnw'] - model8.dec$coef['factor(oscode4)NFB:wnw']) - 
    (model8.dec$coef['factor(oscode4)MNC:wnw:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnw:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnw -factor(oscode4)NFB:wnw=factor(oscode4)MNC:wnw:factor(dec)1-factor(oscode4)NFB:wnw:factor(dec)1")$"Pr(>Chisq)")


####################### Net working capital ###################
# FB-NFB
c((model8.dec$coef['wnwc'] - model8.dec$coef['factor(oscode4)NFB:wnwc']) - 
    (model8.dec$coef['wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"wnwc-factor(oscode4)NFB:wnwc=wnwc:factor(dec)1-factor(oscode4)NFB:wnwc:factor(dec)1")$"Pr(>Chisq)")

#FB-GOVT
c((model8.dec$coef['wnwc'] - model8.dec$coef['factor(oscode4)GOVT:wnwc']) - 
    (model8.dec$coef['wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)GOVT:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"wnwc-factor(oscode4)GOVT:wnwc=wnwc:factor(dec)1-factor(oscode4)GOVT:wnwc:factor(dec)1")$"Pr(>Chisq)")

#FB-MNC
c((model8.dec$coef['wnwc'] - model8.dec$coef['factor(oscode4)MNC:wnwc']) - 
    (model8.dec$coef['wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"wnwc-factor(oscode4)MNC:wnwc=wnwc:factor(dec)1-factor(oscode4)MNC:wnwc:factor(dec)1")$"Pr(>Chisq)")

#GOVT-MNC
c((model8.dec$coef['factor(oscode4)GOVT:wnwc'] - model8.dec$coef['factor(oscode4)MNC:wnwc']) - 
    (model8.dec$coef['factor(oscode4)GOVT:wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnwc -factor(oscode4)MNC:wnwc=factor(oscode4)GOVT:wnwc:factor(dec)1-factor(oscode4)MNC:wnwc:factor(dec)1")$"Pr(>Chisq)")

#GOVT-NFB
c((model8.dec$coef['factor(oscode4)GOVT:wnwc'] - model8.dec$coef['factor(oscode4)NFB:wnwc']) - 
    (model8.dec$coef['factor(oscode4)GOVT:wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnwc -factor(oscode4)NFB:wnwc=factor(oscode4)GOVT:wnwc:factor(dec)1-factor(oscode4)NFB:wnwc:factor(dec)1")$"Pr(>Chisq)")

#MNC-NFB
c((model8.dec$coef['factor(oscode4)MNC:wnwc'] - model8.dec$coef['factor(oscode4)NFB:wnwc']) - 
    (model8.dec$coef['factor(oscode4)MNC:wnwc:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnwc:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnwc -factor(oscode4)NFB:wnwc=factor(oscode4)MNC:wnwc:factor(dec)1-factor(oscode4)NFB:wnwc:factor(dec)1")$"Pr(>Chisq)")


######################### Net fixed assets #######################
# FB-NFB
c((model8.dec$coef['wnfa'] - model8.dec$coef['factor(oscode4)NFB:wnfa']) - 
    (model8.dec$coef['wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"wnfa-factor(oscode4)NFB:wnfa=wnfa:factor(dec)1-factor(oscode4)NFB:wnfa:factor(dec)1")$"Pr(>Chisq)")

#FB-GOVT
c((model8.dec$coef['wnfa'] - model8.dec$coef['factor(oscode4)GOVT:wnfa']) - 
    (model8.dec$coef['wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)GOVT:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"wnfa-factor(oscode4)GOVT:wnfa=wnfa:factor(dec)1-factor(oscode4)GOVT:wnfa:factor(dec)1")$"Pr(>Chisq)")

#FB-MNC
c((model8.dec$coef['wnfa'] - model8.dec$coef['factor(oscode4)MNC:wnfa']) - 
    (model8.dec$coef['wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"wnfa-factor(oscode4)MNC:wnfa=wnfa:factor(dec)1-factor(oscode4)MNC:wnfa:factor(dec)1")$"Pr(>Chisq)")

#GOVT-MNC
c((model8.dec$coef['factor(oscode4)GOVT:wnfa'] - model8.dec$coef['factor(oscode4)MNC:wnfa']) - 
    (model8.dec$coef['factor(oscode4)GOVT:wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)MNC:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnfa -factor(oscode4)MNC:wnfa=factor(oscode4)GOVT:wnfa:factor(dec)1-factor(oscode4)MNC:wnfa:factor(dec)1")$"Pr(>Chisq)")

#GOVT-NFB
c((model8.dec$coef['factor(oscode4)GOVT:wnfa'] - model8.dec$coef['factor(oscode4)NFB:wnfa']) - 
    (model8.dec$coef['factor(oscode4)GOVT:wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)GOVT:wnfa -factor(oscode4)NFB:wnfa=factor(oscode4)GOVT:wnfa:factor(dec)1-factor(oscode4)NFB:wnfa:factor(dec)1")$"Pr(>Chisq)")

#MNC-NFB
c((model8.dec$coef['factor(oscode4)MNC:wnfa'] - model8.dec$coef['factor(oscode4)NFB:wnfa']) - 
    (model8.dec$coef['factor(oscode4)MNC:wnfa:factor(dec)1'] - model8.dec$coef['factor(oscode4)NFB:wnfa:factor(dec)1']),
  linearHypothesis(model8.dec,"factor(oscode4)MNC:wnfa -factor(oscode4)NFB:wnfa=factor(oscode4)MNC:wnfa:factor(dec)1-factor(oscode4)NFB:wnfa:factor(dec)1")$"Pr(>Chisq)")



############################# CRISIS PERIODS #########################
####################### Crisis 1: Asian Financial Crisis Vs. Previous and Following year###############
data8.afc<-read.csv("afc.csv",header=T)
library(plm)
pdata8.afc=pdata.frame(data8.afc,c("compname","year"))
attach(pdata8.afc)

model8.crisis.afc<-plm(wpat~(factor(bg)*(wnw+wnwc+wnfa)*factor(cri)),data=pdata8.afc,model="random",effect="individual")
summary(model8.crisis.afc)


#### Testing for the differences between PATs ####
## Consecutive years
# Within FBs
c((model8.crisis.afc$coef['factor(cri)2 years before AFC'] -  
     model8.crisis.afc$coef['factor(cri)3 years before AFC']), 
  linearHypothesis(model8.crisis.afc," factor(cri)2 years before AFC = factor(cri)3 years before AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(cri)Year Before AFC'] -  
     model8.crisis.afc$coef['factor(cri)2 years before AFC']), 
  linearHypothesis(model8.crisis.afc," factor(cri)Year Before AFC = factor(cri)2 years before AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(cri)AFC'] -  
     model8.crisis.afc$coef['factor(cri)Year Before AFC']), 
  linearHypothesis(model8.crisis.afc," factor(cri)AFC = factor(cri)Year Before AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(cri)Year After AFC'] -  
     model8.crisis.afc$coef['factor(cri)AFC']), 
  linearHypothesis(model8.crisis.afc," factor(cri)Year After AFC = factor(cri)AFC")$"Pr(>Chisq)")

# To see if the change from 'Year after AFC' and '2 years after AFC' was significant, see the negative of coefficient and its significance for 'Year after AFC'
# To see if the change from '2 years after AFC' and '3 years after AFC' was significant, see the coefficient and its significance for '3 years after AFC'


# Within NFBs
c((model8.crisis.afc$coef['factor(bg)NFB:factor(cri)2 years before AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)3 years before AFC']), 
  linearHypothesis(model8.crisis.afc," factor(bg)NFB:factor(cri)2 years before AFC = factor(bg)NFB:factor(cri)3 years before AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(bg)NFB:factor(cri)Year Before AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)2 years before AFC']), 
  linearHypothesis(model8.crisis.afc," factor(bg)NFB:factor(cri)Year Before AFC = factor(bg)NFB:factor(cri)2 years before AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(bg)NFB:factor(cri)AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)Year Before AFC']), 
  linearHypothesis(model8.crisis.afc," factor(bg)NFB:factor(cri)AFC = factor(bg)NFB:factor(cri)Year Before AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(bg)NFB:factor(cri)Year After AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)AFC']), 
  linearHypothesis(model8.crisis.afc," factor(bg)NFB:factor(cri)Year After AFC = factor(bg)NFB:factor(cri)AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(bg)NFB'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)Year After AFC']), 
  linearHypothesis(model8.crisis.afc," factor(bg)NFB = factor(bg)NFB:factor(cri)Year After AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(bg)NFB:factor(cri)3 years after AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB']), 
  linearHypothesis(model8.crisis.afc," factor(bg)NFB:factor(cri)3 years after AFC = factor(bg)NFB")$"Pr(>Chisq)")


##WIthin each year, are the PATs across ownerships significantly different
c((model8.crisis.afc$coef['factor(cri)3 years before AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)3 years before AFC']), 
  linearHypothesis(model8.crisis.afc," factor(cri)3 years before AFC = factor(bg)NFB:factor(cri)3 years before AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(cri)2 years before AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)2 years before AFC']), 
  linearHypothesis(model8.crisis.afc," factor(cri)2 years before AFC = factor(bg)NFB:factor(cri)2 years before AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(cri)Year Before AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)Year Before AFC']), 
  linearHypothesis(model8.crisis.afc," factor(cri)Year Before AFC = factor(bg)NFB:factor(cri)Year Before AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(cri)AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)AFC']), 
  linearHypothesis(model8.crisis.afc," factor(cri)AFC = factor(bg)NFB:factor(cri)AFC")$"Pr(>Chisq)")

c((model8.crisis.afc$coef['factor(cri)Year After AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)Year After AFC']), 
  linearHypothesis(model8.crisis.afc," factor(cri)Year After AFC = factor(bg)NFB:factor(cri)Year After AFC")$"Pr(>Chisq)")

# to see if the difference is significant in the year '2 years after AFC, check the coefficient of 'factor(bg)NFB'

c((model8.crisis.afc$coef['factor(cri)3 years after AFC'] -  
     model8.crisis.afc$coef['factor(bg)NFB:factor(cri)3 years after AFC']), 
  linearHypothesis(model8.crisis.afc," factor(cri)3 years after AFC = factor(bg)NFB:factor(cri)3 years after AFC")$"Pr(>Chisq)")










####################### Crisis 2: Dot Com Burst Vs. Previous and Following year###############
data8.dcb<-read.csv("dcb.csv",header=T)
library(plm)
pdata8.dcb=pdata.frame(data8.dcb,c("compname","year"))
attach(pdata8.dcb)

model8.crisis.dcb<-plm(wpat~(factor(bg)*(wnw+wnwc+wnfa)*factor(cri)),data=pdata8.dcb,model="random",effect="individual")
summary(model8.crisis.dcb)


#### Testing for the differences between PATs ####
## Consecutive years
# Within FBs
c((model8.crisis.dcb$coef['factor(cri)2 years before DCB'] -  
     model8.crisis.dcb$coef['factor(cri)3 years before DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(cri)2 years before DCB = factor(cri)3 years before DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(cri)Year Before DCB'] -  
     model8.crisis.dcb$coef['factor(cri)2 years before DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(cri)Year Before DCB = factor(cri)2 years before DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(cri)DCB'] -  
     model8.crisis.dcb$coef['factor(cri)Year Before DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(cri)DCB = factor(cri)Year Before DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(cri)Year After DCB'] -  
     model8.crisis.dcb$coef['factor(cri)DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(cri)Year After DCB = factor(cri)DCB")$"Pr(>Chisq)")

# To see if the change from 'Year after DCB' and '2 years after DCB' was significant, see the negative of coefficient and its significance for 'Year after DCB'
# To see if the change from '2 years after DCB' and '3 years after DCB' was significant, see the coefficient and its significance for '3 years after DCB'


# Within NFBs
c((model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)2 years before DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)3 years before DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(bg)NFB:factor(cri)2 years before DCB = factor(bg)NFB:factor(cri)3 years before DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)Year Before DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)2 years before DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(bg)NFB:factor(cri)Year Before DCB = factor(bg)NFB:factor(cri)2 years before DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)Year Before DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(bg)NFB:factor(cri)DCB = factor(bg)NFB:factor(cri)Year Before DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)Year After DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(bg)NFB:factor(cri)Year After DCB = factor(bg)NFB:factor(cri)DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(bg)NFB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)Year After DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(bg)NFB = factor(bg)NFB:factor(cri)Year After DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)3 years after DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB']), 
  linearHypothesis(model8.crisis.dcb," factor(bg)NFB:factor(cri)3 years after DCB = factor(bg)NFB")$"Pr(>Chisq)")


##WIthin each year, are the PATs across ownerships significantly different
c((model8.crisis.dcb$coef['factor(cri)3 years before DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)3 years before DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(cri)3 years before DCB = factor(bg)NFB:factor(cri)3 years before DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(cri)2 years before DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)2 years before DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(cri)2 years before DCB = factor(bg)NFB:factor(cri)2 years before DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(cri)Year Before DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)Year Before DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(cri)Year Before DCB = factor(bg)NFB:factor(cri)Year Before DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(cri)DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(cri)DCB = factor(bg)NFB:factor(cri)DCB")$"Pr(>Chisq)")

c((model8.crisis.dcb$coef['factor(cri)Year After DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)Year After DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(cri)Year After DCB = factor(bg)NFB:factor(cri)Year After DCB")$"Pr(>Chisq)")

# to see if the difference is significant in the year '2 years after DCB, check the coefficient of 'factor(bg)NFB'

c((model8.crisis.dcb$coef['factor(cri)3 years after DCB'] -  
     model8.crisis.dcb$coef['factor(bg)NFB:factor(cri)3 years after DCB']), 
  linearHypothesis(model8.crisis.dcb," factor(cri)3 years after DCB = factor(bg)NFB:factor(cri)3 years after DCB")$"Pr(>Chisq)")







####################### Crisis 3: Global Economic Crisis Vs. Previous and Following year###############
data8.gec<-read.csv("gec.csv",header=T)
library(plm)
pdata8.gec=pdata.frame(data8.gec,c("compname","year"))
attach(pdata8.gec)

model8.crisis.gec<-plm(wpat~(factor(bg)*(wnw+wnwc+wnfa)*factor(cri)),data=pdata8.gec,model="random",effect="individual")
summary(model8.crisis.gec)


#### Testing for the differences between PATs ####
## Consecutive years
# Within FBs
c((model8.crisis.gec$coef['factor(cri)2 years before GEC'] -  
     model8.crisis.gec$coef['factor(cri)3 years before GEC']), 
  linearHypothesis(model8.crisis.gec," factor(cri)2 years before GEC = factor(cri)3 years before GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(cri)Year Before GEC'] -  
     model8.crisis.gec$coef['factor(cri)2 years before GEC']), 
  linearHypothesis(model8.crisis.gec," factor(cri)Year Before GEC = factor(cri)2 years before GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(cri)GEC'] -  
     model8.crisis.gec$coef['factor(cri)Year Before GEC']), 
  linearHypothesis(model8.crisis.gec," factor(cri)GEC = factor(cri)Year Before GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(cri)Year After GEC'] -  
     model8.crisis.gec$coef['factor(cri)GEC']), 
  linearHypothesis(model8.crisis.gec," factor(cri)Year After GEC = factor(cri)GEC")$"Pr(>Chisq)")



# Within NFBs
c((model8.crisis.gec$coef['factor(bg)NFB:factor(cri)2 years before GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)3 years before GEC']), 
  linearHypothesis(model8.crisis.gec," factor(bg)NFB:factor(cri)2 years before GEC = factor(bg)NFB:factor(cri)3 years before GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(bg)NFB:factor(cri)Year Before GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)2 years before GEC']), 
  linearHypothesis(model8.crisis.gec," factor(bg)NFB:factor(cri)Year Before GEC = factor(bg)NFB:factor(cri)2 years before GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(bg)NFB:factor(cri)GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)Year Before GEC']), 
  linearHypothesis(model8.crisis.gec," factor(bg)NFB:factor(cri)GEC = factor(bg)NFB:factor(cri)Year Before GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(bg)NFB:factor(cri)Year After GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)GEC']), 
  linearHypothesis(model8.crisis.gec," factor(bg)NFB:factor(cri)Year After GEC = factor(bg)NFB:factor(cri)GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(bg)NFB'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)Year After GEC']), 
  linearHypothesis(model8.crisis.gec," factor(bg)NFB = factor(bg)NFB:factor(cri)Year After GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(bg)NFB:factor(cri)3 years after GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB']), 
  linearHypothesis(model8.crisis.gec," factor(bg)NFB:factor(cri)3 years after GEC = factor(bg)NFB")$"Pr(>Chisq)")


##WIthin each year, are the PATs across ownerships significantly different
c((model8.crisis.gec$coef['factor(cri)3 years before GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)3 years before GEC']), 
  linearHypothesis(model8.crisis.gec," factor(cri)3 years before GEC = factor(bg)NFB:factor(cri)3 years before GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(cri)2 years before GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)2 years before GEC']), 
  linearHypothesis(model8.crisis.gec," factor(cri)2 years before GEC = factor(bg)NFB:factor(cri)2 years before GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(cri)Year Before GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)Year Before GEC']), 
  linearHypothesis(model8.crisis.gec," factor(cri)Year Before GEC = factor(bg)NFB:factor(cri)Year Before GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(cri)GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)GEC']), 
  linearHypothesis(model8.crisis.gec," factor(cri)GEC = factor(bg)NFB:factor(cri)GEC")$"Pr(>Chisq)")

c((model8.crisis.gec$coef['factor(cri)Year After GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)Year After GEC']), 
  linearHypothesis(model8.crisis.gec," factor(cri)Year After GEC = factor(bg)NFB:factor(cri)Year After GEC")$"Pr(>Chisq)")

# to see if the difference is significant in the year '2 years after GEC, check the coefficient of 'factor(bg)NFB' (p-value: 0.148426)

c((model8.crisis.gec$coef['factor(cri)3 years after GEC'] -  
     model8.crisis.gec$coef['factor(bg)NFB:factor(cri)3 years after GEC']), 
  linearHypothesis(model8.crisis.gec," factor(cri)3 years after GEC = factor(bg)NFB:factor(cri)3 years after GEC")$"Pr(>Chisq)")




##################### REGIONWISE ANALYSIS #####################
#This model will not work with either oscode or oscode4 because NFB&NFBG have 0 comapnies in East
model8.reg<-plm(wpat~(wnw+wnwc+wnfa)*factor(bg)*factor(rgcode)+as.numeric(year),data=pdata8,model="random",effect="individual",na.action="na.omit")
summary(model8.reg)

#Error plot
yhat.reg<-model8.reg$model[[1]] - model8.reg$residuals
e.reg<-model8.reg$residuals
std<-function(x) {(x-mean(x))/sqrt(var(x))}
e.std.reg<-std(e.reg)
plot(yhat.reg,e.std.reg,col="Blue",main="Residual plot: Regionwise analysis",ylab="Standardised residuals",xlab="Fitted values",pch=21)

#Normal probability plot
qqnorm(e.std.reg)
qqline(e.std.reg)

#Collinearity diagnostic
colldiag(model.matrix(model8.reg),add.intercept=FALSE)



###################Testing differences################
#######Difference in PAT values across various regions#########
######Within FBs##################
c((model8.reg$coef['factor(rgcode)EAST'] -  
     model8.reg$coef['factor(rgcode)SOUTH']), 
  linearHypothesis(model8.reg," factor(rgcode)EAST = factor(rgcode)SOUTH")$"Pr(>Chisq)")

c((model8.reg$coef['factor(rgcode)WEST'] -  
     model8.reg$coef['factor(rgcode)EAST']), 
  linearHypothesis(model8.reg," factor(rgcode)WEST = factor(rgcode)EAST")$"Pr(>Chisq)")

c((model8.reg$coef['factor(rgcode)WEST'] -  
     model8.reg$coef['factor(rgcode)SOUTH']), 
  linearHypothesis(model8.reg," factor(rgcode)WEST = factor(rgcode)SOUTH")$"Pr(>Chisq)")


#########Within NFBs##############
c(((model8.reg$coef['factor(bg)NFB']+model8.reg$coef['(Intercept)']) -  
     (model8.reg$coef['factor(bg)NFB:factor(rgcode)EAST']+model8.reg$coef['factor(rgcode)EAST'])), 
  linearHypothesis(model8.reg,"factor(bg)NFB+(Intercept) = factor(bg)NFB:factor(rgcode)EAST+factor(rgcode)EAST")$"Pr(>Chisq)")

c(((model8.reg$coef['factor(bg)NFB']+model8.reg$coef['(Intercept)']) -  
     (model8.reg$coef['factor(bg)NFB:factor(rgcode)SOUTH']+model8.reg$coef['factor(rgcode)SOUTH'])), 
  linearHypothesis(model8.reg,"factor(bg)NFB+(Intercept) = factor(bg)NFB:factor(rgcode)SOUTH+factor(rgcode)SOUTH")$"Pr(>Chisq)")

c(((model8.reg$coef['factor(bg)NFB']+model8.reg$coef['(Intercept)']) -  
     (model8.reg$coef['factor(bg)NFB:factor(rgcode)WEST']+model8.reg$coef['factor(rgcode)WEST'])), 
  linearHypothesis(model8.reg,"factor(bg)NFB+(Intercept) = factor(bg)NFB:factor(rgcode)WEST+factor(rgcode)WEST")$"Pr(>Chisq)")


c(((model8.reg$coef['factor(bg)NFB:factor(rgcode)WEST']+model8.reg$coef['factor(rgcode)WEST']) -  
     (model8.reg$coef['factor(bg)NFB:factor(rgcode)EAST']+model8.reg$coef['factor(rgcode)EAST'])), 
  linearHypothesis(model8.reg,"factor(bg)NFB:factor(rgcode)WEST+factor(rgcode)WEST = factor(bg)NFB:factor(rgcode)EAST+factor(rgcode)EAST")$"Pr(>Chisq)")

c(((model8.reg$coef['factor(bg)NFB:factor(rgcode)SOUTH']+model8.reg$coef['factor(rgcode)SOUTH']) -  
     (model8.reg$coef['factor(bg)NFB:factor(rgcode)EAST']+model8.reg$coef['factor(rgcode)EAST'])), 
  linearHypothesis(model8.reg,"factor(bg)NFB:factor(rgcode)SOUTH+factor(rgcode)SOUTH = factor(bg)NFB:factor(rgcode)EAST+factor(rgcode)EAST")$"Pr(>Chisq)")

c(((model8.reg$coef['factor(bg)NFB:factor(rgcode)WEST']+model8.reg$coef['factor(rgcode)WEST']) -  
     (model8.reg$coef['factor(bg)NFB:factor(rgcode)SOUTH']+model8.reg$coef['factor(rgcode)SOUTH'])), 
  linearHypothesis(model8.reg,"factor(bg)NFB:factor(rgcode)WEST+factor(rgcode)WEST = factor(bg)NFB:factor(rgcode)SOUTH+factor(rgcode)SOUTH")$"Pr(>Chisq)")




#################### INDUSTRYWISE ANALYSIS #######################
model8.ind<-lme(wpat~(wnw+wnwc+wnfa)*factor(bg)*factor(indccode)+factor(year),data=pdata8,random=~1|compname,correlation=corAR1(),na.action="na.omit")
summary(model8.ind)

model8.ind<-lme(wpat~(wnw+wnwc+wnfa)*factor(oscode4)+(wnw+wnwc+wnfa)*factor(indccode)+factor(year),data=pdata8,random=~1|compname,correlation=corAR1(),na.action="na.omit")
summary(model8.ind)




##################### Conglomerate Vs. Standalone Companies ####################
model8.cong<-plm(wpat~(wnw+wnwc+wnfa)*factor(cong)+factor(year),data=pdata8,model="random",effect="individual",na.action=na.omit)
summary(model8.cong)

#Residual plots
predicted.cong<-model8.cong$model[[1]] - model8.cong$residuals
e.cong<-residuals(model8.cong)
std<-function(x) {(x-mean(x))/sqrt(var(x))}
e.std.cong<-std(e.cong)
plot(predicted.cong,e.std.cong,xlab="Fitted values",ylab="Standardized residuals",col="BLUE",pch=19,main="Residual Plot: Conglomerates vs. standalone companies")

#Normal probability plot
qqnorm(e.std.cong,main="Normal Q-Q plot: Conglomerates vs. standalone companies")
qqline(e.std.cong)

#Collinearity diagnostic
colldiag(model.matrix(model8.cong),add.intercept=FALSE)


#### FB conglomerates are base ####
################Testing for difference in PAT values ################
c((model8.cong$coef['factor(cong)Others'] -  
     model8.cong$coef['factor(cong)FBNC']), 
  linearHypothesis(model8.cong," factor(cong)Others = factor(cong)FBNC")$"Pr(>Chisq)")






########################## 2 ownerships: Fixed Effects ##################
data8.2<-read.csv("min8full_region.csv",header=T)
pdata8.2=pdata.frame(data8.2,c("compname","year"))
attach(pdata8.2)

model2.re=plm(wpat~factor(oscode4)*wnwc+factor(oscode4)*wnfa+factor(oscode4)*wnw+factor(year),data=pdata8.2,model="random",effect="individual",na.action=na.omit)
summary(model2.re)


########################## Time effect combined/ Trend ##########################
model8.reg<-lme(wpat~(wnw+wnwc+wnfa)*factor(bg)*factor(rgcode)+as.numeric(year),data=pdata8,random=~1|compname,correlation=corAR1(),na.action="na.omit")
summary(model8.reg)

model8.cong<-plm(wpat~(wnw+wnwc+wnfa)*factor(cong)+as.numeric(year),data=pdata8,model="random",effect="individual",na.action=na.omit)
summary(model8.cong)

#Without intercept:did not work because we ourselves did not like the idea
model8.cong.ft<-plm(wpat~factor(year)+(wnw+wnwc+wnfa)*factor(cong)-1,data=pdata8,model="random",effect="individual",na.action=na.omit)
summary(model8.cong.ft)




model8.re=plm(wpat~factor(oscode4)*(wnwc+wnfa+wnw)+as.numeric(year),data=pdata8,model="random",effect="individual",na.action=na.omit)
summary(model8.re)

#Residual plots
predicted.re<-model8.re$model[[1]] - model8.re$residuals
e.re.4<-residuals(model8.re)
std<-function(x) {(x-mean(x))/sqrt(var(x))}
e.std.re<-std(e.re)
plot(predicted.re,e.std.re,xlab="Fitted values",ylab="Standardized residuals",col="RED",pch=19,main="Residual Plot: Time trend")

#Normal probability plot
qqnorm(e.std.re,main="Normal Q-Q plot: Fixed time effects")
qqline(e.std.re)

#Collinearity diagnostic
colldiag(model.matrix(model8.re),add.intercept=FALSE)
