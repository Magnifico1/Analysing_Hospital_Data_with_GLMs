library(nnet)
library(tidyverse)
admit <- read.csv("admissions.csv")

admit<-data.frame(admit[,c(1,2,3,5,4,6,7)])
admit$died<-as.factor(admit$died)
admit$white<-as.factor(admit$white)
admit$age80<-as.factor(admit$age80)
admit$age<-as.factor(admit$age)
admit$admission<-as.factor(admit$admission)
levels(admit$admission)

#relationship between race and admission type
ggplot(admit)+geom_bar(aes(x=admission,fill=admission))+facet_wrap(~white,scales="free")

#relationship between death and admission type
death<-admit[admit$died==1,]
ggplot(death)+geom_bar(aes(x=admission))

barplot(with(admit,table(died,admission)),beside=T,col=2:3)

ggplot(data=admit, aes(x=admission, y=fill=died)) +
  geom_bar(stat="identity", position=position_dodge())

death<-data.frame(table(admit$admission,admit$died))
colnames(death)<-c("Admission","Deaths", "Frequency")
ggplot(death,aes(x=Admission,y=Frequency,fill=Deaths))+
  geom_bar(position="dodge",stat="identity")+
  scale_fill_manual("Outcome", values = c("0"="green3", "1"="red2"))

#relationship between age80 and admission type
ggplot(admit)+geom_bar(aes(x=admission,fill=admission))+facet_wrap(~age80,scales="free")

#relationship between los and admission type
ggplot(admit)+geom_histogram(aes(x=los,fill=admission),stat="count")+
  facet_wrap(~admission,scales="free")+theme(axis.text.x=element_blank())

#fit multinomial model
mn.full<-multinom(admission~died+white+age+age80+los, data=admit)
summary(mn.full)

#check collinearity
require(car)
vif(mn.full)

#check significant parameters
Anova(mn.full)

#model selection by AIC
step(mn.full,direction="both")

require(MuMIn)
options(na.action='na.fail')
head(dredge(mn.full))

#fit reduced model based on ANOVA and stepwise selection
mn.red<-multinom(as.factor(admission)~as.factor(died)+as.factor(white)+los, data=admit)
summary(mn.red)

#check coefficients
coef(mn.red)

#p_ij
p_ij<-predict(mn.red,newdata=data.frame(white=0,died=0,los=0), type="probs")

#calculate log odds of emergency vs elective when variables all 0
log(p_ij["Emergency"]/p_ij["Elective"])
coef(mn.red)[1,1]
#odds of emergency vs elective 
exp(coef(mn.red)[1,1])

#calculate log odds of urgent vs elective when variables all 0
log(p_ij["Urgent"]/p_ij["Elective"])
coef(mn.red)[2,1]
#odds of urgent vs elective 
exp(coef(mn.red)[2,1])

#calculate log odds of emergency vs urgent when variables all 0
log(p_ij["Emergency"]/p_ij["Urgent"])
log(exp(coef(mn.red)[1,1])/exp(coef(mn.red)[2,1]))
#odds of emergency vs urgent 
exp(coef(mn.red)[1,1])/exp(coef(mn.red)[2,1])

#predicting admission probabilities by length of stay for white/died=0/1
los<-1:116
pred<-data.frame(predict(mn.red,newdata=data.frame(died=0,white=0,los=los),
              type="probs"))

pred2<-data.frame(predict(mn.red,newdata=data.frame(died=1,white=0,los=los),
              type="probs"))

pred3<-data.frame(predict(mn.red,newdata=data.frame(died=0,white=1,los=los),
               type="probs"))

pred4<-data.frame(predict(mn.red,newdata=data.frame(died=1,white=1,los=los),
               type="probs"))

#plot probabilities for the four combinations of white/died
ggplot(pred)+geom_line(aes(x=1:116,y=Elective,colour="Elective"),size=1.5)+
  geom_line(aes(x=1:116,y=Emergency,colour="Emergency"),size=1.5)+
  geom_line(aes(x=1:116,y=Urgent,colour="Urgent"),size=1.5)+
  scale_colour_manual("", 
                      breaks = c("Elective", "Emergency", "Urgent"),
                      values = c("green3", "red", "blue"))+
  xlab("Length of Stay (Days)")+
  ylab("Probability")+
  ylim(c(0,1))+
  ggtitle("Admission Type Probability by Length of Stay for Alive Non-Whites")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(pred2)+geom_line(aes(x=1:116,y=Elective,colour="Elective"),size=1.5)+
  geom_line(aes(x=1:116,y=Emergency,colour="Emergency"),size=1.5)+
  geom_line(aes(x=1:116,y=Urgent,colour="Urgent"),size=1.5)+
  scale_colour_manual("", 
                      breaks = c("Elective", "Emergency", "Urgent"),
                      values = c("green3", "red", "blue"))+
  xlab("Length of Stay (Days)")+
  ylab("Probability")+
  ylim(c(0,1))+
  ggtitle("Admission Type Probability by Length of Stay for Dead Non-whites")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(pred3)+geom_line(aes(x=1:116,y=Elective,colour="Elective"),size=1.5)+
  geom_line(aes(x=1:116,y=Emergency,colour="Emergency"),size=1.5)+
  geom_line(aes(x=1:116,y=Urgent,colour="Urgent"),size=1.5)+
  scale_colour_manual("", 
                      breaks = c("Elective", "Emergency", "Urgent"),
                      values = c("green3", "red", "blue"))+
  xlab("Length of Stay (Days)")+
  ylab("Probability")+
  ylim(c(0,1))+
  ggtitle("Admission Type Probability by Length of Stay for Alive Whites")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(pred4)+geom_line(aes(x=1:116,y=Elective,colour="Elective"),size=1.5)+
  geom_line(aes(x=1:116,y=Emergency,colour="Emergency"),size=1.5)+
  geom_line(aes(x=1:116,y=Urgent,colour="Urgent"),size=1.5)+
  scale_colour_manual("", 
                      breaks = c("Elective", "Emergency", "Urgent"),
                      values = c("green3", "red", "blue"))+
  xlab("Length of Stay (Days)")+
  ylab("Probability")+
  labs(colour="Legend")+
  ylim(c(0,1))+
  ggtitle("Admission Type Probability by Length of Stay for Dead Whites")+
  theme(plot.title = element_text(hjust = 0.5))

#Graphical Assessment
library(VGAM)
vglm.red<-vglm(admission~as.factor(died)+as.factor(white)+los,
               family=multinomial,data=admit)
res<-residuals(vglm.red)

mn.red.resid<-residuals(mn.red)
mn.red.fitted<-fitted(mn.red)
mn.red.observed<-mn.red.fitted+mn.red.resid
par(mfrow=c(1,3))
for(i in 1:3){
  plot(mn.red.fitted[,i],mn.red.observed[,i],main=colnames(mn.red.fitted)[i],xlab="fitted p",ylab="observed p")
  abline(a=0,b=1)
  lines(smooth.spline(mn.red.fitted[,i],mn.red.observed[,i],df=4),col="red",lty=2)}

#Numerical Assessment
##chi-squared
dev<-deviance(mn.red)
n<-dim(admit)[1]
p<-length(coef(mn.red))
p.val<-1-pchisq(dev,n-p)
##McFadden's R^2
vglm.null<-vglm(admission~1,family=multinomial,dat=admit)
Rsq_mcf<-1-logLik(vglm.red)/logLik(vglm.null)

#Effects plots
require(effects)
mn.red<-multinom(admission~died+white+los, data=admit)
summary(mn.red)
plot(effect("los", mn.red),xlab="Length of Stay",ylab="Probability of Admission Type")
plot(effect("died", mn.red),xlab="Died",ylab="Probability of Admission Type")
plot(effect("white", mn.red),xlab="White",ylab="Probability of Admission Type")

#Assumption-check code (Pearson residuals do not show random scatter)
par(mfrow=c(1,3))
# convert elective to 1, else other
admit$admissionel<-as.factor(ifelse(admit$admission=='Elective', 1, 0))
b1<-glm(admissionel~los+white+died, dat=admit, family=binomial)
plot(fitted(b1), residuals(b1, type='pearson'), main='Elective')
abline(h=0)

# convert urgent to 1, else other
admit$admissionur<-as.factor(ifelse(admit$admission=='Urgent', 1, 0))
b2<-glm(admissionur~los+white+died, dat=admit, family=binomial)
plot(fitted(b2), residuals(b2, type='pearson'), main='Urgent')
abline(h=0)

# convert emergency to 1, else other
admit$admissionem<-as.factor(ifelse(admit$admission=='Emergency', 1, 0))
b3<-glm(admissionem~los+white+died, dat=admit, family=binomial)
plot(fitted(b3), residuals(b3, type='pearson'), main='Emergency')
abline(h=0)

par(mfrow=c(1,3))
plot(residuals(mn.red)[1:100,1], ylab='raw residuals',main='Elective')
plot(residuals(mn.red)[1:100,2], ylab='raw residuals',main='Emergency')
plot(residuals(mn.red)[1:100,3], ylab='raw residuals',main='Urgent')

par(mfrow=c(1,3))
predf<-predict(mn.red, newdata=admit, type='probs')
# log odds is log(pj/p1) where p1 is elective
pEmelec<-log(predf[,2]/predf[,1])
plot(admit$los, pEmelec, xlab='Length of Stay (days)', ylab='Log Odds', main='Emergency vs Elective')

pUrgelec<-log(predf[,3]/predf[,1])
plot(admit$los, pUrgelec, xlab='Length of Stay (days)', ylab='Log Odds', main='Urgent vs Elective')

pEmUrg<-log(predf[,2]/predf[,3])
plot(admit$los, pEmUrg, xlab='Length of Stay (days)', ylab='Log Odds', main='Emergency vs Urgent')


