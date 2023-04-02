edxc <- read.csv('appendix.csv')

############ Preliminary Findings  ############
colnames(edxc)
hist(edxc$X..Audited)
summary(edxc)

boxplot(edxc$Median.Hours.for.Certification~edxc$Institution,horizontal=TRUE)
boxplot(edxc$Total.Course.Hours..Thousands.~edxc$Institution,horizontal=TRUE)

boxplot(edxc$X..Audited~edxc$Institution,horizontal=TRUE)
boxplot(edxc$X..Audited~edxc$Course.Subject,horizontal=TRUE)
boxplot(edxc$X..Audited~edxc$Year,horizontal=TRUE)
boxplot(edxc$X..Audited~edxc$Honor.Code.Certificates,horizontal=TRUE)


plot(y=edxc$X..Audited,x=edxc$Participants..Course.Content.Accessed.)
plot(y=edxc$X..Audited,x=edxc$X..Played.Video)
plot(y=edxc$X..Audited,x=edxc$X..Posted.in.Forum)
plot(y=edxc$X..Audited,x=edxc$X..Grade.Higher.Than.Zero)
plot(y=edxc$X..Audited,x=edxc$Total.Course.Hours..Thousands.)
plot(y=edxc$X..Audited,x=edxc$Median.Hours.for.Certification)
plot(y=edxc$X..Audited,x=edxc$Median.Age)
plot(y=edxc$X..Audited,x=edxc$X..Male)
plot(y=edxc$X..Audited,x=edxc$X..Bachelor.s.Degree.or.Higher)


############ Generalized Linear Models (Gamma) ############
library('glm2')
library('MASS')

# removing outline (the course of CS50 which has more than 100000 participants)
edxc_clean<-edxc[edxc$Course.Number!="CS50x",]


# model fitting
model<-X..Audited~Institution+Course.Subject+Participants..Course.Content.Accessed.+Median.Hours.for.Certification+X..Posted.in.Forum+X..Female+X..Bachelor.s.Degree.or.Higher+X..Posted.in.Forum:Median.Hours.for.Certification+X..Posted.in.Forum:X..Bachelor.s.Degree.or.Higher
fit<-glm2(model,
          family=Gamma(link='log'),
          data=edxc_clean)
summary(fit)

# residual plot
rss<-sum(resid(fit)^2)
plot(y=fit$residuals,x=fit$fitted.values,ylim=c(-1.5,1.5))
abline(h=c(0),col='red',lty=2,lwd=2)
