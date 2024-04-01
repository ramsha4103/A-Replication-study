#Replication:
load("datamatch.Rdata") #set working directly to downloads
summary(datamatch)
data<- na.omit(datamatch)
  
#matching:
library(Matching)
covariates <- data.frame(data$age.group, data$educ, data$white.collar, data$not.full.time, data$male, data$tech, data$pol.info)
EV <- data[,2]
glm1= glm(EV~eval.voting+easy.voting+sure.counted+conf.secret+how.clean+speed+agree.evoting+eselect.cand, data = data)
datamatched<-Match(Y=glm1$fitted,Tr=EV, X=covariates)
mb <- MatchBalance(EV~eval.voting+easy.voting+sure.counted+conf.secret+how.clean+speed+agree.evoting+eselect.cand, data = data, match.out=datamatched, nboots=500)

datamatched<-Match(Tr=EV, X=covariates)

capable.auth <- data$capable.auth
mout_capable.auth<-Match(Y=capable.auth,Tr=EV, X=covariates)

eval.voting <- data$eval.voting
mout_eval.voting<-Match(Y=eval.voting,Tr=EV, X=covariates)

easy.voting <- data$easy.voting
mout_easy.voting<-Match(Y=easy.voting,Tr=EV, X=covariates)

sure.counted <-data$sure.counted
mout_sure.counted<-Match(Y=sure.counted,Tr=EV, X=covariates)

conf.secret<- data$conf.secret
mout_conf.secret<-Match(Y=conf.secret,Tr=EV, X=covariates)

how.clean<- data$how.clean
mout_how.clean<-Match(Y=how.clean,Tr=EV, X=covariates)

speed<- data$speed
mout_speed<-Match(Y=speed,Tr=EV, X=covariates)

agree.evoting<- data$agree.evoting
mout_agree.evoting<-Match(Y=agree.evoting,Tr=EV, X=covariates)

eselect.cand<- data$eselect.cand
mout_eselect.cand<-Match(Y=eselect.cand,Tr=EV, X=covariates)

Extention_GenMatch:
covariates <- data.frame( data$age.group, data$educ, data$white.collar, data$not.full.time, data$male, data$tech, data$pol.info)
genout <- GenMatch(X = covariates, Tr = EV,pop.size=100,max.generation=50, wait.generations=4)

# DEP VAR 1
eval.voting <- data$eval.voting
mout_eval.voting <- Match(Y = eval.voting, 
                          X = covariates, 
                          Tr = EV, 
                          Weight.matrix = genout)
summary(mout_eval.voting)

# DEP VAR 2
easy.voting <- data$easy.voting
mout_eval.voting <- Match(Y = easy.voting, 
                          X = covariates, 
                          Tr = EV, 
                          Weight.matrix = genout)
summary(mout_easy.voting)

# DEP VAR 3
capable.auth <- data$capable.auth
mout_capable.auth <- Match(Y = capable.auth, 
                          X = covariates, 
                          Tr = EV, 
                          Weight.matrix = genout)
summary(mout_capable.auth)

# DEP VAR 4
sure.counted <- data$sure.counted
mout_sure.counted <- Match(Y = sure.counted, 
                          X = covariates, 
                          Tr = EV, 
                          Weight.matrix = genout)
summary(mout_sure.counted)

# DEP VAR 5
conf.secret <- data$conf.secret
mout_conf.secret <- Match(Y = conf.secret, 
                          X = covariates, 
                          Tr = EV, 
                          Weight.matrix = genout)
summary(mout_conf.secret)

# DEP VAR 6
how.clean <- data$how.clean
mout_how.clean <- Match(Y = how.clean, 
                          X = covariates, 
                          Tr = EV, 
                          Weight.matrix = genout)
summary(mout_how.clean)

# DEP VAR 7
speed <- data$speed
mout_speed <- Match(Y = speed, 
                          X = covariates, 
                          Tr = EV, 
                          Weight.matrix = genout)
summary(mout_speed)

# DEP VAR 8
agree.evoting <- data$agree.evoting
mout_agree.evoting <- Match(Y = agree.evoting, 
                          X = covariates, 
                          Tr = EV, 
                          Weight.matrix = genout)
summary(mout_agree.evoting)

# DEP VAR 9
eselect.cand <- data$eselect.cand
mout_eselect.cand <- Match(Y = eselect.cand, 
                          X = covariates, 
                          Tr = EV, 
                          Weight.matrix = genout)
summary(mout_eselect.cand)
