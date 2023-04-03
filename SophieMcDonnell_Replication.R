#Replication File
#Women’s Political Representation in African Rebel Parties
#Conditionally accepted at Journal of Politics
#Elizabeth L. Brannon


#Load packages
library(ggplot2)
library(extrafont)
library(lme4)
library(Hmisc)
library(stargazer)
library(faraway)
library(tidyverse)
library(broom)
library(lme4)
library(arm)
library(MASS)
library(lmerTest)
library(censReg)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(texreg)



#Load data & name data as winners
winners <- read.csv("JOP_ReplicationData.csv")


#Descriptive Table in RD
t.test(winners$Percent_Women_Seats[winners$Rebel_Party==0], winners$Percent_Women_Seats[winners$Rebel_Party==1])

t.test(winners$Percent_Women_Candidates[winners$Rebel_Party==0], winners$Percent_Women_Candidates[winners$Rebel_Party==1])


###Model 1 -- candidacy

##Drop NAs for models & simulations
omit <- winners[,c("Percent_Women_Candidates", "Rebel_Party",   
                   "democ",
                   "Adopted_Quotas",
                   "exclusive",
                   "opposition",
                   "Party_Institutionalization",
                   "gdplnvdem",
                   "vdem_wom_civsoc", "spline2", "spline3", "spline4", "Country")]
omit <- na.omit(omit)
modelcan <- lmerTest::lmer(Percent_Women_Candidates ~ 
                             Rebel_Party +  
                             democ +
                             Adopted_Quotas +
                             exclusive +
                             opposition+
                             Party_Institutionalization+
                             gdplnvdem  +
                             vdem_wom_civsoc+
                             spline2 + 
                             spline3 + 
                             spline4  + 
                             (1 | Country), 
                           data=omit)

###Simulation, Figure 1
n.draws<-100
sim.model<-coef(sim(modelcan, n.draws))  %>% data.frame() %>% data.matrix()
sq<-seq(from=min(na.omit(omit$Percent_Women_Candidates)), to=max(na.omit(omit$Percent_Women_Candidates)), length.out=100)
sq.x.lo<-data.frame(
  intercept=1,
  Rebel_Party=0,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

sq.x.hi<-data.frame(
  intercept=1,
  Rebel_Party=1,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

pe.x.hi <- as.matrix(NA, nrow= n.draws)
pe.x.lo <- as.matrix(NA, nrow= n.draws)

hi.x.hi <- as.matrix(NA, nrow= n.draws)
hi.x.lo <- as.matrix(NA, nrow= n.draws)

lo.x.hi <- as.matrix(NA, nrow= n.draws)
lo.x.lo <- as.matrix(NA, nrow= n.draws)

sq.x.hi <- as.matrix(sq.x.hi)
sq.x.lo <- as.matrix(sq.x.lo)
#Fill the vectors

for(j in 1:n.draws){
  pe.x.hi[j] <- mean((sq.x.hi %*% sim.model[j,]), na.rm=T)
  pe.x.lo[j] <- mean((sq.x.lo %*% sim.model[j,]), na.rm=T)
  
}

dif.fun <- function(x) {
  result <- c(quantile(x, 0.025), mean(x), quantile(x,.975))
  return(result)
}
pe.x.hi <- dif.fun(pe.x.hi)
pe.x.lo <- dif.fun(pe.x.lo)

preds <- as.data.frame(rbind( pe.x.lo, pe.x.hi))


rownames(preds) <- c('Non-Rebel Party',"Rebel Party")
preds$levels <- rownames(preds)
preds$levels <- factor(preds$levels, levels=unique(preds$levels))

preds$lb <- preds$`2.5%`
preds$ub <- preds$`97.5%`

#Plot estimates
ggplot() + 
  geom_point(data = preds
             ,aes(x=preds$levels, y=preds$V2)
             ,size = 5
             ,color = "black") + 
  geom_segment(aes(x = preds$levels
                   , y = preds$ub
                   , xend = preds$levels
                   , yend = preds$lb)
               , color = "black"
               , size = 1
               , data = preds
               , lty= c(1,1)) +
  theme_bw() + 
  theme(axis.text = element_text(size=15), axis.title=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio=1.25) +
  xlab('') + 
  ylab("E(Proportion of Women Candidates)")
dev.off()


###Model 2 -- Election
##Drop NAs for models & simulations
omit <- winners[,c("Percent_Women_Seats", "Rebel_Party", "logPercent",   
                   "democ",
                   "Adopted_Quotas",
                   "exclusive",
                   "opposition",
                   "Party_Institutionalization",
                   "gdplnvdem",
                   "vdem_wom_civsoc", "spline2", "spline3", "spline4", "Country")]
omit <- na.omit(omit)

modelele <- lmerTest::lmer(Percent_Women_Seats ~ 
                             Rebel_Party 
                           + logPercent
                           + democ
                           + Adopted_Quotas 
                           + exclusive 
                           + opposition
                           + Party_Institutionalization
                           + gdplnvdem  
                           + vdem_wom_civsoc
                           + spline2 
                           + spline3 
                           + spline4  
                           + (1 | Country), data=omit)
#table
texreg(list(modelcan, modelele))

#Simulation, Figure 2
n.draws<-100
sim.model<-coef(sim(modelele, n.draws))  %>% data.frame() %>% data.matrix()
sq<-seq(from=min(na.omit(omit$Percent_Women_Seats)), to=max(na.omit(omit$Percent_Women_Seats)), length.out=100)
sq.x.lo<-data.frame(
  intercept=1,
  Rebel_Party=0,
  logPercent=omit$logPercent,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

sq.x.hi<-data.frame(
  intercept=1,
  Rebel_Party=1,
  logPercent=omit$logPercent,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

pe.x.hi <- as.matrix(NA, nrow= n.draws)
pe.x.lo <- as.matrix(NA, nrow= n.draws)

hi.x.hi <- as.matrix(NA, nrow= n.draws)
hi.x.lo <- as.matrix(NA, nrow= n.draws)

lo.x.hi <- as.matrix(NA, nrow= n.draws)
lo.x.lo <- as.matrix(NA, nrow= n.draws)


sq.x.hi <- as.matrix(sq.x.hi)
sq.x.lo <- as.matrix(sq.x.lo)
#Fill the vectors

for(j in 1:n.draws){
  pe.x.hi[j] <- mean((sq.x.hi %*% sim.model[j,]), na.rm=T)
  pe.x.lo[j] <- mean((sq.x.lo %*% sim.model[j,]), na.rm=T)
  
  #first differences
}

dif.fun <- function(x) {
  result <- c(quantile(x, 0.025), mean(x), quantile(x,.975))
  return(result)
}
pe.x.hi <- dif.fun(pe.x.hi)
pe.x.lo <- dif.fun(pe.x.lo)

preds <- as.data.frame(rbind(pe.x.lo, pe.x.hi))

rownames(preds) <- c('Non-Rebel Party',"Rebel Party")
preds$levels <- rownames(preds)
preds$levels <- factor(preds$levels, levels=unique(preds$levels))


ggplot() + 
  geom_point(data = preds
             ,aes(x=preds$levels, y=preds$V2)
             ,size = 4
             ,color = "black") + 
  geom_segment(aes(x = preds$levels
                   , y = preds$`2.5%`
                   , xend = preds$levels
                   , yend = preds$`97.5%`)
               , color = "black"
               , size = 1
               , data = preds
               , lty= c(1,1)) +
  theme_bw() +  geom_hline(yintercept=c(0), linetype="dotted") +
  theme(axis.text = element_text(size=15), axis.title=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio=1.25) +
  xlab('') + 
  ylab("E(Proportion of Women Elected)")
dev.off()

###Model for Figure 3
m4data <- winners[,c("Percent_Women_Seats", "Rebel_Party","logPercent", "democ", "Adopted_Quotas",  "Party_Institutionalization",  "exclusive", "gdplnvdem","vdem_wom_civsoc", "opposition",  "spline2", "spline3", "spline4", "Country","Time")]
omit <- na.omit(m4data)
omit$time_sq <- omit$Time^2

modelele.time3 <- lmerTest::lmer(Percent_Women_Seats ~ 
                                   Rebel_Party 
                                 +logPercent
                                 + democ
                                 + Adopted_Quotas 
                                 + exclusive 
                                 + opposition
                                 + Party_Institutionalization
                                 + gdplnvdem  
                                 + vdem_wom_civsoc
                                 + Time  
                                 + time_sq 
                                 + spline2 +spline3 + spline4  + (1 | Country), data=omit)
summary(modelele.time3) 

n.draws2 <- 100
sq2<-seq(from=min(na.omit(omit$Time)), to=max(na.omit(omit$Time)), length.out=100)
sim.fem.mem <- coef(sim(modelele.time3, n = 100))  %>% data.frame() %>% data.matrix()

# Empty vectors to store the point estimates and confidence intervals
pe.ov <- as.matrix(NA, nrow=length(sq2))
lo.ov <- as.matrix(NA, nrow=length(sq2)) 
hi.ov <- as.matrix(NA, nrow=length(sq2))

pe.ov.change <- as.matrix(NA, nrow=length(sq2))
lo.ov.change <- as.matrix(NA, nrow=length(sq2)) 
hi.ov.change <- as.matrix(NA, nrow=length(sq2))


for(j in 1:length(sq2)){ # Loop goes across distance values
  # Set the other independent variables to all of their observed values
  x.distance <- data.frame(intercept = 1,
                           Rebel_Party=omit$Rebel_Party,
                           logPercent=omit$logPercent,
                           democ=omit$democ,
                           Adopted_Quotas=omit$Adopted_Quotas,
                           exclusive=omit$exclusive,
                           opposition=omit$opposition,
                           Party_Institutionalization=omit$Party_Institutionalization,
                           gdplnvdem=omit$gdplnvdem,
                           vdem_wom_civsoc=omit$vdem_wom_civsoc,
                           Time=sq2[j],
                           time_sq=sq2[j]^2,
                           spline2=omit$spline2,
                           spline3=omit$spline3,
                           spline4=omit$spline4,
                           fastDummies::dummy_cols(omit$Country)[-c(1)])
  
  x.distance <-data.matrix(x.distance)
  
  # Save average of linear predictor across all the observations
  pp<-matrix(NA, nrow=n.draws2)
  pp.change<-matrix(NA, nrow=n.draws2)
  
  for(i in 1:n.draws2){# Loop over coefficients
    # For each observation in the dataset
    xb<-x.distance%*%(sim.fem.mem[i,])
    pp[i]<-mean(xb)
  }
  dim(sim.fem.mem)
  dim(x.distance)
  # Compute point estimate and CI for each value of distance
  pe.ov[j] <- quantile(pp, 0.5)
  lo.ov[j] <- quantile(pp, 0.025) 
  hi.ov[j] <- quantile(pp, 0.975)
  
} 
ggdata <- data.frame(x=sq2, pe.ov, lo.ov, hi.ov)

ggplot(ggdata, aes(x=x, y=pe.ov, ymin=lo.ov, ymax=hi.ov)) +
  geom_line() + 
  geom_ribbon(alpha=0.1) + 
  theme_bw() +  
  theme(axis.text = element_text(size=14), axis.title=element_text(size=15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio=1) +
  ylab("P(Proportion of Women Elected)") +
  xlab("Years since Conflict")

##Model for figure 4 - ongoing violence
m2data <- winners[,c("Percent_Women_Seats", "Rebel_Party", "logPercent", "democ", "Adopted_Quotas",  "Party_Institutionalization",  "exclusive", "gdplnvdem","vdem_wom_civsoc", "opposition", "Ongoing_conflict", "spline2", "spline3", "spline4", "Country")]
omit <- na.omit(m2data)
modelele.vio <- lmerTest::lmer(Percent_Women_Seats ~ 
                                 Rebel_Party 
                               +logPercent
                               + democ
                               + Adopted_Quotas 
                               + exclusive 
                               + opposition
                               + Party_Institutionalization
                               + gdplnvdem  
                               + vdem_wom_civsoc 
                               + Ongoing_conflict  
                               +  spline2 + spline3 + spline4 
                               + (1 | Country), data=winners)
summary(modelele.vio) 


n.draws<-100
sim.model<-coef(sim(modelele.vio, n.draws))  %>% data.frame() %>% data.matrix()
sq<-seq(from=min(na.omit(omit$Percent_Women_Seats)), to=max(na.omit(omit$Percent_Women_Seats)), length.out=100)
sq.x.lo<-data.frame(
  intercept=1,
  Rebel_Party=omit$Rebel_Party,
  logPercent=omit$logPercent,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  Ongoing_conflict=0,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

sq.x.hi<-data.frame(
  intercept=1,
  Rebel_Party=omit$Rebel_Party,
  logPercent=omit$logPercent,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  Ongoing_conflict=1,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

pe.x.hi <- as.matrix(NA, nrow= n.draws)
pe.x.lo <- as.matrix(NA, nrow= n.draws)

hi.x.hi <- as.matrix(NA, nrow= n.draws)
hi.x.lo <- as.matrix(NA, nrow= n.draws)

lo.x.hi <- as.matrix(NA, nrow= n.draws)
lo.x.lo <- as.matrix(NA, nrow= n.draws)

dif.x <- as.matrix(NA, nrow= n.draws)


sq.x.hi <- as.matrix(sq.x.hi)
sq.x.lo <- as.matrix(sq.x.lo)
#Fill the vectors


for(j in 1:n.draws){
  pe.x.hi[j] <- mean((sq.x.hi %*% sim.model[j,]), na.rm=T)
  pe.x.lo[j] <- mean((sq.x.lo %*% sim.model[j,]), na.rm=T)
  
  
  #first differences
  dif.x[j] <- pe.x.hi[j] - pe.x.lo[j]
}

dif.fun <- function(x) {
  result <- c(quantile(x, 0.025), mean(x), quantile(x,.975))
  return(result)
}
pe.x.hi <- dif.fun(pe.x.hi)
pe.x.lo <- dif.fun(pe.x.lo)

dif.x <- dif.fun(dif.x)

preds <- as.data.frame(rbind(pe.x.lo,pe.x.hi))

rownames(preds) <- c("No Ongoing Conflict",'Ongoing Conflict')
preds$levels <- rownames(preds)
preds$levels <- factor(preds$levels, levels=unique(preds$levels))

ggplot() + 
  geom_point(data = preds
             ,aes(x=preds$levels, y=preds$V2)
             ,size = 4
             ,color = "black") + 
  geom_segment(aes(x = preds$levels
                   , y = preds$`2.5%`
                   , xend = preds$levels
                   , yend = preds$`97.5%`)
               , color = "black"
               , size = 1
               , data = preds
               , lty= c(1,1)) +  theme_bw() + 
  theme(axis.text = element_text(size=14), axis.title=element_text(size=15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio=1) +
  xlab('') + 
  ylab("E(Percent Women Elected)")

###Appendix Models & Figures

#Time, Table 1

winners$time_sq <- winners$Time^2

modelcan.time <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                  Rebel_Party 
                                + democ
                                + Adopted_Quotas 
                                + exclusive 
                                + opposition
                                + Party_Institutionalization
                                + gdplnvdem  
                                + vdem_wom_civsoc
                                +  Time 
                                +  spline2 
                                + spline3 
                                + spline4 + (1 | Country), data=winners)
summary(modelcan.time) 

modelcan.time2 <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                   Rebel_Party 
                                 + democ
                                 + Adopted_Quotas 
                                 + exclusive 
                                 + opposition
                                 + Party_Institutionalization
                                 + gdplnvdem  
                                 + vdem_wom_civsoc
                                 +  Time 
                                 + Time:Rebel_Party
                                 +  spline2 
                                 + spline3 
                                 + spline4 + (1 | Country), data=winners)
summary(modelcan.time2) 

modelcan.time3 <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                   Rebel_Party 
                                 + democ
                                 + Adopted_Quotas 
                                 + exclusive 
                                 + opposition
                                 + Party_Institutionalization
                                 + gdplnvdem  
                                 + vdem_wom_civsoc
                                 +  Time 
                                 + time_sq
                                 +  spline2 
                                 + spline3 
                                 + spline4 + (1 | Country), data=winners)
summary(modelcan.time3) 

modelele.time <- lmerTest::lmer(Percent_Women_Seats ~ Rebel_Party 
                                + logPercent
                                + democ
                                + Adopted_Quotas 
                                + exclusive 
                                + opposition
                                + Party_Institutionalization
                                + gdplnvdem  
                                + vdem_wom_civsoc
                                + Time  
                                +Time
                                + spline2 
                                +spline3 
                                + spline4  + (1 | Country), data=winners)
summary(modelele.time) 

modelele.time2 <- lmerTest::lmer(Percent_Women_Seats ~ 
                                   Rebel_Party 
                                 + logPercent
                                 + democ
                                 + Adopted_Quotas 
                                 + exclusive 
                                 + opposition
                                 + Party_Institutionalization
                                 + gdplnvdem  
                                 + vdem_wom_civsoc
                                 + Time  
                                 + Time:Rebel_Party 
                                 + spline2 +spline3 + spline4  + (1 | Country), data=winners)
summary(modelele.time2) 

modelele.time3 <- lmerTest::lmer(Percent_Women_Seats ~ 
                                   Rebel_Party 
                                 + logPercent
                                 + democ
                                 + Adopted_Quotas 
                                 + exclusive 
                                 + opposition
                                 + Party_Institutionalization
                                 + gdplnvdem  
                                 + vdem_wom_civsoc
                                 + Time  
                                 + time_sq 
                                 + spline2 +spline3 + spline4  + (1 | Country), data=winners)
summary(modelele.time3) 

texreg(list(modelcan.time,modelcan.time2, modelcan.time3, modelele.time, modelele.time2, modelele.time3))

###Conflict Outcome, Table 2

modelcan.rebelvic <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                      Rebel_Party 
                                    + democ
                                    + Adopted_Quotas 
                                    + exclusive 
                                    + opposition
                                    + Party_Institutionalization
                                    + gdplnvdem  
                                    + vdem_wom_civsoc
                                    + rebelvictory 
                                    + Rebel_Party:rebelvictory 
                                    + spline2 + spline3 + spline4 + (1 | Country), data=winners)
summary(modelcan.rebelvic) 

modelcan.peaceag <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                     Rebel_Party 
                                   + democ
                                   + Adopted_Quotas 
                                   + exclusive 
                                   + opposition
                                   + Party_Institutionalization
                                   + gdplnvdem  
                                   + vdem_wom_civsoc
                                   + peaceag 
                                   + Rebel_Party:peaceag 
                                   + spline2 + spline3 + spline4 + (1 | Country), data=winners)
summary(modelcan.peaceag) 

modelcan.lowactivity <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                         Rebel_Party 
                                       + democ
                                       + Adopted_Quotas 
                                       + exclusive 
                                       + opposition
                                       + Party_Institutionalization
                                       + gdplnvdem  
                                       + vdem_wom_civsoc
                                       + lowactivity 
                                       + Rebel_Party:lowactivity 
                                       + spline2 
                                       + spline3 
                                       + spline4 
                                       + (1 | Country), data=winners)
summary(modelcan.lowactivity) 

texreg(list(modelcan.rebelvic, modelcan.peaceag, modelcan.lowactivity))

##Election, Table 3

modelele.rebelvic <- lmerTest::lmer(Percent_Women_Seats ~
                                      Rebel_Party 
                                    + logPercent
                                    + democ
                                    + Adopted_Quotas 
                                    + exclusive 
                                    + opposition
                                    + Party_Institutionalization
                                    + gdplnvdem  
                                    + vdem_wom_civsoc
                                    + rebelvictory 
                                    + Rebel_Party:rebelvictory
                                    + spline2 + spline3 + spline4 
                                    + (1 | Country), data=winners)
summary(modelele.rebelvic)

modelele.peaceag <- lmerTest::lmer(Percent_Women_Seats ~ 
                                     Rebel_Party 
                                   + logPercent
                                   + democ
                                   + Adopted_Quotas 
                                   + exclusive 
                                   + opposition
                                   + Party_Institutionalization
                                   + gdplnvdem  
                                   + vdem_wom_civsoc
                                   + peaceag 
                                   + Rebel_Party:peaceag 
                                   + spline2 + spline3 + spline4 
                                   + (1 | Country), data=winners)
summary(modelele.peaceag)

modelele.lowactivity <- lmerTest::lmer(Percent_Women_Seats ~ 
                                         Rebel_Party 
                                       + logPercent
                                       + democ
                                       + Adopted_Quotas 
                                       + exclusive 
                                       + opposition
                                       + Party_Institutionalization
                                       + gdplnvdem  
                                       + vdem_wom_civsoc 
                                       + lowactivity 
                                       + Rebel_Party:lowactivity 
                                       + spline2 + spline3 + spline4 
                                       + (1 | Country), data=winners)
summary(modelele.lowactivity) 

texreg(list(modelele.rebelvic, modelele.peaceag, modelele.lowactivity))

##Gender, Inclusivity, Rights, & Respect, Table 4



modelcan_left <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                  rebelparty_left +  
                                  democ +
                                  Adopted_Quotas +
                                  exclusive +
                                  opposition+
                                  Party_Institutionalization+
                                  gdplnvdem  +
                                  vdem_wom_civsoc+
                                  spline2 + 
                                  spline3 + 
                                  spline4  + 
                                  (1 | Country), 
                                data=winners)
summary(modelcan_left) 

modelele_left <- lmerTest::lmer(Percent_Women_Seats ~ 
                                  rebelparty_left + 
                                  +logPercent
                                + democ
                                + Adopted_Quotas 
                                + exclusive 
                                + opposition
                                + Party_Institutionalization
                                + gdplnvdem  
                                + vdem_wom_civsoc
                                + spline2 
                                + spline3 
                                + spline4  
                                + (1 | Country), data=winners)
summary(modelele_left) 

modelcan_humn <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                  rebelparty_humn +  
                                  democ +
                                  Adopted_Quotas +
                                  exclusive +
                                  opposition+
                                  Party_Institutionalization+
                                  gdplnvdem  +
                                  vdem_wom_civsoc+
                                  spline2 + 
                                  spline3 + 
                                  spline4  + 
                                  (1 | Country), 
                                data=winners)
summary(modelcan_humn) 

modelele_humn <- lmerTest::lmer(Percent_Women_Seats ~ 
                                  rebelparty_humn + 
                                  +logPercent
                                + democ
                                + Adopted_Quotas 
                                + exclusive 
                                + opposition
                                + Party_Institutionalization
                                + gdplnvdem  
                                + vdem_wom_civsoc
                                + spline2 
                                + spline3 
                                + spline4  
                                + (1 | Country), data=winners)
summary(modelele_humn) 

modelcan_forced <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                    rebelparty_forced +  
                                    democ +
                                    Adopted_Quotas +
                                    exclusive +
                                    opposition+
                                    Party_Institutionalization+
                                    gdplnvdem  +
                                    vdem_wom_civsoc+
                                    spline2 + 
                                    spline3 + 
                                    spline4  + 
                                    (1 | Country), 
                                  data=winners)
summary(modelcan_forced) 

modelele_forced <- lmerTest::lmer(Percent_Women_Seats ~ 
                                    rebelparty_forced + 
                                    +logPercent
                                  + democ
                                  + Adopted_Quotas 
                                  + exclusive 
                                  + opposition
                                  + Party_Institutionalization
                                  + gdplnvdem  
                                  + vdem_wom_civsoc
                                  + spline2 
                                  + spline3 
                                  + spline4  
                                  + (1 | Country), data=winners)
summary(modelele_forced) 
texreg(list(modelcan_left,modelcan_humn, modelcan_forced,modelele_left, modelele_humn, modelele_forced))

##Civilian victimization, Table 5 & Figure 1, Figure 2
m1data <- winners[,c("Percent_Women_Candidates", "civvil", "democ", "Adopted_Quotas",  "Party_Institutionalization",  "exclusive", "gdplnvdem","vdem_wom_civsoc", "opposition", "spline2", "spline3", "spline4", "Country")]
omit <- na.omit(m1data)
modelcan_civ <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                 civvil +
                                 democ +
                                 Adopted_Quotas +
                                 exclusive +
                                 opposition+
                                 Party_Institutionalization+
                                 gdplnvdem  +
                                 vdem_wom_civsoc+
                                 spline2 + 
                                 spline3 + 
                                 spline4  + 
                                 (1 | Country), 
                               data=omit)
summary(modelcan_civ) 

#Simulation for candidacy
n.draws<-100
sim.model<-coef(sim(modelcan_civ, n.draws))  %>% data.frame() %>% data.matrix()
sq<-seq(from=min(na.omit(omit$Percent_Women_Candidates)), to=max(na.omit(omit$Percent_Women_Candidates)), length.out=100)
sq.x.lo<-data.frame(
  intercept=1,
  civvil=0,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

sq.x.med<-data.frame(
  intercept=1,
  civvil=1,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])


sq.x.hi<-data.frame(
  intercept=1,
  civvil=2,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

pe.x.hi <- as.matrix(NA, nrow= n.draws)
pe.x.lo <- as.matrix(NA, nrow= n.draws)
pe.x.med <- as.matrix(NA, nrow= n.draws)

hi.x.hi <- as.matrix(NA, nrow= n.draws)
hi.x.lo <- as.matrix(NA, nrow= n.draws)
hi.x.med <- as.matrix(NA, nrow= n.draws)

lo.x.hi <- as.matrix(NA, nrow= n.draws)
lo.x.lo <- as.matrix(NA, nrow= n.draws)
lo.x.med <- as.matrix(NA, nrow= n.draws)

dif.1 <- as.matrix(NA, nrow= n.draws)
dif.2 <- as.matrix(NA, nrow= n.draws)
dif.3 <- as.matrix(NA, nrow= n.draws)

sq.x.hi <- as.matrix(sq.x.hi)
sq.x.lo <- as.matrix(sq.x.lo)
sq.x.med <- as.matrix(sq.x.med)

#Fill the vectors

for(j in 1:n.draws){
  pe.x.hi[j] <- mean((sq.x.hi %*% sim.model[j,]), na.rm=T)
  pe.x.lo[j] <- mean((sq.x.lo %*% sim.model[j,]), na.rm=T)
  pe.x.med[j] <- mean((sq.x.med %*% sim.model[j,]), na.rm=T)
  
  #first differences
  dif.1[j] <- pe.x.hi[j] - pe.x.med[j]
  dif.2[j] <- pe.x.hi[j] - pe.x.lo[j]
  dif.3[j] <- pe.x.med[j] - pe.x.lo[j]
}

dif.fun <- function(x) {
  result <- c(quantile(x, 0.025), mean(x), quantile(x,.975))
  return(result)
}
pe.x.hi <- dif.fun(pe.x.hi)
pe.x.lo <- dif.fun(pe.x.lo)
pe.x.med <- dif.fun(pe.x.med)
dif.1 <- dif.fun(dif.1)
dif.2 <- dif.fun(dif.2)
dif.3 <- dif.fun(dif.3)

preds <- as.data.frame(rbind( pe.x.lo, pe.x.med, pe.x.hi))

#Pred probplot 
rownames(preds) <- c('Non-Rebel Party', "Rebel Party w/o CV","Rebel Party w/ CV")
preds$levels <- rownames(preds)
preds$levels <- factor(preds$levels, levels=unique(preds$levels))

preds$lb <- preds$`2.5%`
preds$ub <- preds$`97.5%`

ggplot() + 
  geom_point(data = preds
             ,aes(x=preds$levels, y=preds$V2)
             ,size = 5
             ,color = "black") + 
  geom_segment(aes(x = preds$levels
                   , y = preds$ub
                   , xend = preds$levels
                   , yend = preds$lb)
               , color = "black"
               , size = 1
               , data = preds
               , lty= c(1,1,1)) +
  theme_bw() + 
  theme(axis.text = element_text(size=15, angle=90), axis.title=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio=1.25) +
  xlab('') + 
  ylab("E(Proportion of Women Candidates)")


##Election civvic 
m2data <- winners[,c("Percent_Women_Seats", "civvil", "logPercent", "democ", "Adopted_Quotas",  "Party_Institutionalization",  "exclusive", "gdplnvdem","vdem_wom_civsoc", "opposition", "spline2", "spline3", "spline4", "Country")]
omit <- na.omit(m2data)
modelele_civ <- lmerTest::lmer(Percent_Women_Seats ~ 
                                 civvil +
                                 logPercent +
                                 + democ
                               + Adopted_Quotas 
                               + exclusive 
                               + opposition
                               + Party_Institutionalization
                               + gdplnvdem  
                               + vdem_wom_civsoc
                               + spline2 
                               + spline3 
                               + spline4  
                               + (1 | Country), data=omit)
summary(modelele_civ)
texreg(list(modelcan_civ,modelele_civ))

#Simulation

n.draws<-100
sim.model<-coef(sim(modelele_civ, n.draws))  %>% data.frame() %>% data.matrix()
sq<-seq(from=min(na.omit(omit$Percent_Women_Seats)), to=max(na.omit(omit$Percent_Women_Seats)), length.out=100)
sq.x.lo<-data.frame(
  intercept=1,
  civvil=0,
  logPercent=omit$logPercent,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

sq.x.med<-data.frame(
  intercept=1,
  civvil=1,
  logPercent=omit$logPercent,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])


sq.x.hi<-data.frame(
  intercept=1,
  civvil=2,
  logPercent=omit$logPercent,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

pe.x.hi <- as.matrix(NA, nrow= n.draws)
pe.x.lo <- as.matrix(NA, nrow= n.draws)
pe.x.med <- as.matrix(NA, nrow= n.draws)

hi.x.hi <- as.matrix(NA, nrow= n.draws)
hi.x.lo <- as.matrix(NA, nrow= n.draws)
hi.x.med <- as.matrix(NA, nrow= n.draws)

lo.x.hi <- as.matrix(NA, nrow= n.draws)
lo.x.lo <- as.matrix(NA, nrow= n.draws)
lo.x.med <- as.matrix(NA, nrow= n.draws)

dif.1 <- as.matrix(NA, nrow= n.draws)
dif.2 <- as.matrix(NA, nrow= n.draws)
dif.3 <- as.matrix(NA, nrow= n.draws)

sq.x.hi <- as.matrix(sq.x.hi)
sq.x.lo <- as.matrix(sq.x.lo)
sq.x.med <- as.matrix(sq.x.med)

#Fill the vectors

for(j in 1:n.draws){
  pe.x.hi[j] <- mean((sq.x.hi %*% sim.model[j,]), na.rm=T)
  pe.x.lo[j] <- mean((sq.x.lo %*% sim.model[j,]), na.rm=T)
  pe.x.med[j] <- mean((sq.x.med %*% sim.model[j,]), na.rm=T)
  
  #first differences
  dif.1[j] <- pe.x.hi[j] - pe.x.med[j]
  dif.2[j] <- pe.x.hi[j] - pe.x.lo[j]
  dif.3[j] <- pe.x.med[j] - pe.x.lo[j]
}

dif.fun <- function(x) {
  result <- c(quantile(x, 0.025), mean(x), quantile(x,.975))
  return(result)
}
pe.x.hi <- dif.fun(pe.x.hi)
pe.x.lo <- dif.fun(pe.x.lo)
pe.x.med <- dif.fun(pe.x.med)
dif.1 <- dif.fun(dif.1)
dif.2 <- dif.fun(dif.2)
dif.3 <- dif.fun(dif.3)

preds <- as.data.frame(rbind( pe.x.lo, pe.x.med, pe.x.hi))

#Pred prob plot 
rownames(preds) <- c('Non-Rebel Party', "Rebel Party w/o CV","Rebel Party w/ CV")
preds$levels <- rownames(preds)
preds$levels <- factor(preds$levels, levels=unique(preds$levels))

preds$lb <- preds$`2.5%`
preds$ub <- preds$`97.5%`

ggplot() + 
  geom_point(data = preds
             ,aes(x=preds$levels, y=preds$V2)
             ,size = 5
             ,color = "black") + 
  geom_segment(aes(x = preds$levels
                   , y = preds$ub
                   , xend = preds$levels
                   , yend = preds$lb)
               , color = "black"
               , size = 1
               , data = preds
               , lty= c(1,1,1)) +
  theme_bw() + 
  theme(axis.text = element_text(size=15, angle=90), axis.title=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio=1.25) +
  xlab('') + 
  ylab("E(Proportion of Women Elected)")


#Ongoing Violence, Table 6

modelcan.vio <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                 Rebel_Party 
                               + democ
                               + Adopted_Quotas 
                               + exclusive 
                               + opposition
                               + Party_Institutionalization
                               + gdplnvdem  
                               + vdem_wom_civsoc 
                               + Ongoing_conflict 
                               + spline2 + spline3 + spline4 
                               + (1 | Country), data=winners)
summary(modelcan.vio)

modelele.vio <- lmerTest::lmer(Percent_Women_Seats ~ 
                                 Rebel_Party 
                               +logPercent
                               + democ
                               + Adopted_Quotas 
                               + exclusive 
                               + opposition
                               + Party_Institutionalization
                               + gdplnvdem  
                               + vdem_wom_civsoc 
                               + Ongoing_conflict  
                               +  spline2 + spline3 + spline4 
                               + (1 | Country), data=winners)
summary(modelele.vio) 


modelele.vio2 <- lmerTest::lmer(Percent_Women_Seats ~ 
                                 Rebel_Party 
                               +logPercent
                               + democ
                               + Adopted_Quotas 
                               + exclusive 
                               + opposition
                               + Party_Institutionalization
                               + gdplnvdem  
                               + vdem_wom_civsoc 
                               + Ongoing_conflict  
                               + Ongoing_conflict:Rebel_Party
                               +  spline2 + spline3 + spline4 
                               + (1 | Country), data=winners)
summary(modelele.vio2) 

texreg(list(modelcan.vio, modelele.vio, modelele.vio2))


##Women's Participation in Rebel Groups, Table 7, Figure 3, & Figure 4

m1data <- winners[,c("Percent_Women_Candidates", "Female_ord", "democ", "Adopted_Quotas",  "Party_Institutionalization",  "exclusive", "gdplnvdem","vdem_wom_civsoc", "opposition", "spline2", "spline3", "spline4", "Country")]
omit <- na.omit(m1data)

modelcan_fem <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                 Female_ord +
                                 democ +
                                 Adopted_Quotas +
                                 exclusive +
                                 opposition+
                                 Party_Institutionalization+
                                 gdplnvdem  +
                                 vdem_wom_civsoc+
                                 spline2 + 
                                 spline3 + 
                                 spline4  + 
                                 (1 | Country), 
                               data=omit)
summary(modelcan_fem) 

###Estimates for candidacy models

n.draws<-100
sim.model<-coef(sim(modelcan_fem, n.draws))  %>% data.frame() %>% data.matrix()
sq<-seq(from=min(na.omit(omit$Percent_Women_Candidates)), to=max(na.omit(omit$Percent_Women_Candidates)), length.out=100)
sq.x.lo<-data.frame(
  intercept=1,
  Female_ord=0,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

sq.x.med<-data.frame(
  intercept=1,
  Female_ord=1,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])


sq.x.hi<-data.frame(
  intercept=1,
  Female_ord=2,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

pe.x.hi <- as.matrix(NA, nrow= n.draws)
pe.x.lo <- as.matrix(NA, nrow= n.draws)
pe.x.med <- as.matrix(NA, nrow= n.draws)

hi.x.hi <- as.matrix(NA, nrow= n.draws)
hi.x.lo <- as.matrix(NA, nrow= n.draws)
hi.x.med <- as.matrix(NA, nrow= n.draws)

lo.x.hi <- as.matrix(NA, nrow= n.draws)
lo.x.lo <- as.matrix(NA, nrow= n.draws)
lo.x.med <- as.matrix(NA, nrow= n.draws)

dif.1 <- as.matrix(NA, nrow= n.draws)
dif.2 <- as.matrix(NA, nrow= n.draws)
dif.3 <- as.matrix(NA, nrow= n.draws)

sq.x.hi <- as.matrix(sq.x.hi)
sq.x.lo <- as.matrix(sq.x.lo)
sq.x.med <- as.matrix(sq.x.med)

#Fill the vectors

for(j in 1:n.draws){
  pe.x.hi[j] <- mean((sq.x.hi %*% sim.model[j,]), na.rm=T)
  pe.x.lo[j] <- mean((sq.x.lo %*% sim.model[j,]), na.rm=T)
  pe.x.med[j] <- mean((sq.x.med %*% sim.model[j,]), na.rm=T)
  
  #first differences
  dif.1[j] <- pe.x.hi[j] - pe.x.med[j]
  dif.2[j] <- pe.x.hi[j] - pe.x.lo[j]
  dif.3[j] <- pe.x.med[j] - pe.x.lo[j]
}

dif.fun <- function(x) {
  result <- c(quantile(x, 0.025), mean(x), quantile(x,.975))
  return(result)
}
pe.x.hi <- dif.fun(pe.x.hi)
pe.x.lo <- dif.fun(pe.x.lo)
pe.x.med <- dif.fun(pe.x.med)
dif.1 <- dif.fun(dif.1)
dif.2 <- dif.fun(dif.2)
dif.3 <- dif.fun(dif.3)

preds <- as.data.frame(rbind( pe.x.lo, pe.x.med, pe.x.hi))

#Pred probplot 
rownames(preds) <- c('Non-Rebel Party', "Rebel Party w/o Females","Rebel Party w/ Females")
preds$levels <- rownames(preds)
preds$levels <- factor(preds$levels, levels=unique(preds$levels))

preds$lb <- preds$`2.5%`
preds$ub <- preds$`97.5%`

ggplot() + 
  geom_point(data = preds
             ,aes(x=preds$levels, y=preds$V2)
             ,size = 5
             ,color = "black") + 
  geom_segment(aes(x = preds$levels
                   , y = preds$ub
                   , xend = preds$levels
                   , yend = preds$lb)
               , color = "black"
               , size = 1
               , data = preds
               , lty= c(1,1,1)) +
  theme_bw() + 
  theme(axis.text = element_text(size=15, angle=90), axis.title=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio=1.25) +
  xlab('') + 
  ylab("E(Proportion of Women Candidates)")


###Election 
m2data <- winners[,c("Percent_Women_Seats", "Female_ord", "logPercent", "democ", "Adopted_Quotas",  "Party_Institutionalization",  "exclusive", "gdplnvdem","vdem_wom_civsoc", "opposition", "spline2", "spline3", "spline4", "Country")]
omit <- na.omit(m2data)

modelele_fem <- lmerTest::lmer(Percent_Women_Seats ~ 
                                 Female_ord +
                                 logPercent +
                                 + democ
                               + Adopted_Quotas 
                               + exclusive 
                               + opposition
                               + Party_Institutionalization
                               + gdplnvdem  
                               + vdem_wom_civsoc
                               + spline2 
                               + spline3 
                               + spline4  
                               + (1 | Country), data=omit)
summary(modelele_fem)

#tables 
texreg(list(modelcan_fem,modelele_fem))

####Estimates for election


n.draws<-100
sim.model<-coef(sim(modelele_fem, n.draws))  %>% data.frame() %>% data.matrix()
sq<-seq(from=min(na.omit(omit$Percent_Women_Seats)), to=max(na.omit(omit$Percent_Women_Seats)), length.out=100)
sq.x.lo<-data.frame(
  intercept=1,
  Female_ord=0,
  logPercent=omit$logPercent,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

sq.x.med<-data.frame(
  intercept=1,
  Female_ord=1,
  logPercent=omit$logPercent,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])


sq.x.hi<-data.frame(
  intercept=1,
  Female_ord=2,
  logPercent=omit$logPercent,
  democ=omit$democ,
  Adopted_Quotas=omit$Adopted_Quotas,
  exclusive=omit$exclusive,
  opposition=omit$opposition,
  Party_Institutionalization=omit$Party_Institutionalization,
  gdplnvdem=omit$gdplnvdem,
  vdem_wom_civsoc=omit$vdem_wom_civsoc,
  spline2=omit$spline2,
  spline3=omit$spline3,
  spline4=omit$spline4,
  fastDummies::dummy_cols(omit$Country)[-c(1)])

pe.x.hi <- as.matrix(NA, nrow= n.draws)
pe.x.lo <- as.matrix(NA, nrow= n.draws)
pe.x.med <- as.matrix(NA, nrow= n.draws)

hi.x.hi <- as.matrix(NA, nrow= n.draws)
hi.x.lo <- as.matrix(NA, nrow= n.draws)
hi.x.med <- as.matrix(NA, nrow= n.draws)

lo.x.hi <- as.matrix(NA, nrow= n.draws)
lo.x.lo <- as.matrix(NA, nrow= n.draws)
lo.x.med <- as.matrix(NA, nrow= n.draws)

dif.1 <- as.matrix(NA, nrow= n.draws)
dif.2 <- as.matrix(NA, nrow= n.draws)
dif.3 <- as.matrix(NA, nrow= n.draws)

sq.x.hi <- as.matrix(sq.x.hi)
sq.x.lo <- as.matrix(sq.x.lo)
sq.x.med <- as.matrix(sq.x.med)

#Fill the vectors

for(j in 1:n.draws){
  pe.x.hi[j] <- mean((sq.x.hi %*% sim.model[j,]), na.rm=T)
  pe.x.lo[j] <- mean((sq.x.lo %*% sim.model[j,]), na.rm=T)
  pe.x.med[j] <- mean((sq.x.med %*% sim.model[j,]), na.rm=T)
  
  #first differences
  dif.1[j] <- pe.x.hi[j] - pe.x.med[j]
  dif.2[j] <- pe.x.hi[j] - pe.x.lo[j]
  dif.3[j] <- pe.x.med[j] - pe.x.lo[j]
}

dif.fun <- function(x) {
  result <- c(quantile(x, 0.025), mean(x), quantile(x,.975))
  return(result)
}
pe.x.hi <- dif.fun(pe.x.hi)
pe.x.lo <- dif.fun(pe.x.lo)
pe.x.med <- dif.fun(pe.x.med)
dif.1 <- dif.fun(dif.1)
dif.2 <- dif.fun(dif.2)
dif.3 <- dif.fun(dif.3)

preds <- as.data.frame(rbind( pe.x.lo, pe.x.med, pe.x.hi))

#Pred probplot 
rownames(preds) <- c('Non-Rebel Party', "Rebel Party w/o Females","Rebel Party w/ Females")
preds$levels <- rownames(preds)
preds$levels <- factor(preds$levels, levels=unique(preds$levels))

preds$lb <- preds$`2.5%`
preds$ub <- preds$`97.5%`

ggplot() + 
  geom_point(data = preds
             ,aes(x=preds$levels, y=preds$V2)
             ,size = 5
             ,color = "black") + 
  geom_segment(aes(x = preds$levels
                   , y = preds$ub
                   , xend = preds$levels
                   , yend = preds$lb)
               , color = "black"
               , size = 1
               , data = preds
               , lty= c(1,1,1)) +
  theme_bw() + 
  theme(axis.text = element_text(size=15, angle=90), axis.title=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio=1.25) +
  xlab('') + 
  ylab("E(Proportion of Women Elected)")


###Alt Democracy Specification, Table 8
modelcan.int <- lmerTest::lmer(Percent_Women_Candidates ~ 
                             Rebel_Party +  
                             democ +
                               democ:Rebel_Party +
                             Adopted_Quotas +
                             exclusive +
                             opposition+
                             Party_Institutionalization+
                             gdplnvdem  +
                             vdem_wom_civsoc+
                             spline2 + 
                             spline3 + 
                             spline4  + 
                             (1 | Country), 
                           data=winners)
summary(modelcan.int) 

modelcan.elec <- lmerTest::lmer(Percent_Women_Candidates ~ 
                             Rebel_Party +  
                             vdem_elecdem +
                             Adopted_Quotas +
                             exclusive +
                             opposition+
                             Party_Institutionalization+
                             gdplnvdem  +
                             vdem_wom_civsoc+
                             spline2 + 
                             spline3 + 
                             spline4  + 
                             (1 | Country), 
                           data=winners)
summary(modelcan.elec) 

modelcan.lib <- lmerTest::lmer(Percent_Women_Candidates ~ 
                             Rebel_Party +  
                             vdem_libdem +
                             Adopted_Quotas +
                             exclusive +
                             opposition+
                             Party_Institutionalization+
                             gdplnvdem  +
                             vdem_wom_civsoc+
                             spline2 + 
                             spline3 + 
                             spline4  + 
                             (1 | Country), 
                           data=winners)
summary(modelcan) 


modelele.int <- lmerTest::lmer(Percent_Women_Seats ~ 
                             Rebel_Party 
                           + logPercent
                           + democ
                           + democ:Rebel_Party 
                           + Adopted_Quotas 
                           + exclusive 
                           + opposition
                           + Party_Institutionalization
                           + gdplnvdem  
                           + vdem_wom_civsoc
                           + spline2 
                           + spline3 
                           + spline4  
                           + (1 | Country), data=winners)
summary(modelele.int) 

modelele.elec <- lmerTest::lmer(Percent_Women_Seats ~ 
                             Rebel_Party 
                           + logPercent
                           + vdem_elecdem
                           + Adopted_Quotas 
                           + exclusive 
                           + opposition
                           + Party_Institutionalization
                           + gdplnvdem  
                           + vdem_wom_civsoc
                           + spline2 
                           + spline3 
                           + spline4  
                           + (1 | Country), data=winners)
summary(modelele.elec) 

modelele.lib <- lmerTest::lmer(Percent_Women_Seats ~ 
                             Rebel_Party 
                           + logPercent
                           + vdem_libdem
                           + Adopted_Quotas 
                           + exclusive 
                           + opposition
                           + Party_Institutionalization
                           + gdplnvdem  
                           + vdem_wom_civsoc
                           + spline2 
                           + spline3 
                           + spline4  
                           + (1 | Country), data=winners)
summary(modelele.lib) 

texreg(list(modelcan.int, modelcan.elec, modelcan.lib, modelele.int, modelele.elec, modelele.lib))


##GENDER QUOTAS, TABLE 9
#Implemented
modelcan_q1 <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                Rebel_Party +  
                                democ+
                                Implemented_Quotas +
                                exclusive +
                                opposition+
                                Party_Institutionalization+
                                gdplnvdem  +
                                vdem_wom_civsoc+
                                spline2 + 
                                spline3 + 
                                spline4  + 
                                (1 | Country), 
                              data=winners)
summary(modelcan_q1) 

modelele_q2 <- lmerTest::lmer(Percent_Women_Seats ~ 
                                Rebel_Party 
                              + democ
                              + Implemented_Quotas 
                              + exclusive 
                              + opposition
                              + Party_Institutionalization
                              + gdplnvdem  
                              + vdem_wom_civsoc
                              + spline2 
                              + spline3 
                              + spline4  
                              + (1 | Country), data=winners)
summary(modelele_q2) 

#Effective
modelcan_q3 <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                Rebel_Party + 
                                democ +
                                Effective_Quotas +
                                exclusive +
                                opposition+
                                Party_Institutionalization+
                                gdplnvdem  +
                                vdem_wom_civsoc+
                                spline2 + 
                                spline3 + 
                                spline4  + 
                                (1 | Country), 
                              data=winners)
summary(modelcan_q3) 

modelele_q4 <- lmerTest::lmer(Percent_Women_Seats ~ 
                                Rebel_Party 
                              + logPercent
                              + democ
                              + Effective_Quotas 
                              + exclusive 
                              + opposition
                              + Party_Institutionalization
                              + gdplnvdem  
                              + vdem_wom_civsoc
                              + spline2 
                              + spline3 
                              + spline4  
                              + (1 | Country), data=winners)
summary(modelele_q4) 

modelcan_q5 <- lmerTest::lmer(Percent_Women_Candidates ~ 
                                Rebel_Party + 
                               logPercent +
                                democ +
                                Party_Quota +
                                exclusive +
                                opposition+
                                Party_Institutionalization+
                                gdplnvdem  +
                                vdem_wom_civsoc+
                                spline2 + 
                                spline3 + 
                                spline4  + 
                                Rebel_Party*Party_Quota +
                                (1 | Country), 
                              data=winners)
summary(modelcan_q5) 

modelele_q6 <- lmerTest::lmer(Percent_Women_Seats ~ 
                                Rebel_Party 
                              + logPercent
                              + democ
                              + Party_Quota 
                              + exclusive 
                              + opposition
                              + Party_Institutionalization
                              + gdplnvdem  
                              + vdem_wom_civsoc
                              + spline2 
                              + spline3 
                              + spline4  
                              + Rebel_Party*Party_Quota
                              + (1 | Country), data=winners)
summary(modelele_q6) 

texreg(list(modelcan_q1, modelcan_q3, modelcan_q5, modelele_q2, modelele_q4, modelele_q6))

####OLS MODEL, TABLE 10
modelcan <- glm(Percent_Women_Candidates ~ 
                  Rebel_Party +  
                  democ +
                  Adopted_Quotas +
                  exclusive +
                  opposition+
                  Party_Institutionalization+
                  gdplnvdem  +
                  vdem_wom_civsoc+
                  spline2 + 
                  spline3 + 
                  spline4, 
                family=gaussian(identity), 
                data=winners)
summary(modelcan) 

modelele <- glm(Percent_Women_Seats ~ 
                  Rebel_Party 
                +logPercent
                + democ
                + Adopted_Quotas 
                + exclusive 
                + opposition
                + Party_Institutionalization
                + gdplnvdem  
                + vdem_wom_civsoc
                + spline2 
                + spline3 
                + spline4, family=gaussian(identity), 
                data=winners)
summary(modelele) 
texreg(list(modelcan, modelele))





#My Contribution: 


library("sampleSelection")
heckit_model <- selection(Percent_Women_Candidates ~ Adopted_Quotas + Party_Institutionalization + 
                 democ + Rebel_Party,
                 Percent_Women_Seats ~ Adopted_Quotas + Party_Institutionalization + democ + Rebel_Party,
                 data=winners,
                 method='2step')
summary(heckit_model)

