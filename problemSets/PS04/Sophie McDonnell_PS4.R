library('eha')
library('survival')
library('tidyverse')
library('ggfortify')
library('stargazer')

data(child)

child$m.agegroup <- cut(child$m.age, breaks = seq(10, 60, 10), right = FALSE)

child_surv <- with(child, Surv(enter, exit, event))

# Overall survival
km_overall <- survfit(child_surv ~ 1, data = child)
summary(km_overall, times = seq(0, 15, 1))
autoplot(km_overall) 

# Impact of child gender
km_sex <- survfit(child_surv ~ sex, data = child)
autoplot(km_sex)

# Impact of mother's age
km_magegroup <- survfit(child_surv ~ m.agegroup, data = child)
autoplot(km_sex)


### MODEL

cox <- coxph(child_surv ~ m.agegroup + sex, data = child)
summary(cox)

# the model for age group shows us that mothers in their teens have higher child mortality rates than the other age brackets
# the model also highlights that child mortality increases with mothers over the age of 40
# the model shows us that there are up to 26% fewer child deaths in mothers 20 and older versus teenage mothers


### FIT MODEL

cox_fit <- survfit(cox)
autoplot(cox_fit)




newdat <- with(child, 
               data.frame(
                   sex = "female", m.agegroup=c("[10,20)", "[20,30)")
               ))

plot(survfit(cox, newdata = newdat), xscale = 12,
     conf.int = T,
     ylim = c(0.6, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("bottomleft",
       legend=c("[10,20)", "[20,30)"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))

