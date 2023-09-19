library(tidyverse)

ggplot(interaction, aes(x=timediff, y=Mspeed, color=interaction(factor(Crash), factor(Mramp)))) +
  geom_line()
ggplot(interaction, aes(x=timediff, y=Adiversion, color=interaction(factor(Crash), factor(Mramp)))) +
  geom_line()

plot(interaction$Mspeed, interaction$Cmspeed, type='n')
text(interaction$Mspeed, interaction$Cmspeed, interaction$Crash, col=ifelse(interaction$Signstate, "red","blue"))
abline(0,1)

plot(interaction$Adiversion, interaction$Cdiversion, type='n')
text(interaction$Adiversion, interaction$Cdiversion, interaction$Crash, col=ifelse(interaction$Signstate, "red","blue"))
abline(0,1)



plot(interaction$Mspeed, interaction$Cmspeed, type='n')
text(interaction$Mspeed, interaction$Cmspeed, interaction$Mramp, col=ifelse(interaction$Signstate, "red","blue"))
abline(0,1)

plot(interaction$Adiversion, interaction$Cdiversion, type='n')
text(interaction$Adiversion, interaction$Cdiversion, interaction$Mramp, col=ifelse(interaction$Signstate, "red","blue"))
abline(0,1)





library(lme4)

fit1 <- lmer(Mspeed ~ timediff*factor(Signstate) + (1|Crash/Mramp), data=interaction)
summary(fit1)

fit2 <- lmer(Adiversion ~ timediff*factor(Signstate) + (1|Crash/Mramp), data=interaction)
summary(fit2)

fit3<- lmer(Adiversion ~ Cdiversion*factor(Signstate) + timediff*factor(Signstate) + (1|Crash/Mramp), data=interaction)
summary(fit3)
