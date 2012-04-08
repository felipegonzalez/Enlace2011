# ===================================================
# = Construir componente a partir de mate y español =
# ===================================================
medias.post <- jags.fit$BUGSoutput$mean
comp.medias <- data.frame(español = medias.post$p.esp.bound,
        mate = medias.post$p.mat.bound)
comp.medias <- transform(comp.medias,
    logit.esp = log(español/(1-español)),
    logit.mate = log(mate/(1-mate)))

comps.1 <- princomp(comp.medias[,c('logit.esp', 'logit.mate')])
summary(comps.1)

score.1 <- comps.1$scores[,1]

## Pegar en base
primarias.s$score <- score.1
primarias.s$p.español <- comp.medias$español
primarias.s$p.mate <- comp.medias$mate

primarias.ord <- arrange(primarias.s, (score))
head(primarias.ord,20)

