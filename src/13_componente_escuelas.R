# ===================================================
# = Construir componente a partir de mate y español =
# ===================================================
medias.post <- jags.fit$BUGSoutput$mean
comp.medias <- data.frame(español = medias.post$p.esp.bound,
        mate = medias.post$p.mat.bound, esperado.logit = medias.post$Xbeta.0)
comp.medias <- transform(comp.medias,
    logit.esp = log(español/(1-español)),
    logit.mate = log(mate/(1-mate)))
comp.medias <- transform(comp.medias, esp.adj.logit = logit.esp - esperado.logit,
    mat.adj.logit = logit.mate - esperado.logit)


comps.1 <- princomp(comp.medias[,c('esp.adj.logit', 'mat.adj.logit')])
summary(comps.1)

score.1 <- comps.1$scores[,1]

## Pegar en base
primarias.s$score <- score.1
primarias.s$p.español <- comp.medias$español
primarias.s$p.mate <- comp.medias$mate

# ===========================
# = Componente para ranking =
# ===========================


primarias.ord <- arrange(primarias.s, (score))
head(primarias.ord,20)

