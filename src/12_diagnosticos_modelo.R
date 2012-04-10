# cargar los siguientes archivos:
# load('./out/jagsfit_Entrega_abril_7.Rdata')
# library(ggplot2)
# library(rjags)
# library(R2jags)
# library(ddply)
# ===========================================
# = Correr después de 11_primarias_modelo.R =
# ===========================================

svg(file = "./graphs/plot_jagsfit.svg")
plot(jags.fit)
dev.off()

#traceplot(jags.fit)

medias.post <- jags.fit$BUGSoutput$mean
comp.medias <- data.frame(español = medias.post$p.esp.bound,
        mate = medias.post$p.mat.bound)
svg(file = './graphs/comp_mate_español_medias.svg')        
ggplot(comp.medias, aes(x = español, y = mate)) + geom_point() +
    geom_abline(xintercept = 0, slope = 1, colour = 'red')
dev.off()