# ===================================================
# = Construir componente a partir de mate y español y agregar estimaciones =
# ===================================================
library(ggplot2)
library(rjags)
library(R2jags)
library(ddply)
library(Hmisc)
logit <- function(x){ log(x/(1-x))}

load("primarias_s_Entrega_abril_7.RData")

#Agregamos estimadores puntuales de proporción de buenos:
#usamos primarias.s del scrit 11_primarias_modelo
primarias.s$p.mate <- jags.fit$BUGSoutput$mean$p.mat.bound
primarias.s$p.esp <- jags.fit$BUGSoutput$mean$p.esp.bound
primarias.temp <- primarias.s[ , c('CVE_ESCUELA', 'TURNO','ESCUELA','TIPO_ESCUELA','ENTIDAD','NOM_ENT.1',
  'tipo','marginación','tot.primaria','no.bueno.esp','no.bueno.mate', 'p.español','p.mate')]


## Ahora calculamos el score
sims.1 <- jags.fit$BUGSoutput$sims.list

beta.0 <- (sims.1$Xbeta.0)
p.esp.logit <- logit(sims.1$p.esp.bound)
p.mate.logit <- logit(sims.1$p.mat.bound)
logit.esp.rank <- p.esp.logit - beta.0 # este es el score simulado
logit.mate.rank <- p.mate.logit - beta.0 # este es el score simulado
esp.adj.mean <- apply(logit.esp.rank, 2, mean)
esp.adj.sd <- apply(logit.esp.rank, 2, sd)
mate.adj.mean <- apply(logit.mate.rank, 2, mean)
mate.adj.sd <- apply(logit.mate.rank, 2, sd)

#media y sd de score
resultados <- data.frame(esp.adj.mean, esp.adj.sd, mate.adj.mean, mate.adj.sd)

primarias.salida <- data.frame(primarias.temp, resultados)
        

comps.1 <- princomp(primarias.salida[,c('esp.adj.mean', 'mate.adj.mean')])
summary(comps.1)

score.1 <- comps.1$scores[,1]
primarias.salida$score <- score.1
primarias.salida$score.quintil <- cut2(primarias.salida$score, g = 5)

save(primarias.salida, file = './out/primarias_salida_7abril.RData')



