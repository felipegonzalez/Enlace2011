library(ProjectTemplate)
library(ggplot2)
library(plyr)
load('./out/primarias_salida_7abril.RData')

# ========================
# = Algunos diagnósticos =
# ========================
## 
primarias.resumen <- ddply(primarias.salida, c('tipo', 'marginación'), summarise,
  num.esc = length(p.esp),
  media.esp = mean(p.esp),
  media.mate = mean(p.mate), 
  sd.esp=sd(p.esp)/sqrt(length(p.esp)),
  sd.mate = sd(p.mate)/sqrt(length(p.mate))
  )


primarias.resumen <- subset(primarias.resumen, num.esc > 5)
svg(file = './graphs/matematicas_tipo_marginacion.svg')
ggplot(primarias.resumen, aes(x=marginación, y=media.mate, ymax=media.mate+3*sd.mate, ymin=media.mate-3*sd.mate, 
  colour=tipo, group=tipo)) + geom_linerange() + 
  geom_point() + geom_line() + ylim(c(0,0.8))
dev.off()

svg(file = './graphs/español_tipo_marginacion.svg')
ggplot(primarias.resumen, aes(x=marginación, y=media.esp, ymax=media.esp+3*sd.esp, ymin=media.esp-3*sd.esp, 
  colour=tipo, group=tipo)) + geom_linerange() + 
  geom_point() + geom_line() + ylim(c(0,0.8))
dev.off()

primarias.resumen.2 <- ddply(primarias.salida, c('tipo', 'marginación','score.quintil'), summarise,
  num.esc = length(p.español),
  media.esp = mean(p.español),
  media.mate = mean(p.mate), 
  sd.esp=sd(p.español)/sqrt(length(p.español)),
  sd.mate = sd(p.mate)/sqrt(length(p.mate))
  )
  
primarias.resumen.2 <- ddply(primarias.resumen.2, c('tipo','marginación'), transform,
  prop.escuelas = num.esc/sum(num.esc))

svg(file = './graphs/distribución_equitativa_quintiles.svg')
ggplot(primarias.resumen.2, aes(x=marginación, fill=score.quintil, weight=prop.escuelas)) + geom_bar() +
  facet_wrap(~tipo)
dev.off()


### Coeficientes
hist(jags.fit$BUGSoutput$mean$a.estado.adj[-20]) #Nota: Oaxaca sólo tiene unas cuantas escuelas - ignorar
estados.coef <- data.frame(estado = names(table(primarias.salida$NOM_ENT.1)), 
  coeficiente = jags.fit$BUGSoutput$mean$a.estado.adj[-20], 
  coeficiente.sd = jags.fit$BUGSoutput$sd$a.estado.adj[-20])
estados.coef.2 <- arrange(estados.coef, coeficiente)  # Nótese que el error es grande para estados

#Sin embargo, las estimaciones de coeficientes de marginación y tipo tienen muy alta precisión:
marginacion.coef <- data.frame(media = jags.fit$BUGSoutput$mean$a.marg.adj, sd = jags.fit$BUGSoutput$sd$a.marg.adj)
tipo.coef <- data.frame(media = jags.fit$BUGSoutput$mean$a.tipo.adj, sd = jags.fit$BUGSoutput$sd$a.tipo.adj)
write.csv(estados.coef.2, file = './out/estados_coef.csv')
write.csv(marginacion.coef, file = './out/marginacion_coef.csv')
write.csv(tipo.coef, file = './out/tipo_coef.csv')