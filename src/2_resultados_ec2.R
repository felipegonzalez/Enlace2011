library(ProjectTemplate)
library(ggplot2)
library(plyr)
load('./out/primarias_salida_Entrega_7abril.RData')

# ========================
# = Algunos diagnósticos =
# ========================
## 
primarias.resumen <- ddply(primarias.salida, c('tipo', 'marginación'), summarise,
  num.esc = length(p.español),
  media.esp = mean(p.español),
  media.mate = mean(p.mate), 
  sd.esp=sd(p.español)/sqrt(length(p.español)),
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


