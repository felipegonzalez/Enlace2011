library(ProjectTemplate)
library(ggplot2)
library(plyr)
load('./out/primarias_s_Entrega_abril_7.RData')

primarias.salida <- primarias.s.comp[ , c('CVE_ESCUELA', 'TURNO','ESCUELA','TIPO_ESCUELA','ENTIDAD','NOM_ENT.1',
  'tipo','marginación','tot.primaria','no.bueno.esp','no.bueno.mate', 'score', 'p.español','p.mate')]

save(primarias.salida, file='./out/primarias_salida_7abril.RData')


# ========================
# = Algunos diagnósticos =
# ========================
primarias.resumen <- ddply(primarias.salida, c('tipo', 'marginación','TURNO'), summarise,
  num.esc = length(p.español),
  media.esp = mean(p.español),
  media.mate = mean(p.mate), 
  sd.esp=sd(p.español)/sqrt(length(p.español)),
  sd.mate = sd(p.mate)/sqrt(length(p.mate)))

primarias.resumen <- subset(primarias.resumen, num.esc > 5)

ggplot(primarias.resumen, aes(x=marginación, y=media.mate, ymax=media.mate+2*sd.mate, ymin=media.mate-2*sd.mate, 
  colour=TURNO, group=TURNO)) + geom_linerange() + 
  geom_point() + geom_line() + facet_wrap(~tipo)


ggplot(primarias.salida, aes(x=marginación, y= p.español, colour=tipo)) +
  geom_boxplot(outlier.colour = 'gray') +  facet_wrap(~NOM_ENT.1)
ggplot(primarias.salida, aes(x=marginación, y= p.mate, colour=tipo)) +
  geom_boxplot(outlier.colour = 'gray') +  facet_wrap(~tipo)
ggplot(primarias.salida, aes(x=marginación, y= score, colour=tipo)) +
  geom_boxplot(outlier.colour = 'gray') + facet_wrap(~tipo)
  