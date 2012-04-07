library(ProjectTemplate)
load.project()

##Temp
#primarias.s <- primarias[ sample(1:nrow(primarias), 1000), ] 

primarias.s <- subset(primarias, tot.primaria > 30 & TIPO_ESCUELA!="CONAFE")
primarias.s <- primarias.s[sample(1:nrow(primarias.s), 5000),]

no.escuelas <- nrow(primarias.s)
no.bueno.esp <- primarias.s$no.bueno.esp
no.bueno.mat <- primarias.s$no.bueno.mat
no.eval <- primarias.s$tot.primaria

marg <- as.numeric(primarias.s$marginación)
tipo <- as.numeric(primarias.s$tipo)
n.marg <- max(marg)
n.tipo <- max(tipo, na.rm =TRUE)

x.lavadora <- as.numeric(scale(primarias.s$p_VPH_LAVAD))
x.piso.tierra <- as.numeric(scale(primarias.s$p_VPH_PISODT))
x.autom <- as.numeric(scale(primarias.s$p_VPH_AUTOM))
x.telef <- scale(primarias.s$p_VPH_TELEF)
x.celular <- as.numeric(scale(primarias.s$p_VPH_CEL))
x.tv <- scale(primarias.s$p_VPH_TV)
x.radio <- scale(primarias.s$p_VPH_RADIO)
x.internet <- as.numeric(scale(primarias.s$p_VPH_INTER))
x.pc <- as.numeric(scale(primarias.s$p_VPH_PC))


## Variables centradas por grado de marginación
primarias.s <- ddply(primarias, c('marginación'), transform,
    pc.cent = p_VPH_PC - mean(p_VPH_PC, na.rm = TRUE),
    internet.cent = p_VPH_INTER - mean(p_VPH_INTER, na.rm = TRUE),
    lavadora.cent = p_VPH_LAVAD - mean(p_VPH_LAVAD, na.rm = TRUE),
    celular.cent = p_VPH_CEL - mean(p_VPH_CEL, na.rm = TRUE),
    .progress = 'text')

pc.cent <- primarias.s$pc.cent
internet.cent <- primarias.s$internet.cent
lavadora.cent <- primarias.s$lavadora.cent
celular.cent <- primarias.s$celular.cent
estado <- primarias.s$ENTIDAD
n.estados <- 32

jags.inits <- function(){
     list('a.marg'=rep(0,n.marg), 'a.tipo'=rep(0,n.tipo), 
    # 'tau.marg'=runif(1),
    # 'tau.tipo'=runif(1),
     'sigma'=runif(1)
     )
 }

jags.data <- c('no.bueno.esp', 'no.bueno.mat', 'no.eval', 'no.escuelas', 'tipo', 'marg', 'n.marg',
    'n.tipo','pc.cent', 'internet.cent', 'lavadora.cent', 'celular.cent', 'estado', 'n.estados')

jags.params <- c('a.marg.adj', 'a.tipo.adj', 'a.adj', 'sigma.mat', 'sigma.esp',
    'tau.mat', 'tau.esp', 'p.mat.bound', 'p.esp.bound','a.pc','a.internet','a.estado')

jags.fit <- jags(model.file = './src/modelo_logit.model', 
    data = jags.data, inits = jags.inits, 
    parameters.to.save = jags.params,
    n.chains = 2, n.burnin=1000, n.thin = 5, DIC = FALSE,
    n.iter = 6000)

medias.escuelas <- jags.fit$BUGSoutput$mean$p.bound  
hist(medias.escuelas)
plot(jags.fit)
traceplot(jags.fit$BUGSoutput)