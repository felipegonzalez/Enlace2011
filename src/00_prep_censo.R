library(ProjectTemplate)
load.project()

tab.iter.loc$tipo <- ifelse(tab.iter.loc$POBTOT < 5000, "Rural","Urbana" )

# ===============================
# = Construir tabla por municpio =
# ===============================
# =====================================
# = Crea tab.mun.completa y nombres.2 =
# =====================================
tab.mun.completa <- ddply(tab.iter.loc, 
    c("ENTIDAD", "NOM_ENT", "MUN","NOM_MUN"), 
    function(df){ 
         vec.out <- (apply(df[,10:195], 2 ,sum, na.rm=TRUE)) 
         vec.out 
    }, .progress="text", .parallel=FALSE) 
#cache('tab.mun.completa')

head(tab.mun.completa)
#Número de municipios
nrow(tab.mun.completa)


nombres.tab.mun <- names(tab.mun.completa[,165:189])
#nombres.tab.mun

prop.servicios <- t(scale(t(tab.mun.completa[,165:189]),center=FALSE, 
    scale=tab.mun.completa$VIVPAR_HAB))

nombres.2 <- paste("p_",colnames(prop.servicios), sep="")
tab.mun.completa[, nombres.2] <- prop.servicios

names(tab.mun.completa)[1] <- "CVE_ENT"
names(tab.mun.completa)[3] <- "CVE_MUN"

cache("tab.mun.completa")
cache("nombres.2")

