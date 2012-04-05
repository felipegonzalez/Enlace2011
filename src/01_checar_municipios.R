library(ProjectTemplate)
load.project()

# ===================================
# = Correción nombres de municipios =
# ===================================
## Crea primarias.x y modifica tab.mun.completa
muns.censo <- tolower(tab.mun.completa$NOM_MUN)
muns.enlace <- tolower(primarias$MUNICIPIO)

muns.censo.sa <- sapply(muns.censo, function(nom){
    nom.1 <- str_replace_all(nom, "á","a")
    nom.1 <- str_replace_all(nom.1, "é","e")
    nom.1 <- str_replace_all(nom.1, "í","i")
    nom.1 <- str_replace_all(nom.1, "ó","o")
    nom.1 <- str_replace_all(nom.1, "ú","u")
    nom.1
})

dat.muns.censo <- data.frame(entidad = tab.mun.completa$NOM_ENT, muns.censo.sa)
write.csv(arrange(dat.muns.censo, entidad, muns.censo.sa), file="./out/nombres_censo.csv")


muns.enlace.sa <- sapply(muns.enlace, function(nom){
    nom.1 <- str_replace_all(nom, "á","a")
    nom.1 <- str_replace_all(nom.1, "é","e")
    nom.1 <- str_replace_all(nom.1, "í","i")
    nom.1 <- str_replace_all(nom.1, "ó","o")
    nom.1 <- str_replace_all(nom.1, "ú","u")
    nom.1
})

table(muns.enlace.sa %in% muns.censo.sa)

dat.muns <- data.frame(muns.enlace.sa, en.censo = muns.enlace.sa %in% muns.censo.sa,
        entidad = primarias$ENTIDAD)
nombres.mal <- unique(subset(arrange(dat.muns, en.censo, muns.enlace.sa), 
    en.censo == FALSE)[,c("entidad","muns.enlace.sa")])
nombres.ok <- c("acambaro", "atltzayanca", "amanalco", "la antigua", "san juanito de escobedo",
    "el arenal", "la barca", "briseñas","chicxulub pueblo","las choapas",
    "ayala", "zapotlan el grande", "coacalco de berriozabal", "la colorada", "cosamaloapan de carpio",
    "coyame del sotol", "cuapiaxtla", "cuautitlan de garcia barragan","cueramaro",
    "dr. arroyo", "dr. belisario dominguez", "dr. coss","dr. gonzalez",
    "dolores hidalgo cuna de la independencia nacional",
    "ecatepec de morelos", "del nayar", "gral. bravo", "gral. escobedo" ,
    "gral. teran", "gral. treviño", "gral. zaragoza", "gral. zuazua",
    "güemez", "el higo", "huanimaro", "la huerta", "ixtlahuacan de los membrillos",
    "xalatlaco","jerecuaro", "leon", "el limon", "santa maria del oro", "la manzanilla de la paz",
    "mexicaltzingo", "las minas", "moroleon", "muñoz de domingo arenas",
    "ozuluama de mascareñas", "la perla", "la piedad", "puerto vallarta",
    "purisima del rincon", "los reyes", "san diego de la union",
    "san francisco del rincon", "san jose iturbide","atizapan",
    "santiago maravatio", "santo tomas", "santo tomas tamazulapan",
    "san cristobal de la barranca", "san gabriel", "san lorenzo axocomanitla",
    "san martin de bolaños", "soyaniquilpan de juarez", "santa maria de los angeles",
    "techaluta de montenegro", "alamo temapache", "tingüindin",
    "tixmehuac", "tixpehual", "tlajomulco de zuñiga", "san francisco tetlanohcan",
    "las vigas de ramirez", "villagran", "yahualica de gonzalez gallo",
    "zapotitlan del rio", "ziltlaltepec de trinidad sanchez santos","zontecomatlan de lopez y fuentes" )

dat.muns$nombre.corregido <- as.character(dat.muns$muns.enlace.sa)

#primera corrección
for(i in 1:length(nombres.mal$muns.enlace.sa)){
    indices.1 <- which(dat.muns$muns.enlace.sa==nombres.mal$muns.enlace.sa[i])
    dat.muns$nombre.corregido[indices.1] <- nombres.ok[i]
}
## segunda
dat.muns$nombre.corregido[dat.muns$muns.enlace.sa=="tlalnepantla" &
    dat.muns$entidad=="MÉXICO"] <- "tlalnepantla de baz"
dat.muns$nombre.corregido[dat.muns$muns.enlace.sa=="jose azueta" &
    dat.muns$entidad=="GUERRERO"] <- "zihuatanejo de azueta"

nuevos <- unique(data.frame(original=dat.muns$muns.enlace.sa, corregido=dat.muns$nombre.corregido))
nuevos$dif <-  as.character(nuevos$original)!=as.character(nuevos$corregido)
arrange(nuevos, dif)



dat.muns.2 <- data.frame(dat.muns$nombre.corregido, en.censo = dat.muns$nombre.corregido %in% muns.censo.sa,
        entidad = primarias$ENTIDAD)
    
head(arrange(dat.muns.2, en.censo),100)


tab.mun.completa$mun.corregido <- muns.censo.sa
primarias$mun.corregido <- dat.muns$nombre.corregido

primarias$NOM_ENT <- primarias$ENTIDAD
primarias$ENTIDAD <- primarias$CVE_ENT
primarias.x <- join(primarias, tab.mun.completa, by=c("CVE_ENT", "mun.corregido"))
nrow(primarias.x)
head(primarias.x)
cache("primarias.x")

# ================
# = Diagnósticos =
# ================
primarias.x$faltante.internet <- !is.na(primarias.x$p_VPH_INTER)
#Ojo: falta el de zapotitlan del rio en oaxaca - pero hay una sola escuela
table(primarias.x$faltante.internet)
sub.temp <- subset(primarias.x, faltante.internet==FALSE)
