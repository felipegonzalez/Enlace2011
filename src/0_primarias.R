library(ProjectTemplate)
load.project()

# ==========================================================================
# = Agregar variables de marginación, calcular número de buenos/excelentes =
# ==========================================================================
primarias.base <- primarias
primarias <- primarias.x
primarias$tot.primaria <- (primarias$ALUMNEVAL_6G - primarias$ALUMRESPOCONF_6G) +
    (primarias$ALUMNEVAL_5G - primarias$ALUMRESPOCONF_5G) +
    (primarias$ALUMNEVAL_4G - primarias$ALUMRESPOCONF_4G) +
    (primarias$ALUMNEVAL_3G - primarias$ALUMRESPOCONF_3G)

primarias$no.bueno <- floor((primarias$ALUMNEVAL_6G - primarias$ALUMRESPOCONF_6G)*
    (primarias$PORALUM_ESP_6G_EXCEL+primarias$PORALUM_ESP_6G_BUENO )/100) +
    floor((primarias$ALUMNEVAL_5G - primarias$ALUMRESPOCONF_5G) *
        ( primarias$PORALUM_ESP_5G_EXCEL+primarias$PORALUM_ESP_5G_BUENO )/100)+
    floor((primarias$ALUMNEVAL_4G - primarias$ALUMRESPOCONF_4G) *
        ( primarias$PORALUM_ESP_4G_EXCEL+primarias$PORALUM_ESP_4G_BUENO )/100)+
    floor((primarias$ALUMNEVAL_3G - primarias$ALUMRESPOCONF_3G) *
        ( primarias$PORALUM_ESP_3G_EXCEL+primarias$PORALUM_ESP_3G_BUENO )/100)

primarias$no.malo <- primarias$tot.primaria - primarias$no.bueno

primarias$ESCUELA.num <- factor(1:nrow(primarias))
primarias$marginación <- factor(str_trim(as.character(primarias$GRAD_MARG)),
    levels = c("MUY BAJO","BAJO","MEDIO","ALTO", "MUY ALTO"))
primarias$tipo <- factor(str_trim(as.character(primarias$TIPO_ESCUELA)),
    levels = c("GENERAL","PARTICULAR","INDÍGENA", "CONAFE"))

primarias$tipo.marg <- factor(as.character(interaction(primarias$tipo, primarias$marginación)))

# Agregar datos de municipios
#primarias.1 <- join(primarias, tab.mun.completa[ , 
#    c( "CVE_ENT", "CVE_MUN", "POBTOT", nombres.2)], by =c("CVE_ENT", "CVE_MUN"))

#primarias.2 <- subset(primarias.1, tot.primaria > 30 & TIPO_ESCUELA!="CONAFE")
#primarias.s <- primarias.2[ sample(1:nrow(primarias.2), 10000), ] 
