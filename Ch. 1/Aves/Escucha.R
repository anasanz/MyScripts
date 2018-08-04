

#####################En qué fincas no se han detectado especies por ESCUCHA###############

oe<-read.csv("Dades_ocells_escolta.csv",sep = ";",
             header=TRUE,fill = TRUE)

colnames(oe)[colnames(oe)=="Id_cens_detall"] <- "id_cens_detall"
colnames(oe)[colnames(oe)=="codi_finca"] <- "codi_finca_esc"
#códigos de finca

esc<-full_join(l,oe,by = "id_cens_detall")

#NA en Especie generados en censos donde no se ha hecho batida (escucha)


#???Cleaning data: Eliminar variables extra
names(esc)
esc<-esc[ ,c("id_cens_detall","Id_dades_ocells_esc","id_campanya_detall","Data","Tipus_cens","Especie","Sexe","Codi_unitat","Codi_finca","codi_finca_esc")]

#First: Substitute NA by NONE (Para después dentificar las fincas donde no hay ninguna Especie objetivo)

esc[,6][is.na(esc[,6])]<-'NONE'
class(esc$Especie)#Not possible because it is a factor

# Get levels and add "None"
levels<-levels(esc$Especie)
levels[length(levels) + 1] <- "NONE"

# refactor Species to include "None" as a factor level and replace NA with "None"
esc$Especie <- factor(esc$Especie, levels = levels)
esc$Especie[is.na(esc$Especie)] <- "NONE"
















#Join species detected in each census:ESCOLTA (Esto probablemente no esté bien hecho)

names(oe)
oe<-oe[ ,c("Id_dades_ocells_esc","id_cens_detall","codi_finca_esc","Especie","Sexe")]

full<-full_join(bat,oe,by.y = "id_cens_detall")


full<-arrange(full,id_cens_detall)