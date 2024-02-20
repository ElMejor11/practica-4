#Ejercicio 1. Importar_tabla_de_datos_spearheads_como_data_frame
#Install_readxl
installed.packages("readxl")
library(readxl)
#La ruta no muy larga
spear <- read_excel("C:/spearhead/spearheads.xlsx")
View(spear) ##Muestra una tabla en el editor de registros
str(spear) ##Tipo de datos campos
class(spear) ##Tipo de estructura de datos del objeto

#Convertimos a data frame con la función de as.data.frame(spear)
spear <- as.data.frame(spear)
class(spear)


#Ejercicio 2 renombra_las_variables
names(spear)[names(spear) == "Mat"] <- "Materiales"
names(spear)[names(spear) == "Con"] <- "Contexto"
names(spear)[names(spear) == "Cond"] <- "Conservacion"
names(spear)[names(spear) == "Loo"] <- "Loop"
names(spear)[names(spear) == "Peg"] <- "Remache"
names(spear)[names(spear) == "Date"] <- "Fecha"
names(spear)[names(spear) == "Maxle"] <- "Longitud_max"
names(spear)[names(spear) == "Socle"] <- "Longitud_encaje"
names(spear)[names(spear) == "Maxwi"] <- "Ancho_max"
names(spear)[names(spear) == "Upsoc"] <- "Ancho_encaje"
names(spear)[names(spear) == "Mawit"] <- "Ancho_max_encaje"
names(spear)[names(spear) == "Weight"] <- "Peso"
spear
View(spear)


#Ejercicio 3 asigna_las_etiquetas
spear$Contexto=factor(spear$Contexto, levels=c('1','2','3'), labels=c("s/c", "Habitacional", "Funerario"))
spear$Conservacion=factor(spear$Conservacion, levels=c(1,2,3,4), 
                          labels=c("Excelente", "Bueno", "Regular", "Malo"))
spear$Remache=factor(spear$Remache, levels=c(1,2), labels=c('Si', 'No'))
spear$Materiales=factor(spear$Materiales, levels=c(1,2), labels=c('Bronce', 'Hierro'))
View(spear)

#Ejercicio 4. Genera tablas de frecuencia de las variaciones

freq.Mat=table(spear$Materiales)
View (freq.Mat)
freq.Con=table(spear$Contexto)
View (freq.Con)
freq.Cond=table(spear$Conservacion)
View (freq.Cond)

#Ejercicio 5. Genera tablas cruzadas de Materiales 

cross.condcon=table(spear$Materiales, spear$Contexto)
cross.condcon
cross.condmat=table(spear$Conservacion, spear$Materiales)
cross.condmat

#Ejercicio 6
prop.Mat=prop.table(freq.Mat)
View(prop.Mat)
prop.Mat <- as.data.frame(prop.Mat)
prop.Mat$Porcentaje <- prop.Mat$Freq * 100
prop.Mat

#Ejercicio 7
prop.cross.condon=round(prop.table(cross.condcon)*100,0)
View(prop.cross.condon)
prop.cross.condmat=round(prop.table(cross.condmat)*100,0)
View(prop.cross.condmat)

#Ejercicio 8

bar.cond=barplot(table(spear$Conservacion))
bar.con=barplot(table(spear$Contexto))

#Ejercicio 9

grafico_barra_horizontal=barplot(table(spear$Materiales), horiz=TRUE)
xlim <- c(0, 1.2*max(25))
grafico_barra_horizontal=barplot(table(spear$Remache), horiz=TRUE)
xlim <- c(0, 1.2*max(25))

#Ejercicio 10


bar.cond=barplot(cross.condmat, width=0,85, ylim=c(0,sum(cross.condmat[,1])*1.1), 
                main = "Estado de conservación por Materiales",
                ylab = "Frecuencia",
                legend= T)

#Ejercicio 11

labs<- paste ("(", freq.cond,")\n", names(freq.cond), sep="")
pie(freq.cond, labels=labs, main="Conservacion_recuento", col=gray.colors(length(levels(factor(names freq.Cond))), start=0, 3, end=0, 8)

    
#Inconcluso# 

#Ejercicio 12



                                                                        

   