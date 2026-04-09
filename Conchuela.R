#Datos de conchuela
require(dplyr)
#Si quieren usar el último gráfico que sale bonito necesitan
#este paquete
require(ggplot2)


getwd()
list.files()
conchuela <- read.table("conchuela.csv", sep = ",",header = TRUE)
str(conchuela)

conchuela$Especie <- as.factor(conchuela$Especie)
conchuela$Condicion_del_arbol <- as.factor(conchuela$Condicion_del_arbol)

entrada.conchuela <-table(conchuela$Especie,conchuela$Presencia.de.Conchuela)
entrada.conchuela.plot <- as.data.frame(entrada.conchuela)

ji.cuadrada <-chisq.test(entrada.conchuela)
ji.cuadrada
ji.cuadrada$stdres

#Para las gráficas bonitas necesitan el paquete ggplot2

ggplot(entrada.conchuela.plot, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity",position = "dodge")+
  ylab("Frecuencias")+
  xlab("Presencia de Daño")+
  labs(fill = "Especie")+
  theme_bw()


table(conchuela$User)


#Analisis de la SEVERIDAD de danio entre especies

analisis.conchuela.danio <-table(conchuela$Condicion,conchuela$Especie)
entrada.conchuela.sp.danio <-as.data.frame(table(conchuela$Condicion,conchuela$Especie))

para.ji.ciadrada <-table(conchuela$Condicion,conchuela$Especie)
analisis.danio <- chisq.test(para.ji.ciadrada)
analisis.danio$stdres

ggplot(entrada.conchuela.sp.danio, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity",position = "dodge")+
  ylab("Frecuencias")+
  xlab("Especie")+
  labs(fill = "Nivel de Daño")+
  theme_bw()



