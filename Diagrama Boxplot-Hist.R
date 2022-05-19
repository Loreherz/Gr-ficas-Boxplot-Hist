install.packages("AER")
library(AER)
data(package="AER")
data("CPS1985") 
attach(CPS1985)
View(CPS1985)

p=hist(wage,
     breaks = 20,
     main = "Distribución de salario (dólares por año)",
     freq=FALSE,
     xlab="Salario",
     ylab= "Frecuencia relativas",
     xlim=c(0,50),
     ylim = c(0,0.12),
     col="#ff9900",
     border="#0040ff")
p

curve(dnorm(x, mean=mean(wage),sd=sd(wage)), add = TRUE,
col= "#00ff00",lwd=3)


# Si queremos ubicar la media con una linea punteada 

abline(v=mean(wage), lwd=2, lty=3, col="darkblue")

      
# su probabilidad

val=round(p$density, digits=2) # redondear valores a dos digitos 

text(p$mids, p$density, labels = val, adj=c(0.5,-0.5), cex=0.5 )


text(p$mids, p$density, labels= val,adj=c(0.5,-0.5), cex=0.5)

#################### Diagrama de cajas ###############

names(CPS1985)
boxplot(wage)

# mejoremos el gráfico

boxplot(wage,
        main="Salario (dólarespor hora)",
        ylab="Salario",
        col="pink",
        border="purple"
)

summary(wage)        

p<-boxplot(wage,
           main="Salario (dólarespor hora)",
           ylab="Salario",
           col="pink",
           border="purple"
)
p

# ¿Cómo serían los salarios de los hombres y las mujeres?

p<-boxplot(wage~gender)
p

p$out[1:7]

# valor maximo 

max(p$out[1:7])

# Valore atipicos mujeres 

p$out[8:length(p$out)]
max(p$out[8:length(p$out)])

# Representación de una mejor manera 

boxplot(wage~gender,
        main="Salario(dólares por hora)según género",
        ylab="Salario",
        names=c("Hombres","mujeres"))

# vamos a sacar los datos solamente de nujeres y hombres 
# subconjunto 

datos_mujeres<-subset(CPS1985,gender=="female") # separa la base de datos en un subconjunto

datos_mujeres

datos_hombres<-subset(CPS1985,gender=="male")

datos_hombres

# Boxplot de manera vertical 

p<-boxplot(wage~gender,
           main ="Salario(dólares por hora)según género",
           ylab = "Salario",
           names=c("Hombres","mujeres"),
           horizontal = TRUE,
           xlab= "Genero",
           notch=TRUE) # Un tipo de diagrama de caja diferente
p

# cambiar el nombre de las etiquetas 

p<-boxplot(wage~gender,
           main ="Salario(dólares por hora)según género",
           ylab = "Género",
           names=c("Hombres","mujeres"),
           horizontal = TRUE,
           xlab= "Salario",
           notch=TRUE) # Un tipo de diagrama de caja diferente
p

# Cálculo del valor de la media en le boxplot 

points(mean(datos_mujeres$wage),2,pch=25, bg="red",cex=1.5)

points(mean(datos_hombres$wage),1,pch=19, bg="blue",cex=1.5)

# colores con rainbow

p<-boxplot(wage~gender,
           main ="Salario(dólares por hora)según género",
           ylab = "Género",
           names=c("Hombres","mujeres"),
           horizontal = TRUE,
           xlab= "Salario",
           notch=TRUE,
           col=rainbow(2,alpha=0.5)) # Un tipo de diagrama de caja diferente
p


# colores con heat.colors

p<-boxplot(wage~gender,
           main ="Salario(dólares por hora)según género",
           ylab = "Género",
           names=c("Hombres","mujeres"),
           horizontal = TRUE,
           xlab= "Salario",
           notch=TRUE,
           col=heat.colors(2)) # Un tipo de diagrama de caja diferente
p


unique(CPS1985$occupation)# me permite ver las variables 

# Salario por ocupación 

datos_worker<-subset(CPS1985,occupation=="worker")
datos_management<-subset(CPS1985,occupation=="management")
datos_sales<-subset(CPS1985,occupation=="sales")
datos_office<-subset(CPS1985,occupation=="office")
datos_services<-subset(CPS1985,occupation=="services")
datos_technical<-subset(CPS1985,occupation=="technical")


# Diagrama de caja 

p<-boxplot(wage~occupation,
           main ="Salario(dólares por hora)según género",
           ylab = "Cargo",
           horizontal = TRUE,
           xlab= "Salario",
           notch=F,
           col=rainbow(6)) # Un tipo de diagrama de caja diferente
p


####### otra forma para el color 


p<-boxplot(wage~occupation,
           main ="Salario(dólares por hora)según género",
           ylab = "Cargo",
           horizontal = TRUE,
           xlab= "Salario",
           notch=F,
           col=rainbow(length(unique(occupation))),alpha=0.6,
           border =rainbow(length(unique(occupation)),v=0.7)) # Un tipo de diagrama de caja diferente
p


############## Media por ocupación 

points(mean(datos_worker$wage),1, pch=25, bg="yellow", cex=1.5)
points(mean(datos_technical$wage),2, pch=16, bg="red", cex=1.5)
points(mean(datos_services$wage),3, pch=22, bg="red", cex=1.5)
points(mean(datos_office$wage),4, pch=25, bg="darkblue", cex=1.5)
points(mean(datos_sales$wage),5, pch=22, bg="red", cex=1.5)
points(mean(datos_management$wage),6, pch=19, bg="red", cex=1.5)


####### Realizar un bloxplot por género y por cargo ###########


p<-boxplot(wage~gender*occupation)

p<-boxplot(wage~gender+occupation)

a<-rainbow(6)
b<-rainbow(6, alpha = 0.2)
c<-rainbow(6, v=0.5)

boxplot(wage~occupation,
        col=b)

boxplot(wage~gender*occupation,
        col=b)


boxplot(wage~occupation,
        
        col=b,
        boxcol=c, # me detalla el color del borde de la caja
        medcol=c, # detalla el color de la mediana 
        whiskcol=a, # detalla el color de los bogotes (lineas punteadas)
        staplecol=c, # detalla el color del minimo y el maximo
        outcol=c,     #detalla el color de los valores atipicos
        outbg=c, # relleno de los datos atipicos
        pch=20, # define el tipo de punto de los datos
        cex = 1, # define el tamaño de los puntos 
        horizontal=TRUE,
        main="Salario (Dólares por hora"
        
  )

# Cambiar los nombres a español 


boxplot(wage~occupation,
        
        col=b,
        boxcol=c, # me detalla el color del borde de la caja
        medcol=c, # detalla el color de la mediana 
        whiskcol=a, # detalla el color de los bogotes (lineas punteadas)
        staplecol=c, # detalla el color del minimo y el maximo
        outcol=c,     #detalla el color de los valores atipicos
        outbg=c, # relleno de los datos atipicos
        pch=20, # define el tipo de punto de los datos
        cex = 1, # define el tamaño de los puntos 
        horizontal=TRUE,
        main="Salario (Dólares por hora",
        ylab="ocupación",
        xlab="Salario",
        names=c("Empleado","Tecnico","servicio","oficina","ventas","")
)

# Modificación de las margenes 


opar<-par()

par(mar=c(5,6,4,2)) # 5 lo corre hacia la parte inferior, 6 = corre a la izquierda, 4 = lo corre a la parte superior, 2 = corre hacia la derecha.

boxplot(wage~occupation,
        
        col=b,
        boxcol=c, # me detalla el color del borde de la caja
        medcol=c, # detalla el color de la mediana 
        whiskcol=a, # detalla el color de los bogotes (lineas punteadas)
        staplecol=c, # detalla el color del minimo y el maximo
        outcol=c,     #detalla el color de los valores atipicos
        outbg=c, # relleno de los datos atipicos
        pch=15, # define el tipo de punto de los datos
        cex = 1.15, # define el tamaño de los puntos 
        horizontal=TRUE,
        main="Salario (Dólares por hora",
        ylab=NULL,
        xlab=NULL,
        ylim =c(0,50),
        las=1, # cambia la orientacion de las etiquetas
        frame=F
        
)

par (opar)


# leyenda anexa la grafico


boxplot(wage~occupation,
        
        col=b,
        boxcol=c, # me detalla el color del borde de la caja
        medcol=c, # detalla el color de la mediana 
        whiskcol=a, # detalla el color de los bogotes (lineas punteadas)
        staplecol=c, # detalla el color del minimo y el maximo
        outcol=c,     #detalla el color de los valores atipicos
        outbg=c, # relleno de los datos atipicos
        pch=15, # define el tipo de punto de los datos
        cex = 1, # define el tamaño de los puntos 
        horizontal=TRUE,
        main="Salario (Dólares por hora",
        ylab=NULL,
        xlab="Salario",
        yaxt="n") # elimina el eje vertical
        
par(opar)        
legend("bottomright",title= " Salario",levels(occupation),fill=b)

str(CPS1985)

