----------------#Overlap------------

# Overlap

install.packages("ovelap")
install.packages("circular")

library(overlap)

Crax_data <- read.csv("C_rubra.csv", header = T)

hora_radianes_Cr <- Crax_data$Time_2 * 2 * pi

Cr_SL <- hora_radianes_Cr  [Crax_data$Project.Name == "Selva Lacandona"]

Cr_Zap <- hora_radianes_Cr  [Crax_data$Project.Name == "El Zapotal"]

Zap_Cr <- densityPlot(Cr_Zap, xscale = 24,  xcenter = c("noon", "midnight"), add = F,rug = T, extend = "lightgray", main = "El Zapotal", xlab = "Time", ylab = "Density", bty = "L")
abline(v=c(6.30, 18+50/60), lty=4)

SL_Cr <- densityPlot(Cr_SL, xscale = 24,  xcenter = c("noon", "midnight"), add = F,rug = T, extend = "lightgray", main = "Montes Azules", xlab = "Time", ylab = "Density", bty = "L") 
abline(v=c(6.15, 18+30/60), lty=4)

#11. Si se quiere comparar la actividad de dos especies o entre sexos de la misma especie o entre temporadas, se puede estimar y graficar el coeficiente de traslape
#El coeficiente de traslape es una medida no parametrica que estima el área de coincidencia bajo dos curvas de densidad (Schmid y Schmidt 2006).
#El rango del coeficiente va de 0 a 1, dónde 1 = actividad igual y 0 = actividad diferente (Schmid y Schmidt 2006, Ridout y Linkie 2009).
#Se recomienda utilizar adjust = 0.8 para estimar delta 1, adjust = 1 para estimar delta 4 y adjust = 4 para estimar delta 5.
#El mejor estimador depende del tamaño de la muestra más pequeña, cuando la muestra es menor a 50 registros se utilizá delta 1, cuando es mayor de 75 se utilizá delta 4

Cr_SL
Cr_Zap

min(length(Cr_SL), length(Cr_Zap))
max(length(Cr_SL), length(Cr_Zap))

Cr_delta <- overlapEst(Cr_SL, Cr_Zap)

Cr_delta

#Después se hace la grafica con la siguiente función:
Comp_SL_Zap <- overlapPlot(Cr_SL, Cr_Zap, main = " ", linecol = c("black", "blue"), lty = c(1,3), rug = T, bty = "L", xlab = "Time", ylab = "Density")
legend("topright", inset = c(0,0), title = "Δ=0.66 (CI= 0.54-0.78)", c("Montes Azules", "El Zapotal"), lty= c(1,3), col = c("gray25", "seagreen"), bty = "n")

Cr_remuestreo_SL <- resample(Cr_SL, 10000)
Cr_remuestreo_Zap<- resample(Cr_Zap, 10000)

#Ahora utlizamos los datos remuestreados para estimar los intervalos de confianza

Cr_remuestreo <- bootEst(Cr_remuestreo_SL, Cr_remuestreo_Zap, adjust = c(NA, 1, NA))

#El valor de adjust dependerá de la delta que se utilizará por ejemplo si es delta1 se utiliza "adjust= c(0.8, NA, NA)".

Cr_SL_delta_remuestreo  <- colMeans(Cr_remuestreo_SL)
Cr_SL_delta_remuestreo

Cr_Zap_delta_remuestreo  <- colMeans(Cr_remuestreo_Zap)
Cr_Zap_delta_remuestreo

Cr_Reser_remuestreo <- bootEst(Cr_remuestreo_SL, Cr_remuestreo_Zap, adjust = c(NA, 1, NA))

#Para extaer los intervalos de confianza se utiliza la siguiente función:

Cr_Zap_delta_remuestreo2 <- Cr_remuestreo_Zap[, 2]
bootCI(Cr_Zap_delta_remuestreo [2], Cr_Zap_delta_remuestreo)

Cr_SL_delta_remuestreo2 <- Cr_remuestreo_SL[, 2]
bootCI(Cr_SL_delta_remuestreo [2], Cr_SL_delta_remuestreo2 )

Cr_SL
Cr_Zap

overlapPlot(Cr_SL, Cr_Zap, xscale = 24, xcenter = c("noon", "midnight"), main = " ", linetype = c(1, 2), linecol = c("black", "blue"), linewidth = c(1, 1), olapcol = "lightgrey", rug=FALSE, extend=NULL,
            n.grid = 128, kmax = 3, adjust = 1, xlab = "Hora", ylab = "Densidad")

overlapEst(Cr_SL, Cr_Zap, kmax = 3, adjust=c(0.8, 1, 4), n.grid = 128,type=c("all", "Dhat1", "Dhat4", "Dhat5"))

est <-overlapEst(Cr_SL, Cr_Zap, type="Dhat4")

boots_Cr <- bootstrap(Cr_SL, Cr_Zap, 99, type="Dhat4")
mean(boots_Cr )
hist(boots_Cr )
abline(v=est, col='red', lwd=2)
abline(v=mean(boots2), col='blue', lwd=2, lty=3)

boots_Cr2 <- bootstrap(Cr_SL, Cr_Zap, 99, type="Dhat4")
mean(boots_Cr2)
hist(boots_Cr2)
abline(v=est, col='red', lwd=2)
abline(v=mean(boots2), col='blue', lwd=2, lty=3)

# If the smaller sample is less than 50, Dhat1 gives the best estimates, together with
# confidence intervals from a smoothed bootstrap with norm0 or basic0 confidence interval.
length(Cr_SL)
length(Cr_Zap)


# Calculate estimates of overlap:
( Dhats <- overlapEst(Cr_SL, Cr_Zap ))  # or just get Dhat1

( Dhat4 <- overlapEst(Cr_SL, Cr_Zap, type="Dhat4"))

bs_Cr <- bootstrap(Cr_SL, Cr_Zap, 999, type="Dhat4")
mean(bs_Cr)
hist(bs_Cr)
abline(v=Dhat4, col='red', lwd=2)
abline(v=mean(bs2), col='blue', lwd=2, lty=3)
#Get confidence intervals:
bootCI(Dhat4, bs_Cr)['norm0', ]
bootCI(Dhat4, bs_Cr)['basic0', ]

----------------#circular-----------
library(activity) 
library(CircStats) 
library(Directional) 
library(ggplot2)
library(circular)
library(tidyverse)

Crax_data <- read.csv("C_rubra.csv", header = T)

# Crear el data frame
lacandona_df <- Crax_data %>% filter(Project.Name == "Selva Lacandona") %>% select(Camera.Trap.Name, Photo.Date, Photo.time, Time_2)

zapotal_df <- Crax_data %>% filter(Project.Name == "El Zapotal") %>% select(Camera.Trap.Name, Photo.Date, Photo.time, Time_2)

lacandona_time <- Crax_data %>%
  filter(Project.Name == "Selva Lacandona") %>% select(Time_2)

zapotal_time <- Crax_data %>%
  filter(Project.Name == "El Zapotal") %>%
  select(Time_2)

Zap_cir <- subset(Crax_data, Project.Name == "El Zapotal") 
SL_cir <- subset(Crax_data, Project.Name == "Selva Lacandona")

#Representaciones gráficas de datos circulares.  diagramas de rosa convirtiendo los datos circulares en un objeto circular primero y usando luego rose.diag.

Zap_cir_Cr<- circular::circular(Zap_cir$Time_2*24, units = "hours", template =  "clock24")
SL_cir_Cr<- circular::circular(SL_cir$Time_2*24, units = "hours", template =  "clock24")
#convertimos en horas los valores temporales expresados como proporción
#Fig. 1:

rose.diag (SL_cir_Cr, main = "Montes Azules", bins = 24, prop = 2.0, shrink = 0.9, col="#3B8945", tol = 0.03, tcl = 0.04,border="black", xlim = c(-1, 1), ylim = c(-1, 1)) 

rose.diag (Zap_cir_Cr, main = "El Zapotal", bins = 24, prop = 1.8, shrink = 0.9, col="#699AC2", tol = 0.03, tcl = 0.04,border="black", xlim = c(-1, 1), ylim = c(-1, 1)) 
#Test entre dos distribuciones circulares: test de Mardia-Watson-Wheeler aplicado a los patrones actividad de los animales

circular::watson.wheeler.test(list(Zap_cir_Cr, SL_cir_Cr))

# prueba de watson.wheeler.test(list(x1,x2))

# Example used in Zar (1999)
x1 <- circular ((lacandona_time$Time_2),
                units="degrees", template="geographics")

x2 <- circular((zapotal_time$Time_2),
               units="degrees", template="geographics")

watson.wheeler.test(list(x1,x2))

#Ejemplo_2
zap.circ <- circular::circular(zapotal_time$Time_2*24, units = "hours", template =  "clock24") 
lacandona.circ <- circular::circular(lacandona_time$Time_2*24, units = "hours", template =  "clock24")

#convertimos en horas los valores temporales expresados como proporción
#Fig. 1:
circular::rose.diag(zap.circ, main = "El Zapotal", units = "hours", bins = 24,prop = 1.7, shrink=1, col=7)
#para poner color ponemos col=3

circular::rose.diag(lacandona.circ, main = "Selva Lacandona",  bins = 24, prop = 2, shrink=1, col=4)

#Estadística circular. Se ruerda que los datos se convierten de grados a radianes usando la fórmula 360o= 2𝜋 rad.
Zap.sum <- Directional::circ.summary(zapotal_time$Time_2*2*pi, plot = F) 
Zap.sum$mesos 
#media angular en radianes
## [1] 3.217906

Zap.sum$circstd 
#desviación estándar circular en radianes ## [1] 0.01915859
lacandona.sum <- Directional::circ.summary(lacandona_time$Time_2*2*pi, plot = F) 
lacandona.sum$mesos 
#media angular en radianes
## [1] 2.904522

lacandona.sum$circstd 
#desviación estándar circular en radianes ## [1] 0.01513543

# Test entre dos distribuciones circulares: test de Mardia-Watson-Wheeler aplicado a los patrones actividad de los animales

circular::watson.wheeler.test(list(zap.circ, lacandona.circ))

