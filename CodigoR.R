library(tidyverse)
library(knitr)
library(kableExtra)
library(readxl)
library(car)

#Medias por factor

mediaDeter <- tapply(Promedio,Detergente, mean)
desvD <- tapply(Promedio, Detergente, sd)

mediaTela <- tapply(Promedio, Tela, mean)
desvS <- tapply(Promedio, Tela, sd)

Interaccion <- tapply(Promedio, list(Detergente, Tela), mean)
InteraccionSd <- tapply(Promedio, list(Detergente, Tela), sd)

#gráfico de los efectos

plot.design(Promedio~Tela*Detergente, main = "Grafico de efectos", las = 1)

interaction.plot(Tela, Detergente, Promedio, type = "b", pch = 19, fixed = T, xlab = "Tipo de tela", ylab = "Calificación promedio", main = "Grafico de interaccion de los efectos",
                 las = 1, col=c("red", "blue", "black"), cex.axis=.7, leg.bty =0.1)

interaction.plot(Detergente, Tela, Promedio, type = "b", pch = 19, fixed = T, xlab = "Tipo de tela", ylab = "Calificación promedio", main = "Grafico de interaccion de los efectos", 
                 las = 1, col=c("red", "blue", "black", "green"), cex.axis=.7, leg.bty =0.1)

#Anova del modelo

modPromedio <- aov(Promedio~Detergente*Tela)

#Normlaidad

residuales <- residuals(modPromedio)
test <- shapiro.test(residuales)
qqnorm(residuales,main="Gráfico de normalidad de los residuos del modelo", cex.main = 1, cex = 1, pch = 20) 
qqline(residuales,col="#7d3434", lwd=2)
legend("topleft",title = "Test de Shapiro-Wilk",title.col="#7d3434",title.adj= 1, legend=rbind(c("Statistic W","p-value"), 
                                                                                               round(c(test$statistic,test$p.value),digits=6)), cex=.8)
#homogeniedad e independencia

dwtest <- durbinWatsonTest(modPromedio)

plot(residuals(modPromedio), pch=19, col=1, 
     main = "Grafico de órden vs. Residuales del modelo", 
     xlab = "Órden", ylab = "Residuales")
abline(h=0, col="#7d3434", lwd = 3)

res<-ggplot(modPromedio, aes(x=modPromedio$fitted.values, y=modPromedio$residuals))+
  geom_point()+
  geom_hline(yintercept=0,
             col="#7d3434", linetype="dashed")+
  theme(axis.title=element_text(size=10,face="bold"),
        axis.text=element_text(size=10, face="bold"))+
  xlab("Valores ajustados")+
  ylab("Residuales")+
  ggtitle("Residuales vs. Valores ajustados")
res

b1 <- bartlett.test(residuales, Detergente)
b2 <- bartlett.test(residuales, Tela)

#boxplot para efectos

ggplot(datos,aes(y=Promedio, x=Detergente, col=Detergente, fill=Detergente))+labs(title='Box-plot según el tipo de Detergente')+
  geom_boxplot(alpha=0.4)+
  stat_summary(aes(y=Promedio,x=Detergente, group=Detergente),
               fun=mean, geom="point", shape=20, 
               size=3, color="yellow")+theme(legend.title=element_text( face='bold', size=14, color='black', hjust=0.5), title=element_text( face='bold', size=14, color='black', hjust=0.5))

ggplot(datos,aes(y=Promedio, x=Tela, col=Tela, fill=Tela))+labs(title='Box-plot según el tipo de Tela')+
  geom_boxplot(alpha=0.4)+
  stat_summary(aes(y=Promedio,x=Tela, group=Tela),
               fun=mean, geom="point", shape=20, 
               size=3, color="yellow")+theme(legend.title=element_text( face='bold', size=14, color='black', hjust=0.5), title=element_text( face='bold', size=14, color='black', hjust=0.5))

# método duncan para medias del tratamiento Tela
duncan_d <- duncan.test(modPromedio, "Detergente")
#duncan_d

# método duncan para medias del tratamiento Tela
duncan_d <- duncan.test(modPromedio, "Detergente")
#duncan_d

# método duncan para medias del tratamiento Tela
duncan_t <- duncan.test(modPromedio, "Tela")
#duncan_t

