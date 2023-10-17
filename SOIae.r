library(fda)
library(ggplot2)
library(plotly)
library(tidyr)
library(gridExtra)
stda = read.csv('/home/yashar/Downloads/standarized_data.csv')
ano = read.csv('/home/yashar/Downloads/anomaly.csv')
stda=stda[-73,]
ano=ano[-73,]
summary(ano)
ano$SEP=as.numeric(ano$SEP)
stda$SEP=as.numeric(stda$SEP)
anot=t(ano[,-1])
stdat=t(as.matrix(stda[,-1]))
rownames(stda)=as.vector(stda[,1])
rownames(ano)=paste0("ANO",as.vector(ano[,1]))
ano=ano[,-1]
stda=stda[,-1]
head(stdat)
dim(stdat)
colnames(stdat)=as.vector(stda[,1])
colnames(anot)=paste0("ANO",as.vector(ano[,1]))
datt=cbind(stdat,anot[,33],anot[,60])
head(datt)
which(apply(anot,2,min)==-6)
maxstdat=apply(stda[-1,],2,max)
maxanot=apply(ano[-1,],2,max)
minstdat=apply(stda[-1,],2,min)
minanot=apply(ano[-1,],2,min)
dim(datt)
fb=fbplot(stdat,method = "MBD")
fb=fbplot(anot,method = "MBD")

fb=fbplot(datt,method = "MBD")

hist(fb$depth)
fb$medcurve
# Crea un dataframe con tus datos
df <- data.frame(
  x = 1:length(maxstdat),
  maxstdat = maxstdat,
  maxanot = maxanot,
  minstdat = minstdat,
  minanot = minanot
)

# Convierte el dataframe a un formato largo
df_long <- pivot_longer(df, cols = -x, names_to = "Variable", values_to = "Valor")

# Crea el gráfico de línea
p <- ggplot(df_long, aes(x = x, y = Valor, color = Variable)) +
  geom_line() +
  labs(x = "Mes", y = "Valor", color = "Variable") +
  scale_color_manual(values = c("maxstdat" = "blue", "maxanot" = "red", "minstdat" = "green", "minanot" = "gray")) +
  theme_minimal()

# Muestra el gráfico
print(p)


data_long <- gather(stda, Month, Temperature,-YEAR)
data_longa <- gather(ano, Month, Temperature,-YEAR)

# Crear el gráfico de líneas
ggplot(data_long, aes(x = YEAR, y = Temperature, group = Month, color = Month)) +
  geom_line() +
  labs(x = "Año", y = "Temperatura", title = "Temperaturas Mensuales (1951-2022)") +
  scale_color_discrete(name = "Mes") +
  theme_minimal()

ggplot(data_longa, aes(x = YEAR, y = Temperature, group = Month, color = Month)) +
  geom_line() +
  labs(x = "Año", y = "Temperatura", title = "Temperaturas Mensuales (1951-2022)") +
  scale_color_discrete(name = "Mes") +
  theme_minimal()


# Calcular la frecuencia de cada valor en el dataframe
value_counts <- as.data.frame(table(data_long$Temperature))
value_countsa <- as.data.frame(table(data_longa$Temperature))

# Ordenar los valores por frecuencia de mayor a menor
sorted_counts <- value_counts[order(-value_counts$Freq), ]
sorted_countsa <- value_countsa[order(-value_countsa$Freq), ]

# Primer histograma
histogram1 <- ggplot(data_long, aes(x = Temperature)) +
  geom_histogram(stat = "count", fill = "blue", color = "black", binwidth = 2) +
  labs(x = "Temperatura", y = "Frecuencia", title = "Histograma de Temperaturas Mensuales estadanraizadas (1951-2022)") +
  theme_minimal()

# Segundo histograma
histogram2 <- ggplot(data_longa, aes(x = Temperature)) +
  geom_histogram(stat = "count", fill = "red", color = "black", binwidth = 10) +
  labs(x = "Temperatura", y = "Frecuencia", title = "Histograma de Temperaturas Mensuales anomalias (1951-2022)") +
  theme_minimal()

# Combinar los histogramas en una sola imagen
combined_plot <- grid.arrange(histogram1, histogram2, ncol = 2)

# Mostrar la imagen combinada
print(combined_plot)

for(i in 2:72) {
  lines(stdat[,i],type="l",col="red")
  lines(anot[,i],type="l",col="blue")
}

