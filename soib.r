library(fda)
library(ggplot2)
library(plotly)
library(tidyr)
library(gridExtra)
library(tidyverse)
library(plotly) 
library(htmlwidgets)

ymsoi = read.csv('/home/yashar/Downloads/datoSOI.csv')
ymsoi$Month <- factor(ymsoi$Month, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
msoi=subset(ymsoi, select=c(Year,Month,SOI))

msoir <- msoi %>% 
  pivot_wider(names_from = Year, values_from = SOI)

# Asegúrate de que los nombres de las columnas sean apropiados
colnames(msoir) <- c("Month", as.character(unique(msoi$Year)))
mysoir=msoir[,-1]
rownames(mysoir)=msoir$Month
mysoir=subset(mysoir,select=-145)
mysoir=as.data.frame(mysoir)
fb=fbplot(mysoir,method = "MBD")
fb$depth
msoirt=t(mysoir)
colnames(msoirt)=msoir$Month

#data_long <- gather(msoi, Month, SOI,-Year)

msoi$Month <- paste(msoi$Year, msoi$Month, sep = "-")
write.csv(msoi, "moiIA.csv", row.names=FALSE)
msoi$Color <- ifelse(msoi$SOI > 7, "Above 7", ifelse(msoi$SOI < -7, "Below -7", "Between -7 and 7"))

# Crear el gráfico de líneas
g<-ggplot(msoi, aes(x = Month, y = SOI, fill = Color)) +
  geom_bar(stat = "identity") +
  labs(x = "Year-Month", y = "SOI", title = "Behavior of SOI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = c(7, -7), color = "purple", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "green", linetype = "dashed") +
  scale_x_discrete(breaks = msoi$Month[c(seq(1, nrow(msoi), by = 30))] ) +
  scale_fill_manual(values = c("Above 7" = "red", "Between -7 and 7" = "gray", "Below -7" = "blue"))

interactive_plot<-ggplotly(g)
# Guardar el gráfico interactivo en un archivo HTML
saveWidget(interactive_plot, "/home/yashar/Documents/Trabajo/InDataAnalitycs/SOI_i.html", selfcontained = TRUE)

# También puedes exportar a otros formatos, como una imagen PNG, utilizando orca
#orca(interactive_plot, "SOI_i.png")

dat2000=subset(msoi,Year>=2000 & Year<=2019)
#dat2000$Month <- paste(dat2000$Year, dat2000$Month, sep = "-")
# Crear un gráfico de línea con SOI como una línea continua
dat2000$Color <- ifelse(dat2000$SOI > 7, "Above 7", ifelse(dat2000$SOI < -7, "Below -7", "Between -7 and 7"))

# Crear el gráfico de líneas
p<-ggplot(dat2000, aes(x = Month, y = SOI, fill = Color)) +
  geom_bar(stat = "identity") +
  labs(x = "Year-Month", y = "SOI", title = "Behavior of SOI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = c(7, -7), color = "purple", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "green", linetype = "dashed") +
  scale_x_discrete(breaks = dat2000$Month[c(seq(1, nrow(dat2000), by = 10))] ) +
  scale_fill_manual(values = c("Above 7" = "red", "Between -7 and 7" = "gray", "Below -7" = "blue"))

# Crear un gráfico de barras por mes-año
interactive_plot<-ggplotly(p)
# Guardar el gráfico interactivo en un archivo HTML
saveWidget(interactive_plot, "/home/yashar/Documents/Trabajo/InDataAnalitycs/SOI_2000.html", selfcontained = TRUE)

