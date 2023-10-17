library(fda)
library(ggplot2)
library(plotly)
library(tidyr)
library(gridExtra)
library(tidyverse)
library(plotly) 
library(htmlwidgets)

nnnsst = read.csv('/home/yashar/Downloads/sst.csv')
sstn3.4=subset(nnnsst, select=c(YEAR,MON,NINO3.4))
ssta3.4=subset(nnnsst, select=c(YEAR,MON,ANOM3.4))
#--------------------------------------------------------------------
#Temperatures
sstn3.4$MON <- factor(sstn3.4$MON, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
sstn3.4r <- sstn3.4 %>% 
  pivot_wider(names_from = YEAR, values_from = NINO3.4)
cop=sstn3.4r
#colnames(sstn4r) <- c("Month", as.character(unique(sstn4r$Month)))
sstn3.4r=sstn3.4r[,-1]
sstn3.4r=sstn3.4r[,-1]
sstn3.4r=sstn3.4r[,-41]
rownames(sstn3.4r)=cop$MON
#mysoir=subset(mysoir,select=-145)
sstn3.4r=as.data.frame(sstn3.4r)
fb=fbplot(sstn3.4r,method = "MBD")
fb$depth
sstn3.4$MON <- paste(sstn3.4$YEAR, sstn3.4$MON, sep = "-")
#write.csv(msoi, "moiIA.csv", row.names=FALSE)
sstn3.4$Color <- ifelse(sstn3.4$NINO3.4> 28, "Above 28C","Below 28")

# Crear el gráfico de líneas
gn<-ggplot(sstn3.4, aes(x = MON, y = NINO3.4,fill=Color)) +
  geom_bar(stat = "identity") +
  labs(x = "Year-Month", y = "Temperature", title = "Temperature in El nino4 zone") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = 28, color = "purple", linetype = "dashed")  +
  scale_x_discrete(breaks = sstn3.4$MON[c(seq(1, nrow(sstn3.4), by = 30))] ) +
  scale_fill_manual(values = c("Above 28C" = "red"))

ipn<-ggplotly(gn)
saveWidget(ipn, "/home/yashar/Documents/Trabajo/InDataAnalitycs/SST_n.html", selfcontained = TRUE)
#-------------------------------------------------------------------------
#Anomalys
ssta3.4$MON <- factor(ssta3.4$MON, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
ssta3.4r <- ssta3.4 %>% 
  pivot_wider(names_from = YEAR, values_from = ANOM3.4)
cop=ssta3.4r
#colnames(sstn4r) <- c("Month", as.character(unique(sstn4r$Month)))
ssta3.4r=ssta3.4r[,-1]
ssta3.4r=ssta3.4r[,-1]
ssta3.4r=ssta3.4r[,-41]
rownames(ssta3.4r)=cop$MON
#mysoir=subset(mysoir,select=-145)
ssta3.4r=as.data.frame(ssta3.4r)
fb=fbplot(ssta3.4r,method = "MBD")
fb
ssta3.4$MON <- paste(ssta3.4$YEAR, ssta3.4$MON, sep = "-")
#write.csv(msoi, "moiIA.csv", row.names=FALSE)
ssta3.4$Color <- ifelse(ssta3.4$ANOM3.4 > 0.5, "Above 0.5C", ifelse(ssta3.4$ANOM3.4 < -0.5, "Below -0.5C", "Between -0.5C and 0.5C"))

# Crear el gráfico de líneas
ag<-ggplot(ssta3.4, aes(x = MON, y = ANOM3.4,fill=Color)) +
  geom_bar(stat = "identity") +
  labs(x = "Year-Month", y = "Temperature", title = "Temperature in El nino4 zone") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = c(-0.5,0.5), color = "purple", linetype = "dashed")  +
  scale_x_discrete(breaks = ssta3.4$MON[c(seq(1, nrow(ssta3.4), by = 30))] ) +
  scale_fill_manual(values = c("Above 0.5C" = "red", "Between -0.5C and 0.5C" = "gray", "Below -0.5C" = "blue"))

ipa<-ggplotly(ag)
saveWidget(ipa, "/home/yashar/Documents/Trabajo/InDataAnalitycs/SST_a.html", selfcontained = TRUE)

