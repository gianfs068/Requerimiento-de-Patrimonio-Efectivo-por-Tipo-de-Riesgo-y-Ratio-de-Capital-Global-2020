
#title: "Requerimiento de Patrimonio Efectivo por Tipo de Riesgo y Ratio de Capital Global 2020"
#author: "Gian Franco Jacinto Quispe"

# Scrip de Limpieza


library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)


setwd("C:/Users/USER/Desktop/Tarea 3/RPE y RCG")

bd_Ene_2020 = readxl::read_xls("C-1252-en2020.XLS",skip = 6)
bd_Ene_2020 = na.omit(bd_Ene_2020)
bd_Ene_2020 = bd_Ene_2020[-13:-14,]
names(bd_Ene_2020) = c("ENTIDAD","RPE_RIESGO_CRÉDITO","RPE_RIESGO_MERCADO","RPE_RIESGO_OPERACIONAL","TOTAL_REQUERIMIENTO","PATRIMONIO EFECTIVO","RATIO_DE_CAPITAL_GLOBAL")
bd_Ene_2020[,-1] = sapply(bd_Ene_2020[,-1],function(x) round(as.numeric(as.character(x)),2))
bd_Ene_2020$FECHA = "2020-01-31"          


bd_Feb_2020 = readxl::read_xls("C-1252-fe2020.XLS",skip = 6)
bd_Feb_2020 = na.omit(bd_Feb_2020)
bd_Feb_2020 = bd_Feb_2020[-13:-14,]
names(bd_Feb_2020) = c("ENTIDAD","RPE_RIESGO_CRÉDITO","RPE_RIESGO_MERCADO","RPE_RIESGO_OPERACIONAL","TOTAL_REQUERIMIENTO","PATRIMONIO EFECTIVO","RATIO_DE_CAPITAL_GLOBAL")
bd_Feb_2020[,-1] = sapply(bd_Feb_2020[,-1],function(x) round(as.numeric(as.character(x)),2))
bd_Feb_2020$FECHA = "2020-02-29"          


bd_Mar_2020 = readxl::read_xls("C-1252-ma2020.XLS",skip = 6)
bd_Mar_2020 = na.omit(bd_Mar_2020)
bd_Mar_2020 = bd_Mar_2020[-13:-14,]
names(bd_Mar_2020) = c("ENTIDAD","RPE_RIESGO_CRÉDITO","RPE_RIESGO_MERCADO","RPE_RIESGO_OPERACIONAL","TOTAL_REQUERIMIENTO","PATRIMONIO EFECTIVO","RATIO_DE_CAPITAL_GLOBAL")
bd_Mar_2020[,-1] = sapply(bd_Mar_2020[,-1],function(x) round(as.numeric(as.character(x)),2))
bd_Mar_2020$FECHA = "2020-03-31"          


bd_Abr_2020 = readxl::read_xls("C-1252-ab2020.XLS",skip = 6)
bd_Abr_2020 = na.omit(bd_Abr_2020)
bd_Abr_2020 = bd_Abr_2020[-13:-14,]
names(bd_Abr_2020) = c("ENTIDAD","RPE_RIESGO_CRÉDITO","RPE_RIESGO_MERCADO","RPE_RIESGO_OPERACIONAL","TOTAL_REQUERIMIENTO","PATRIMONIO EFECTIVO","RATIO_DE_CAPITAL_GLOBAL")
bd_Abr_2020[,-1] = sapply(bd_Abr_2020[,-1],function(x) round(as.numeric(as.character(x)),2))
bd_Abr_2020$FECHA = "2020-04-30"          


bd_May_2020 = readxl::read_xls("C-1252-my2020.XLS",skip = 6)
bd_May_2020 = na.omit(bd_May_2020)
bd_May_2020 = bd_May_2020[-13:-14,]
names(bd_May_2020) = c("ENTIDAD","RPE_RIESGO_CRÉDITO","RPE_RIESGO_MERCADO","RPE_RIESGO_OPERACIONAL","TOTAL_REQUERIMIENTO","PATRIMONIO EFECTIVO","RATIO_DE_CAPITAL_GLOBAL")
bd_May_2020[,-1] = sapply(bd_May_2020[,-1],function(x) round(as.numeric(as.character(x)),2))
bd_May_2020$FECHA = "2020-05-31"          


bd_Jun_2020 = readxl::read_xls("C-1252-jn2020.XLS",skip = 6)
bd_Jun_2020 = na.omit(bd_Jun_2020)
bd_Jun_2020 = bd_Jun_2020[-13:-14,]
names(bd_Jun_2020) = c("ENTIDAD","RPE_RIESGO_CRÉDITO","RPE_RIESGO_MERCADO","RPE_RIESGO_OPERACIONAL","TOTAL_REQUERIMIENTO","PATRIMONIO EFECTIVO","RATIO_DE_CAPITAL_GLOBAL")
bd_Jun_2020[,-1] = sapply(bd_Jun_2020[,-1],function(x) round(as.numeric(as.character(x)),2))
bd_Jun_2020$FECHA = "2020-06-30"          


bd_Jul_2020 = readxl::read_xls("C-1252-jl2020.XLS",skip = 6)
bd_Jul_2020 = na.omit(bd_Jul_2020)
bd_Jul_2020 = bd_Jul_2020[-13:-14,]
names(bd_Jul_2020) = c("ENTIDAD","RPE_RIESGO_CRÉDITO","RPE_RIESGO_MERCADO","RPE_RIESGO_OPERACIONAL","TOTAL_REQUERIMIENTO","PATRIMONIO EFECTIVO","RATIO_DE_CAPITAL_GLOBAL")
bd_Jul_2020[,-1] = sapply(bd_Jul_2020[,-1],function(x) round(as.numeric(as.character(x)),2))
bd_Jul_2020$FECHA = "2020-07-31"          


bd_Ago_2020 = readxl::read_xls("C-1252-ag2020.XLS",skip = 6)
bd_Ago_2020 = na.omit(bd_Ago_2020)
bd_Ago_2020 = bd_Ago_2020[-13:-14,]
names(bd_Ago_2020) = c("ENTIDAD","RPE_RIESGO_CRÉDITO","RPE_RIESGO_MERCADO","RPE_RIESGO_OPERACIONAL","TOTAL_REQUERIMIENTO","PATRIMONIO EFECTIVO","RATIO_DE_CAPITAL_GLOBAL")
bd_Ago_2020[,-1] = sapply(bd_Ago_2020[,-1],function(x) round(as.numeric(as.character(x)),2))
bd_Ago_2020$FECHA = "2020-08-31"          


RCG <- rbind(bd_Ene_2020,bd_Feb_2020)
RCG <- rbind(RCG,bd_Mar_2020)
RCG <- rbind(RCG,bd_Abr_2020)
RCG <- rbind(RCG,bd_May_2020)
RCG <- rbind(RCG,bd_Jun_2020)
RCG <- rbind(RCG,bd_Jul_2020)
RCG <- rbind(RCG,bd_Ago_2020)

View(RCG)

table(RCG$ENTIDAD)

unique(RCG$ENTIDAD)

RCG$FECHA=as.Date(RCG$FECHA)



ggplot(RCG, aes(x = FECHA, y = RATIO_DE_CAPITAL_GLOBAL)) + 
  geom_line(aes(color = ENTIDAD), size = 1) +
  theme_minimal()


