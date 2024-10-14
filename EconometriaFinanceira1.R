library(readr)
library(dplyr)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(lubridate)
library(naniar)
library(zoo)
#install.packages("zoo")
#library(plyr)

Base <- read.csv("C:/Users/vinih/Desktop/FGV/Econo Fin/EconoFinBaseProb2.csv")
PL <- read.csv("C:/Users/vinih/Desktop/FGV/Econo Fin/Economatica-PL.csv")

du <- Base %>%
  filter(Ativo == "PETR4<XBSP>") %>%
  filter(Fechamento.ajust.p..prov.Em.moeda.orig != "-") %>%
  mutate(ano = year(Data)) %>%
  mutate(mes = month(Data)) %>%
  mutate(dia = day(Data)) %>%
  select(ano,mes,dia)

port3 <- Base %>%
  mutate(ano = year(Data)) %>%
  mutate(mes = month(Data)) %>%
  mutate(dia = day(Data)) %>%
  inner_join(du) %>%
  arrange(Ativo, ano, mes, dia) %>%
  transform(Close = as.numeric(Fechamento.ajust.p..prov.Em.moeda.orig)) %>%
  transform(MktCap = as.numeric(Valor.Mercado.da.empresa.Em.moeda.orig.em.milhares)) %>%
  select(Ativo, ano, mes, dia, Close, MktCap)

port3 <- subset(port3, !is.na(MktCap))

iliquid <- subset(port3, is.na(Close))

illiquidIDs <- unique(iliquid$Ativo)

PL <- PL %>%
  mutate(ano = year(Data)) %>%
  mutate(mes = month(Data)) %>%
  mutate(dia = day(Data)) %>%
  mutate(PatrimLiq = as.numeric(Patrim.Liq..Em.moeda.orig..em.milhares..consolid.sim.)) %>%
  group_by(Ativo) %>%
  fill(PatrimLiq, .direction = "down")


port3 <- port3 %>%
  filter(!(Ativo %in% illiquidIDs)) %>%
  mutate(Returns = (Close - lag(Close))/lag(Close)) %>%
  na.omit(Returns) %>%
  inner_join(PL)

port3 <- port3 %>%
  select(Ativo, ano, mes, dia, Returns, MktCap, PatrimLiq) %>%
  mutate(BM = PatrimLiq/MktCap) %>%
  group_by(ano, mes) %>%
  mutate(mes = mes + 1) %>%
  mutate(ano = replace(ano, mes == 13, ano + 1)) %>%
  mutate(mes = replace(mes, mes ==  13, 1)) %>%
  mutate(rank = ceiling(rank(BM) * 4 / n())) %>%
  select(Ativo, ano, mes, dia, rank, MktCap, Returns, BM) %>%
  ungroup() %>%
  arrange(ano, mes, dia, rank)

port3 <- port3 %>%  
  group_by(ano, mes, dia, rank) %>%
  mutate(weight = MktCap / sum(MktCap))


port3_final <- port3 %>%
  group_by(ano, mes, dia, rank) %>%
  summarise(ret_medio = sum(Returns*weight)) %>%
  spread(key = rank, value = ret_medio, sep = "") %>%
  mutate(long_short = rank1-rank4)

mean(port3_final$long_short)
