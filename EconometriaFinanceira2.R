library(readr)
library(dplyr)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(lubridate)
library(naniar)
library(zoo)
library(RcppRoll)
install.packages("naniar")

Base <- read.csv("C:/Users/vinih/Desktop/FGV/Econo Fin/EconoFinBaseProb2.csv")

du <- Base %>%
  filter(Ativo == "PETR4<XBSP>") %>%
  filter(Fechamento.ajust.p..prov.Em.moeda.orig != "-") %>%
  mutate(ano = year(Data)) %>%
  mutate(mes = month(Data)) %>%
  mutate(dia = day(Data)) %>%
  select(ano,mes,dia)

port4_base <- Base %>%
  mutate(ano = year(Data)) %>%
  mutate(mes = month(Data)) %>%
  mutate(dia = day(Data)) %>%
  inner_join(du) %>%
  arrange(Ativo, ano, mes, dia) %>%
  transform(Close = as.numeric(Fechamento.ajust.p..prov.Em.moeda.orig)) %>%
  transform(MktCap = as.numeric(Valor.Mercado.da.empresa.Em.moeda.orig.em.milhares)) %>%
  select(Ativo, ano, mes, dia, Close, MktCap)

port4 <- subset(port4_base, !is.na(MktCap))

iliquid <- subset(port4, is.na(Close))

illiquidIDs <- unique(iliquid$Ativo)

port4_lags <- port4 %>%
  filter(!(Ativo %in% illiquidIDs)) %>%
  mutate(Returns = (Close - lag(Close))/lag(Close)) %>%
  na.omit(Returns)

port4 <- port4_lags %>%
  mutate(Returns = Returns + 1) %>%
  group_by(Ativo, ano, mes) %>%
  summarise(MonthReturns = prod(Returns) - 1) %>%
  arrange(Ativo, ano, mes)

port4 <- port4 %>%
  mutate(MonthReturns = MonthReturns + 1) %>%
  ungroup() %>%
  group_by(Ativo) %>%
  mutate(Mom = 100 * ((roll_prod(MonthReturns,12, align = "right", fill = NA)
                       - MonthReturns) - 1)) %>%
  na.omit(Mom)

port4 <- port4 %>%
  ungroup() %>%
  merge(port4_lags[,c("Ativo", "ano", "mes", "dia","MktCap", "Returns")], by = c("Ativo", "ano", "mes"), all.x = TRUE) %>%
  select(Ativo, ano, mes, dia, MktCap, Mom, Returns) %>%
  arrange(Ativo, ano, mes, dia) %>%
  group_by(ano, mes) %>%
  mutate(mes = mes + 1) %>%
  mutate(ano = replace(ano, mes == 13, ano + 1)) %>%
  mutate(mes = replace(mes, mes ==  13, 1)) %>%
  mutate(rank = ceiling(rank(Mom) * 4 / n())) %>%
  ungroup() %>%
  group_by(ano, mes, dia, rank) %>%
  arrange(ano, mes, dia, rank) %>%
  mutate(weight = MktCap / sum(MktCap))


port4_final <- port4 %>%
  summarise(ret_medio = sum(Returns*weight)) %>%
  spread(key = rank, value = ret_medio, sep = "") %>%
  mutate(long_short = rank4-rank1)


