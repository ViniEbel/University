library(readr)
library(tidyverse)
library(stringi)
library(lubridate)
library(ggplot2)
library(plm)
library(stargazer)
library(dplyr)
library(lmtest)
library(mgcv)

#RUMO LOG ON 

DadosAcoes = data.frame(read.csv("/Users/vinih/Desktop/FGV/StockPriceBOVESPA2001.csv")) 
DataICO2 <- read.csv2("~/FGV/DataICO2.csv")
NefinRF <- data.frame(read.csv("/Users/vinih/Desktop/FGV/nefin_factors (2).csv")) %>%
  mutate(Data = as.Date(Date))

##
str_sub(DataICO2$Data, 7, 6) <- "20"
DataICO2$Data = format(as.Date(DataICO2$Data, format = "%d/%m/%Y"), "%Y-%m-%d")
DadosAcoes$Ativo = substr(DadosAcoes$Ativo, 1, nchar(DadosAcoes$Ativo) - 6)
##

du <- DadosAcoes %>%
  filter(Ativo == "PETR4") %>%
  filter(Fechamento.ajust.p..prov.Em.moeda.orig != "-") %>%
  mutate(ano = year(Data)) %>%
  mutate(mes = month(Data)) %>%
  mutate(dia = day(Data)) %>%
  select(ano,mes,dia)


DadosAcoesClean1 <- DadosAcoes %>%
  mutate(Close = as.numeric(Fechamento.ajust.p..prov.Em.moeda.orig),
         Data = as.Date(Data)) %>%
  mutate(ano = year(Data)) %>%
  mutate(mes = month(Data)) %>%
  mutate(dia = day(Data)) %>%
  inner_join(du)

DadosAcoesClean2 <- DadosAcoesClean1 %>%
  rename(Ticker = Ativo) %>%
  group_by(Ticker) %>%
  arrange(Ticker, ano, mes, dia) %>%
  mutate(Retorno = (Close - dplyr::lag(Close))/dplyr::lag(Close)) %>%
  na.omit(Retorno) %>%
  select(Ticker, Data, ano, mes, dia, Retorno)

DataICO2Clean <- DataICO2 %>%
  mutate(Data = as.Date(Data)) %>%
  mutate(Ticker = gsub(" ","",Ticker)) %>%
  mutate(ano = year(Data)) %>%
  mutate(mes = month(Data)) %>%
  mutate(dia = day(Data)) %>% 
  rename(CompIndice = X..PL) %>%
  select(Ativo, Ticker, Data, ano, mes, dia, CompIndice) %>%
  filter(Ativo != "Rumo Log ON")

#####

TotalData <- inner_join(DadosAcoesClean2, DataICO2Clean, join_by(Ticker == Ticker,
                                                         ano == ano, 
                                                         mes == mes)) %>%
  rename(Data = Data.x) %>%
  select(Ativo, Ticker, Data, ano, mes,Retorno, CompIndice) %>%
  mutate(HelperRet = Retorno + 1) %>%
  group_by(Ticker, ano, mes) %>%
  arrange(Ticker, ano, mes) %>%
  mutate(RetMensal = prod(HelperRet) - 1) %>%
  ungroup() %>%
  select(Ativo, Ticker, Data, ano, mes, Retorno, RetMensal, CompIndice)

Rebalanceamento <- data.frame(unique(as.Date(DataICO2$Data))) %>%
  rename(Data = unique.as.Date.DataICO2.Data..)

TotalDataRFDIARIO <- inner_join(TotalData, NefinRF) %>%
  select(Ativo, Ticker, Data, Retorno, CompIndice, RM, Risk_Free, 
         Rm_minus_Rf, SMB, HML, WML, IML) %>%
  group_by(Ticker, Data) %>%
  distinct()

TotalDataRFMENSAL <- inner_join(TotalData, NefinRF) %>%
  select(Ativo, Ticker, Data, ano, mes, RetMensal, CompIndice, RM, Risk_Free, 
         Rm_minus_Rf, SMB, HML, WML) %>%
  mutate(HelperRM = RM + 1,
         HelperRf = Risk_Free + 1,
         HelperRm_minus_Rf = Rm_minus_Rf + 1,
         HelperSMB = SMB + 1,
         HelperHML = HML + 1,
         HelperWML = WML + 1,
         ) %>%
  group_by(ano, mes) %>%
  arrange(Ticker,ano, mes) %>%
  mutate(RMMensal = prod(HelperRM) - 1,
         RfMensal = prod(HelperRf) - 1,
         RpMensal = prod(HelperRm_minus_Rf) - 1,
         SMBMensal = prod(HelperSMB) - 1,
         HMLMensal = prod(HelperHML) - 1,
         WMLMensal = prod(HelperWML) - 1,
         RetMensal = RetMensal * 100
         ) %>%
  inner_join(Rebalanceamento) %>%
  select(Ativo, Ticker, Data, RetMensal, CompIndice, RMMensal,
         RfMensal, RpMensal, SMBMensal, HMLMensal, WMLMensal) %>%
  group_by(Ticker, Data) %>%
  distinct()

Dummyizer <- function(vec){
  for(i in 1:length(vec)){
    if (vec[i] > 0){
      vec[i] = 1
    }
  }
  return(vec)
}

TotalDataRFMENSAL_Dummy <- TotalDataRFMENSAL %>%
  mutate(DummyComp = Dummyizer(CompIndice))

####



regPOLS_Mensal <- plm(RetMensal ~ DummyComp + RMMensal + RfMensal
                    + SMBMensal + HMLMensal + WMLMensal,
                    data = TotalDataRFMENSAL_Dummy,
                    index = c("Ticker", "Data"), model = "pooling",
                    na.rm = T)
stargazer(regPOLS_Mensal, type = "text")

regFE_Mensal <- plm(RetMensal ~ DummyComp + RMMensal + RfMensal
                    + SMBMensal + HMLMensal + WMLMensal,
                    data = TotalDataRFMENSAL_Dummy,
                    index = c("Ticker", "Data"), model = "within",
                    na.rm = T)
stargazer(regFE_Mensal)

regFD_Mensal <- plm(RetMensal ~ DummyComp + RMMensal + RfMensal
                    + SMBMensal + HMLMensal + WMLMensal,
                    data = TotalDataRFMENSAL_Dummy,
                    index = c("Ticker", "Data"), model = "fd")
summary(regFD_Mensal)
stargazer(regFD_Mensal, type = "text")


ACF1 <- acf(resid(regPOLS_Mensal), main="ACF dos resíduos antes da diferença da média")
ACF2 <- acf(resid(regFD_Mensal), main="ACF dos resíduos do modelo em primeira diferença")

robust<-coeftest(regFE_Mensal, vcov. = vcovHC(regFE_Mensal, method = "arellano",
                                      type = "HC1"))
stargazer(robust)

stargazer(EstatisticasDescritivasRaw[1,], summary = F)

stargazer(regFE_Mensal, robust)

EstatisticasDescritivasTS1 <- TotalDataRFMENSAL %>%
  group_by(Data) %>%
  filter(CompIndice != 0) %>%
  summarise(Numero_de_ativos_no_indice = n_distinct(Ativo))
CompositionPlot <- EstatisticasDescritivasTS %>%
  ggplot(aes(x=Data, y=Numero_de_ativos_no_indice, color = "red")) + 
  geom_line( show.legend = FALSE)
CompositionPlot

EstatisticasDescritivasTS2 <- TotalDataRFDIARIO %>%
  group_by(Data) %>%
  filter(CompIndice != 0) %>%
  summarise(RetornoICO2 = mean(Retorno, ra.rm = T)) %>%
  inner_join(TotalDataRFDIARIO[c("Data", "RM")]) %>%
  distinct()

cor(EstatisticasDescritivasTS2$RetornoICO2, 
    EstatisticasDescritivasTS2$RM)

ReturnPlot <- EstatisticasDescritivasTS2 %>%
  ggplot(aes(Data)) + 
  geom_line(aes(y=RetornoICO2, color = "blue"),show.legend = FALSE ) + 
  geom_line(aes(y=RM, color = "red"), show.legend = FALSE) + 
  ggtitle("Retornos diários do ICO2 e Carteira de mercado (ρ = 0,96)") +
  xlab("Data") + ylab("Retorno diário (%)")
ReturnPlot

EstatisticasDescritivasRaw <- TotalDataRFDIARIO %>%
  ungroup() %>%
  mutate(RetDiarioMedio = mean(Retorno)) %>%
  mutate(CompIndiceMedio = mean(CompIndice)) %>%
  mutate(RmMedio = mean(RM)) %>%
  mutate(RfMedio = mean(Risk_Free)) %>%
  mutate(SMBMedio = mean(SMB)) %>%
  mutate(HMLMedio = mean(HML)) %>%
  mutate(WMLMedio = mean(WML)) %>%
  mutate(RetDiarioSD = sd(Retorno)) %>%
  mutate(CompIndiceSD = sd(CompIndice)) %>%
  mutate(RmSD = sd(RM)) %>%
  mutate(RfSD = sd(Risk_Free)) %>%
  mutate(SMBSD = sd(SMB)) %>%
  mutate(HMLSD = sd(HML)) %>%
  mutate(WMLSD = sd(WML)) %>%
  mutate(RetDiarioMin = min(Retorno)) %>%
  mutate(CompIndiceMin = min(CompIndice)) %>%
  mutate(RmMin = min(RM)) %>%
  mutate(RfMin = min(Risk_Free)) %>%
  mutate(SMBMin = min(SMB)) %>%
  mutate(HMLMin = min(HML)) %>%
  mutate(WMLMin = min(WML)) %>%
  mutate(RetDiarioMax = max(Retorno)) %>%
  mutate(CompIndiceMax = max(CompIndice)) %>%
  mutate(RmMax = max(RM)) %>%
  mutate(RfMax = max(Risk_Free)) %>%
  mutate(SMBMax = max(SMB)) %>%
  mutate(HMLMax = max(HML)) %>%
  mutate(WMLMax = max(WML)) %>%
  mutate(RetDiarioMedian = median(Retorno)) %>%
  mutate(CompIndiceMedian = median(CompIndice)) %>%
  mutate(RmMedian = median(RM)) %>%
  mutate(RfMedian = median(Risk_Free)) %>%
  mutate(SMBMedian = median(SMB)) %>%
  mutate(HMLMedian = median(HML)) %>%
  mutate(WMLMedian = median(WML)) %>%
  select(RetDiarioMedio, CompIndiceMedio, RmMedio,
         RfMedio, SMBMedio, HMLMedio, WMLMedio,"RetDiarioSD",      "CompIndiceSD",    
         "RmSD"  ,           "RfSD"  ,           "SMBSD"  ,         
         "HMLSD"      ,      "WMLSD"   ,         "RetDiarioMin" ,   
       "CompIndiceMin"    ,"RmMin"       ,     "RfMin"   ,        
          "SMBMin"      ,     "HMLMin"    ,       "WMLMin",          
       "RetDiarioMax"  ,   "CompIndiceMax" ,   "RmMax"   ,        
         "RfMax"         ,   "SMBMax"    ,       "HMLMax"    ,      
        "WMLMax"         ,  "RetDiarioMedian" , "CompIndiceMedian",
          "RmMedian"   ,      "RfMedian"  ,       "SMBMedian",       
          "HMLMedian"      ,  "WMLMedian"       )

STATDESC<-EstatisticasDescritivasRaw[1,]




stargazer(TotalDataRFDIARIO)
  
  






