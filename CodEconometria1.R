install.packages("AER","stargazer")
install.packages("geobr")
install.packages("rio")
install.packages("readr")
install.packages("summarytools")
install.packages("data.table")
install.packages("grid")
install.packages("gridExtra")
library(data.table)
library(geobr)
library(summarytools)
library(sf)
library(ggplot2)
library(AER)
library(stargazer)
library(dplyr)
library(rio)
library(readr)
library(readxl)
library(gridExtra)
library(grid)

Base <- read_excel("C:/Users/vinih/Desktop/FGV/Econo1/Bases Trabalho econo/Base/Base trabalho econo (20).xlsx")

#Limpando a Base
#Base1 é a base com corte de dados NA em IDEB
Base1 <- Base[complete.cases(Base$IDEB), ]
#Base2 é a base com cortes de dados NA em Invest
Base2 <- Base1[complete.cases(Base1$`(Invest/PIB)*100`), ]
#Base2 é a base com cortes de dados NA em Invest
Base21 <- Base2[complete.cases(Base2$`Emprego & Renda`), ]
Base3 <- Base21[complete.cases(Base21$Bolsa), ]


#Nomeando as variaveis para a Base3
IDEB <- Base3$IDEB
Rural <- Base3$`Rural0/Urbano1`
Prof <- Base3$`Prof/Aluno`
PC <- Base3$`PCs/Aluno`
Infra <- Base3$`Algum problema de saneamento/infraestrutura`
Homic <- Base3$Homicídio
Net <- Base3$`Escolas com Internet/Escolas(0 a 1)`
Renda <- Base3$`Emprego & Renda`
Saude <- Base3$Saúde
Bolsa <- Base3$Bolsa
Invest <- Base3$Invest
Ia <- Invest/Base3$Alunos
  
#Tabela Descritiva Organizada, para LaTex:
dt <- data.table()
dt$IDEB <- Base3$IDEB
dt$Rural <- Base3$`Rural0/Urbano1`
dt$Prof <- Base3$`Prof/Aluno`
dt$PC <- Base3$`PCs/Aluno`
dt$Infra <- Base3$`Algum problema de saneamento/infraestrutura`
dt$Homic <- Base3$Homicídio
dt$Net <- Base3$`Escolas com Internet/Escolas(0 a 1)`
dt$Renda <- Base3$`Emprego & Renda`
dt$Saude <- Base3$Saúde
dt$Bolsa <- Base3$Bolsa
dt$Ia <- Invest/Base3$Alunos
dt$alunos <- Base3$Alunos

stargazer(dt,  type="latex", align = TRUE,digits=3)


#Para os gráficos dos Municipios
Invest1 <- Base$`(Invest/PIB)*100`
Rural1 <- Base$`Rural0/Urbano1`

#Regressão Simples de IDEB com Ia:
Reg = lm(IDEB ~ Ia, data = Base3)
plot(Ip, IDEB, ylim=c(min(Base3$IDEB), max(Base3$IDEB)))
abline(Reg, col="blue")

eq <- paste0("y =", round(coef(Reg)[1],2), " + ", round(coef(Reg)[2],2), "x")
mtext(eq,3,line=-2)
summary(Reg)

bptest(Reg)
coeftest(Reg, vcov = vcovHC(Reg, "HC1"))
stargazer(Reg,title = "Tabela de Regressão Simples", align = TRUE)

#Regressão múltipla:
AReg <-  lm(IDEB ~ Ia + Infra + Homic + Net + PC + Prof
            + Rural + Renda + Saude + Bolsa, data = Base3)
summary(AReg)

bptest(AReg)
coeftest(AReg, vcov = vcovHC(AReg, "HC1"))

stargazer(AReg,title = "Tabela de Regressão Multipla", align = TRUE)


#Regressão múltipla com iteração Ia:Saude:
AiReg <-  lm(IDEB ~ Ia + Infra + Homic + Net + PC + Prof
              + Rural + Renda + Saude + Bolsa + Ia*Saude, data = Base3)
summary(AiReg)

bptest(AiReg)
stargazer(AiReg, align = TRUE)

#Correlação das variaveis
Cor <- cor(Base3[, c("IDEB","(Invest/PIB)*100","Rural0/Urbano1","Homicídio",
              "Algum problema de saneamento/infraestrutura","PCs","Internet",
              "Profs","Emprego & Renda","Saúde","Bolsa")])
Cor 



#Histograma Ia
ggplot(Base3, aes(x=Ia))+
  geom_density(alpha = 0.8, colour = "black", fill ="red3")

hh <- qplot(x = Ia,fill=..count.., data = Base3, geom = "histogram")
hh+scale_fill_gradient(low="blue", high="red")

#Histograma IDEB
ggplot(Base3, aes(x=IDEB))+
  geom_density(alpha = 0.8, colour = "black", fill = "blue4")

h <- qplot(x = IDEB,fill=..count.., data = Base3, geom = "histogram")
h+scale_fill_gradient(low="blue", high="red")


#Gráfico
a <- ggplot(Base3, aes(x=Ia, y=IDEB)) +
  geom_point(shape = 21, size = 2, stroke = 1,
             color = "black", fill = "red4") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", fill = "blue4") +
  ylab("IDEB") +
  xlab("Ia") +
  theme(plot.title = element_text(face = "bold.italic",lineheight = .8, size = 16))+
  theme(legend.position = "bottom")+
  ggtitle("IDEB ~ Ia")
a

#Gráfico IDEB e PC
c <- ggplot(Base3, aes(x=PC, y=IDEB)) +
  geom_point(shape = 21, size = 3, stroke = 1,
             color = "black", fill = "green4") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", fill = "blue4") +
  ylab("IDEB") +
  xlab("PC") +
  theme(plot.title = element_text(face = "bold.italic",lineheight = .8, size = 16))+
  theme(legend.position = "bottom")+
  ggtitle("IDEB ~ PC")
c

#Gráfico IDEB e Prof
d <- ggplot(Base3, aes(x=Prof, y=IDEB)) +
  geom_point(shape = 21, size = 1, stroke = 1,
             color = "black", fill = "red4") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", fill = "blue4") +
  ylab("IDEB") +
  xlab("Prof") +
  theme(plot.title = element_text(face = "bold.italic",lineheight = .8, size = 16))+
  theme(legend.position = "bottom")+
  ggtitle("IDEB ~ Prof")
d

#Gráfico IDEB e Saude
e <- ggplot(Base3, aes(x=Saude, y=IDEB)) +
  geom_point(shape = 21, size = 3, stroke = 1,
             color = "black", fill = "yellow4") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", fill = "blue4") +
  ylab("IDEB") +
  xlab("Saude") +
  theme(plot.title = element_text(face = "bold.italic",lineheight = .8, size = 16))+
  theme(legend.position = "bottom")+
  ggtitle("IDEB x Saude")
e

#Gráfico IDEB e Net
f <- ggplot(Base3, aes(x=Net, y=IDEB)) +
  geom_point(shape = 21, size = 1, stroke = 1,
             color = "black", fill = "yellow4") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", fill = "blue4") +
  ylab("IDEB") +
  xlab("Net") +
  theme(plot.title = element_text(face = "bold.italic",lineheight = .8, size = 16))+
  theme(legend.position = "bottom")+
  ggtitle("IDEB x Net")
f

#Gráfico IDEB e Homic
g <- ggplot(Base3, aes(x=Homic, y=IDEB)) +
  geom_point(shape = 21, size = 3, stroke = 1,
             color = "black", fill = "green4") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", fill = "blue4") +
  ylab("IDEB") +
  xlab("Homic") +
  theme(plot.title = element_text(face = "bold.italic",lineheight = .8, size = 16))+
  theme(legend.position = "bottom")+
  ggtitle("IDEB x Homic")
g

#Gráfico IDEB e Renda
h <- ggplot(Base3, aes(x=Renda, y=IDEB)) +
  geom_point(shape = 21, size = 1, stroke = 1,
             color = "black", fill = "green4") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", fill = "blue4") +
  ylab("IDEB") +
  xlab("Renda") +
  theme(plot.title = element_text(face = "bold.italic",lineheight = .8, size = 16))+
  theme(legend.position = "bottom")+
  ggtitle("IDEB x Renda")
h

#Gráfico IDEB e Rural
i <- ggplot(Base3, aes(x=Rural, y=IDEB)) +
  geom_point(shape = 21, size = 3, stroke = 1,
             color = "black", fill = "green4") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", fill = "blue4") +
  ylab("IDEB") +
  xlab("Rural") +
  theme(plot.title = element_text(face = "bold.italic",lineheight = .8, size = 16))+
  theme(legend.position = "bottom")+
  ggtitle("IDEB x Rural")
i

#Gráfio dos municipios, IDEB
all_mun_sp <- read_municipality(code_muni=35, year=2019)
dataset_final = left_join(all_mun_sp, Base, by=c("name_muni"="Município"))

ggplot() +
  geom_sf(data=dataset_final, aes(fill=IDEB), color= NA, size=.15)+
  labs(title="IDEB 2019 dos Municipíos do estado de São Paulo",
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_distiller(palette = "Reds", limits=c(4.9, 8.3),
                       name="IDEB")+
  theme_minimal()



#Gráfio dos municipios, Ip
ggplot() +
  geom_sf(data=dataset_final, aes(fill=Invest1 ), color= NA, size=.15)+
  labs(title="Investimento em Educação/PIB dos Municipíos do estado de São Paulo",
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_distiller(palette = "Greens", limits=c(0.3, 15.4),
                       name="Invest/PIB")+
  theme_minimal()

#Gráfio dos municipios, Rural/Urbano

ggplot() +
  geom_sf(data=dataset_final, aes(fill= Rural1 ), color= NA, size=.15)+
  labs(title="Distribuição dos Municipíos Rural/Urbano do Esdato de São Paulo",
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_distiller(palette = "RdGy", limits=c(0, 1),
                       name="Rural(0)-Urbano(1)")+
  theme_minimal()


