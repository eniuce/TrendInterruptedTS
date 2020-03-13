#=========================================================================#
#      Análise de Tendência em Séries Temporais Interrompidas             #
#-------------------------------------------------------------------------#
#            Eniuce Menezes de Souza e Marcia Lorena Alves                #                                               
#-------------------------------------------------------------------------#
#               DES: Dep. de Estatística - UEM - Brazil                   #
#               PBE: Mestrado em Bioestatística - UEM - Brazil            #
#=========================================================================#

library("ggplot2")
library("tidyverse")

######################################################################################
#    PARTE 1 - Escolher arquivo rda (Idade, Escolaridade e Raça) que quer trabalhar 
######################################################################################
setwd("D:\\GoogleDrive UEM\\Assessorias\\João Ricardo Vissoci\\Eliane\\dados correto")

# Exemplo do arquivo rda utilizado:
#                     Regiao      Idade      Ano     Value
# 1          1 Região Norte Idade 15-29  O.S2002  4.843051
# 2       2 Região Nordeste Idade 15-29  O.S2002  3.961266
# 3        3 Região Sudeste Idade 15-29  O.S2002  4.610102
# 4            4 Região Sul Idade 15-29  O.S2002  7.723145
# 5   5 Região Centro-Oeste Idade 15-29  O.S2002  7.669829
# 6          1 Região Norte Idade 30-44  O.S2002  4.265900

# Taxa de suicídios por Região e Idade
sui.taxa          <-      get(load(file = "suitaxafaixaetaria.rda"))
quebra.sui.taxa   <-      split(sui.taxa, sui.taxa$RegiaoIdade)
names             <-      expand.grid(reg=c("North", "Northeast",  "Southeast", "South", "Mid-West"),
                                      age=paste("Age", c("15-29","30-44","45-59","60-69"))) %>% arrange(reg)
names.series      <-      paste(names[,1],names[,2])
names(quebra.sui.taxa) <- names.series

# Taxa de suicídios por Região e Escolaridade
sui.taxa          <-      get(load(file = "suitaxaescolaridade1.rda"))
quebra.sui.taxa   <-      split(sui.taxa, sui.taxa$RegiaoEscolaridade)
names             <-      expand.grid(reg=c("North", "Northeast",  "Southeast", "South", "Mid-West"), #Central Western
                                      age=paste("Study", c("<=3 yr","4-7 yr",">=8 yr"))) %>% arrange(reg)
names.series      <-      paste(names[,1],names[,2])
names(quebra.sui.taxa) <- names.series

# Taxa de suicídios por Região e Raça/Cor
sui.taxa          <-      get(load(file = "suitaxaracacor1.rda"))
quebra.sui.taxa   <-      split(sui.taxa, sui.taxa$RegiaoRacaCor)
names             <-      expand.grid(reg=c("North", "Northeast",  "Southeast", "South", "Mid-West"),
                                      age=paste("Race/Color", c("Yellow","White","Indian","Parda","Black"))) %>% arrange(reg)
names.series      <-      paste(names[,1],names[,2])
names(quebra.sui.taxa) <- names.series


###############################################################################################
#                 PARTE 2 - Análise de tendência 
###############################################################################################

#----------------------------------------------------------------------
# Criando os objetos para receber as informações do Modelo de tendencia
#----------------------------------------------------------------------

crise.economica   <-      2008-2002+1
crise.politica    <-      2014-2002+1

x <- cbind(  1:length(quebra.sui.taxa[[1]]$Value),                                             # para estimar beta1 - tendência de 2002 a 2017
            c(rep(0,crise.economica),1:(length(quebra.sui.taxa[[1]]$Value)-crise.economica)),  # para estimar beta2 - tendência de 2008 a 2014
            c(rep(0,crise.politica),1:(length(quebra.sui.taxa[[1]]$Value)-crise.politica)))    # para estimar beta3 - tendência de 2012 a 2017

resultados_t   <-  r_quadrado_t   <- list(); for(i in 1:length(quebra.sui.taxa))
                                                  { 
                                                    resultados_t[[i]]           <- matrix(NA, ncol=3, nrow=9);
                                                    r_quadrado_t[[i]]           <- matrix(NA, ncol=1, nrow=3);
                                                    dimnames(resultados_t[[i]]) <- list( c("tend_antes_crise_e_2002_2007", "mud_tend_apos_crise_e_2008_2013", "mud_tend_apos_crise_p_2014_2017","tend_2008_2013","tend_2014_2017","taxa estimada em 2002","taxa estimada em 2008","taxa estimada em 2014","taxa estimada em 2017"),          #row names
                                                                                         c("Estimativa", "erro padrão", "valor p")); #col names
                                                    colnames(r_quadrado_t[[i]]) <- c("r_quadrado"); 
                                                  }

names(resultados_t) <- names(quebra.sui.taxa)



#---------------------------------------------------
# Modelo de tendencia
#---------------------------------------------------

for(j in 1:length(quebra.sui.taxa))
                                  {
                                    modelo                         <- lm(ts(quebra.sui.taxa[[j]]$Value, freq=1, start=2002) ~ x)
                                    resultados_t[[j]][c(1, 2, 3),] <- round(summary(modelo)[[4]][-1,-3], 3)
                                    quebra.sui.taxa[[j]]$fitted    <- modelo$fitted.values
                                    quebra.sui.taxa[[j]]$date      <- seq(as.Date("2002-01-01"),as.Date("2017-01-01"),by="year")
                                    quebra.sui.taxa[[j]][,1]       <- names.series[j]
                                  
                                    r_quadrado_t[[j]]        <- round(summary(lm(ts(quebra.sui.taxa[[j]]$Value, freq=1, start=2002) ~ x))$r.squared,3)
                                    resultados_t[[j]]        <- cbind(resultados_t[[j]], r_quadrado_t[[j]])
                                    soma_tend_int1           <- sum(resultados_t[[j]][1:2,1]) # soma tendencias
                                    ep_tend_int1             <- sqrt(resultados_t[[j]][1,2]^2+resultados_t[[j]][2,2]^2) # ep resultante
                                    resultados_t[[j]][4,1:3] <- round(c(soma_tend_int1, ep_tend_int1, 
                                                                      2*pt(-abs(soma_tend_int1/ep_tend_int1),df = 14)),3)#quantil
                                                                                         
                                    soma_tend_int2           <- sum(resultados_t[[j]][1:3,1]) # soma tendencias
                                    ep_tend_int2             <- sqrt(resultados_t[[j]][1,2]^2+resultados_t[[j]][2,2]^2+resultados_t[[j]][3,2]^2)# ep resultante
                                    resultados_t[[j]][5,1:3] <- round(c(soma_tend_int2,ep_tend_int2, 
                                                                 2*pt(-abs(soma_tend_int2/ep_tend_int2),#quantil
                                                                      df = 14)),3)
                                    resultados_t[[j]][6,1]   <- round(modelo$fitted.values[1],2)
                                    resultados_t[[j]][7,1]   <- round(modelo$fitted.values[7],2)
                                    resultados_t[[j]][8,1]   <- round(modelo$fitted.values[13],2)
                                    resultados_t[[j]][9,1]   <- round(modelo$fitted.values[16],2)
                                  }

#-------------------------------------------------------------
# Salvando os resutlados do Modelo de tendencia em arquivo csv
#-------------------------------------------------------------

export 		<- 		NULL
for(n in names(resultados_t)) 
  export 	<- 		rbind(export, c(n, rep( "", (ncol(resultados_t[[1]]))- 1)), resultados_t[[n]],  "")

write.table(export, file=paste("E:\\João Ricardo Vissoci\\Eliane\\dados correto\\Resultados Tendência", names(sui.taxa)[1], ".csv"), quote=F, sep=";", row.names=TRUE,na = "")


#---------------------------------------------------
# Plotando graficos usando ggplot2 
#---------------------------------------------------

quebra.sui.taxa.df   <-   do.call(rbind, quebra.sui.taxa)  

#>>>>>>Importante: Execute apenas linha referente a covariável que incluiu no modelo anterior
quebra.sui.taxa.df$RegiaoIdade <- factor(quebra.sui.taxa.df$RegiaoIdade, levels = names.series) 
quebra.sui.taxa.df$RegiaoEscolaridade <- factor(quebra.sui.taxa.df$RegiaoEscolaridade, levels = names.series) 
quebra.sui.taxa.df$RegiaoRacaCor <- factor(quebra.sui.taxa.df$RegiaoRacaCor, levels = names.series) 


g <- ggplot(aes(x = date, y = Value), data = quebra.sui.taxa.df) +
  geom_line(color = "black", size=1) +
  xlab("Time") + ylab("Suicide Rate") +
  theme_bw() +
  
  #>>>>>>Importante: Mudar nome da covariável a seguir de acordo com modelo anterior
  facet_wrap(~ RegiaoEscolaridade, scales = "free_y", ncol = 5, dir = "v") 

g + geom_line(aes(x = date, y = fitted), color = "red",linetype = "dashed", size=1) +
  geom_vline(xintercept =c(as.Date("2008-01-01"),as.Date("2014-01-01")),col="blue",linetype = "dashed", size=1)

