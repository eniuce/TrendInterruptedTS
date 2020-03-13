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
#    Importando arquivo rda 
######################################################################################
setwd("D:\\GoogleDrive UEM\\Assessorias\\João Ricardo Vissoci\\Eliane\\dados correto")

# Exemplo do arquivo rda utilizado:
# matrix with:
# 16 Rows: years 
# 10 Columns: 5 regions for Male and 5 regions for Female gender 

series            <-  get(load(file="suitaxasexomasc5regfem5reg.rda"))



###############################################################################################
#                  Análise de tendência 
###############################################################################################

#----------------------------------------------------------------------
# Criando os objetos para receber as informações do Modelo de tendencia
#----------------------------------------------------------------------

n=nrow(series)
crise.economica = 2008-2002+1
crise.politica = 2014-2002+1

# para estimar beta1 - tendência de 2002 a 2017
x1=1:n
x1

# para estimar beta2 - tendência de 2008 a 2014
x2=c(rep(0,crise.economica),1:(n-crise.economica))
x2

# para estimar beta3 - tendência de 2012 a 2017
x3 <- c(rep(0,crise.politica),1:(n-crise.politica))
x3
x <- cbind(x1,x2,x3) 
x

resultados.tendencia   <- matrix(nrow=10,ncol=20)
aux.imprimir = c(rep("Masculino",5),rep("Feminino",5))
series.fitted = series


#---------------------------------------------------
# Modelo de tendencia
#---------------------------------------------------

for (i in 1:10) {
  modelo              <- lm(ts(series[,i],freq=1,start=2002) ~ x)
  res.sumario         <- summary(modelo)
  series.fitted[,i]   <- modelo$fitted.values
  soma_tend_int1      <- sum(res.sumario$coef[2:3,c(1)]) # soma tendencias
  ep_tend_int1        <- sqrt(res.sumario$coef[2,c(2)]^2+res.sumario$coef[3,c(2)]^2) # ep resultante
  soma_tend_int2      <- sum(res.sumario$coef[c(2,3,4),c(1)]) # soma tendencias
  ep_tend_int2        <- sqrt(res.sumario$coef[c(2),c(2)]^2+res.sumario$coef[c(3),c(2)]^2+res.sumario$coef[4,c(2)]^2) # ep resultante

  
  resultados.tendencia[i,1:20]   <- c(as.vector(t(res.sumario$coef[2:4,c(1,2,4)])),round(c(soma_tend_int1, ep_tend_int1, 2*pt(-abs(soma_tend_int1/ep_tend_int1),df = 14)),3)
                                      ,round(c(soma_tend_int2, ep_tend_int2, 2*pt(-abs(soma_tend_int2/ep_tend_int2),df = 14)),3)
                                      , modelo$fitted.values[c(1,7,13,16)],res.sumario$r.squared )

}


#-------------------------------------------------------------
# Salvando os resutlados do Modelo de tendencia em arquivo csv
#-------------------------------------------------------------

resultados.tendencia            <-  data.frame(resultados.tendencia)
names(resultados.tendencia)     <-  as.character(c(#"beta1",             "se_beta1",           "p_beta1",
  #"beta2",             "se_beta2",           "p_beta2",
  #"beta3",             "se_beta3",           "p_beta3",
  "tend_antes_crise_e_2002_2007", "se","p", "mud_tend_apos_crise_e_2008_2013", "se","p","mud_tend_apos_crise_p_2014_2017","se","p","tend_2008_2013","se","p","tend_2014_2017","se","p","taxa estimada em 2002","taxa estimada em 2008","taxa estimada em 2014","taxa estimada em 2017","R2"))

row.names(resultados.tendencia) <-  c(paste("Masc", dados$REGIÃO[1:5]),paste("Fem", dados$REGIÃO[1:5]))
round(resultados.tendencia,3)

write.table(round(resultados.tendencia,3),"E:/João Ricardo Vissoci/Eliane/dados correto/ResultadosSexoTendencia.csv",row.names = T,col.names = T,sep=";")



#---------------------------------------------------
# Plotando graficos usando ggplot2 
#---------------------------------------------------
names             <-      expand.grid(reg=c("North", "Northeast",  "Southeast","South", "Mid-West"),gender=c("Male","Female")) %>% arrange(gender)
names.series      <-      paste(names[,1],names[,2])

quebra.sui.taxa.df            <- data.frame(RegiaoSexo = rep(names.series,each=16), date = rep(seq(as.Date("2002-01-01"),as.Date("2017-01-01"),by="year"),10), Value = c(series), fitted=c(series.fitted))
quebra.sui.taxa.df$RegiaoSexo <- factor(quebra.sui.taxa.df$RegiaoSexo, levels = names.series)

g = ggplot(aes(x = date, y = Value), data = quebra.sui.taxa.df) +
  geom_line(color = "black", size=1) +
  xlab("Time") + ylab("Suicide Rate") +
  theme_bw() +
  facet_wrap(~ RegiaoSexo, scales = "free_y", ncol = 5, dir = "h") 

g + geom_line(aes(x = date, y = fitted), color = "red",linetype = "dashed", size=1) +
  geom_vline(xintercept =c(as.Date("2008-01-01"),as.Date("2014-01-01")),col="blue",linetype = "dashed", size=1)




























