# Carregando os pacotes
setwd("/Users/apple/Desktop/dashboard")
getwd()



#-----------------------------------------------------------
#install.packages("fresh")
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(hrbrthemes)
library(viridis)
library(stringr)
library(fresh)
library(knitr)


#--------------------------------------------------------------

# Carregando os dados
#------------------------------------------------------------
#dados1 <- read.csv2(file="/Users/apple/Downloads/data_sisma.csv",sep=",",encoding = "ISO 8859-1")
#dados1$District=stringr::str_replace_all(string=dados1$District,pattern="ILHA DE MO\xcc\xe0AMBIQUE",replacement="Ilha de Moz")

dados1 <- read.csv(file="dados_dashh.csv",header = F)
dados1=dados1[-1,]


#------------------------------------------------------------------------

# Preprocessamento dos dados
#------------------------------------------------------------------------------------------
#dados1$F_type <- NULL
#dados1$women.receiving.postnatal.check.within.48.hours.of.birth <-NULL
#names(dados1) <- c("Distrito","Nome_Us","periodo","Partos","Nados_Vivo","Nados_Morto","Nados_Morto_intra","Nados_Morto_Macerados",
#                   "Reanimados_Sucesso","Reanimados_SemSucessos","Asfixia_Grave","Mortes_Materna_Directa","Mortes_Materna_Indirecta",
 #                  "ANC1","Menor12Semanas","ANC4","Partograma","Uterotonico","Ate_2oDia","Diarreias_Tratadas","Diarreias_Diagnosticada",
  #                 "Pneumonia_Tratada","Pneumonia_Diagnosticada","BCG","Sarampo_2aDose")
names(dados1) <-c("Distrito","Nome_Us","periodo","Partos","ANC1","ANC1 Coorte","ANC4","Menor12Semanas","1aCPP","Ate_2oDia","Uterotonico","Partograma",
                  "Nados_Vivo","RN_pele_a_pele","Asfixia_Grave","Reanimados_Sucesso","RN_Aleitamento_1aHora","Pneumonia_Tratada","Pneumonia_Diagnosticada",
                  "Diarreias_Diagnosticada","SRO & Zinco","So Zn","So SRO","Complicacoes Abortos com certas complicacoes (Hemorragia Sepses","Complicacoes Gravidez ectopica",
                  "Complicacoes Hemorragia Ante Parto","Complicacoes Hemorragia Pos Parto","Complicacoes Parto prolongado/obstruido",
                  "Complicacoes Roputra Uterina","Complicacoes Sespsi posparto","Complicacoes pre-eclampsia/eclampsia","Mortes Abortos com certas complicacoes (Hemorragia Sepses","Mortes Gravidez ectopica",
                  "Mortes Hemorragia Ante Parto","Mortes Hemorragia Pos Parto","Mortes Parto prolongado/obstruido","Mortes pre-eclampsia/eclampsia","Mortes Roputra Uterina","Mortes Sespsi posparto","RN com sepsi",
                  "RN prematuro","RN com baixo peso","BCG aos 11meses","DPT1","DPT3","Polio1","Sarampo1","Sarampo2","CCV")

# Trocando os NA por zero
dados1[is.na(dados1)]=0

str(dados1)

for (i in 4:49){
   dados1[,i]=as.numeric(dados1[,i]) 
}



# Separando a coluna de periodo em duas
#dados1 <-tidyr::separate(data=dados1,col = periodo,into=c("Mes","Ano"),sep=" ")
dados1 <-tidyr::separate(data=dados1,col = periodo,into=c("Mes","Ano"),sep=" ")

#dados1$Mes=factor(x=dados1$Mes,labels=c("January","February","March","April","May","June","July","August","September","October","November","December"), 
 #                 levels = c("Janeiro","Fevereiro","Mar̤co","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro"))

dados1$Mes=factor(x=dados1$Mes,labels=c("January","February","March","April","May","June","July","August","September","October","November","December"), 
                  levels = c("Jan'","Fev","Mar'","Abr","Mai","Jun'","Jul'","Ago","Set","Out","Nov'","Dez"))
dados1$Ano=factor(x=dados1$Ano,labels=c("2019","2020","2021","2022"), 
                  levels = c("19","20","21","22"))


# Adicionand nova coluna em formato data
dados1 <-unite(data=dados1,col="periodo",c("Mes","Ano"),sep="-",remove=F)
dados1$periodo=my(dados1$periodo)

dados1[is.na(dados1)]=0


# Inicio de Maternidade


#  ***grafico de barra entre uterotonico e partos realizados

grafico(2021,"July","ERATI","CS Alua")

grafico=function(ANO,MES,DISTRITO,US){
  dados_mat1=dados1 %>%
    select(Distrito,Nome_Us,periodo, Mes, Ano,Partos,Uterotonico,Partograma) %>%
    filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO, Nome_Us %in% US) 
  
  uterotorico_pivot <-tidyr::pivot_longer(
    data=dados_mat1,
    cols=c("Partos","Uterotonico"),
    names_to = "partos_utrrotonico",
    values_to = "valores"
  )
  
  uterotonico_mat=uterotorico_pivot %>% group_by(partos_utrrotonico) %>%
    summarise(
      partos_utr=sum(valores)
    )
  ##B4E1FF
 
  grafico_uterotonico= ggplot(data=uterotonico_mat,aes(y=partos_utr,x=partos_utrrotonico))+
    geom_col(fill="#004C99")+
    geom_text(aes(label=partos_utr),position =position_stack(vjust =1.1))+
    theme_bw()+
    xlab("")+
    ylab("")+
    theme(
      axis.text.y =element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  return(grafico_uterotonico)
}

#Cartao de info de % de uterotonicos
mat_box=function(ANO,MES,DISTRITO,US){
  dados_mat=dados1 %>%
    select(Distrito,Nome_Us,periodo, Mes, Ano,Partos,Uterotonico,Partograma) %>%
    filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
  
  uterotorico_pivot <-tidyr::pivot_longer(
    data=dados_mat,
    cols=c("Partos","Uterotonico"),
    names_to = "partos_utrrotonico",
    values_to = "valores"
  )
  
  uterotonico=uterotorico_pivot %>% group_by(partos_utrrotonico) %>%
    summarise(
      utero_valores=sum(valores)
    ) %>%
    arrange(desc(utero_valores)) 
  
  mat=uterotonico %>%
    mutate(
      pertg=uterotonico[uterotonico["partos_utrrotonico"]=="Uterotonico","utero_valores"]/uterotonico[uterotonico["partos_utrrotonico"]=="Partos","utero_valores"],
      partos_utrrotonico=factor(x=partos_utrrotonico,levels = partos_utrrotonico)
    )
  
  ate2dia_percentagem=mat[mat["partos_utrrotonico"]=="Partos","pertg"]*100
  ate2dia_percentagem
  return(round(ate2dia_percentagem,0))
}


  
# grafico de % de partograma ao longo dos meses por ano

funcao_grafico_tendencia_partograma("ERATI","CS Alua")
funcao_grafico_tendencia_partograma=function(DISTRITO,US){
  dados_mat1=dados1 %>%
    select(Distrito,Nome_Us,periodo, Mes, Ano,Partos,Uterotonico,Partograma) %>%
    filter(Ano %in% c(2021,2022),Distrito %in% DISTRITO,Nome_Us %in% US)
  
  dados_mat_periodo=dados_mat1 %>% group_by(periodo) %>%
    summarise(
      Partos=sum(Partos),
      Partograma=sum(Partograma)
    ) %>%
    mutate(Perc_Partograma=Partograma/Partos)
  
  
  partograma_tempo=ggplot(dados_mat_periodo,aes(x=periodo,y=Perc_Partograma))+
    geom_col(fill="#001933")+
    geom_text(aes(label=round(Perc_Partograma*100,0)),position =position_stack(vjust =1.1))+
    theme_bw()+
    theme(
      axis.text.y =element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )+
    ylab("% de partograma")+
    xlab("")
  
  return(partograma_tempo)
}



 # grafico de barra entre nados vivos e ate 2o dia
funcao_cpp(2021,"July","ERATI","CS Alua")

 funcao_cpp=function(ANO,MES,DISTRITO,US){
   dados_cpp=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,"1aCPP",Ate_2oDia) %>%
     filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
   
   cpp_pivot <-tidyr::pivot_longer(
     data=dados_cpp,
     cols=c("Ate_2oDia","1aCPP"),
     names_to = "Nados_cpp",
     values_to = "valores"
   )
   
   ate_2o_dia=cpp_pivot %>% group_by(Nados_cpp) %>%
     summarise(
       nados_cpp_valores=sum(valores)
     ) %>%
     arrange(desc(nados_cpp_valores))%>%
     mutate(Nados_cpp=factor(x=Nados_cpp,levels = Nados_cpp))
  # FCC40E
   #D1063C
   grafico_cpp= ggplot(data=ate_2o_dia,aes(y=nados_cpp_valores,x=Nados_cpp))+
     geom_col(fill="#404040")+
     geom_text(aes(label=nados_cpp_valores),position =position_stack(vjust =1.1))+
     theme_bw()+
     xlab("")+
     ylab("")+
     theme(
       axis.text.y =element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )
   return(grafico_cpp)
 }
 
 
# box de info de % de ate 2o dia apos o parto
cpp_box=function(ANO,MES,DISTRITO,US){
  dados_cpp=dados1 %>%
    select(Distrito,Nome_Us,periodo,Mes,Ano,"1aCPP",Ate_2oDia) %>%
    filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
  
  cpp_pivot <-tidyr::pivot_longer(
    data=dados_cpp,
    cols=c("Ate_2oDia","1aCPP"),
    names_to = "Nados_cpp",
    values_to = "valores"
  )
  
  ate_2o_dia=cpp_pivot %>% group_by(Nados_cpp) %>%
    summarise(
      nados_cpp_valores=sum(valores)
    ) %>%
    arrange(desc(nados_cpp_valores)) 
  
  cp=ate_2o_dia %>%
    mutate(
      pertg=ate_2o_dia[ate_2o_dia["Nados_cpp"]=="Ate_2oDia","nados_cpp_valores"]/ate_2o_dia[ate_2o_dia["Nados_cpp"]=="1aCPP","nados_cpp_valores"],
      Nados_cpp=factor(x=Nados_cpp,levels = Nados_cpp)
    )
  
  ate2dia_percentagem=cp[cp["Nados_cpp"]=="Ate_2oDia","pertg"]*100
  ate2dia_percentagem
  return(round(ate2dia_percentagem,0))
}
 


 # Tendencia de total de partos realizados ao longo dos meses por ano
funcao_partos_tendencia("ERATI","CS Alua")
 funcao_partos_tendencia=function(DISTRITO,US){
   dados_mat1=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,Partos,Uterotonico,Partograma) %>%
     filter(Ano %in% c(2021,2022),Distrito %in% DISTRITO,Nome_Us %in% US)
   
   dados_mat_periodo=dados_mat1 %>% group_by(periodo) %>%
     summarise(
       Partos=sum(Partos),
       Partograma=sum(Partograma)
     ) %>%
     mutate(Perc_Partograma=Partograma/Partos)
   
   grafico_partos_tendencia=ggplot(dados_mat_periodo,aes(x=periodo,y=Partos))+
     geom_line(color="#004C99")+
     theme_bw()+
     theme(
       #axis.text.y =element_blank(),
       #panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )+
     xlab("")+
     ylab("No de partos")+
      scale_y_continuous(limits = c(0,NA))
   
   return(grafico_partos_tendencia)
 }
 ?scale_y_continuous

 # fim de maternidade
 
 # inicio de Recem nascido
 
 #  ***% de aleitamento a 1a HORA e contacto pele-a-pele
 


 grafico_aleitamento_pele=function(DISTRITO,US){
   dados_rn=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,Partos,RN_Aleitamento_1aHora,RN_pele_a_pele) %>%
   filter(Ano %in% c(2021,2022),Distrito %in% DISTRITO, Nome_Us %in% US) 
     
   
   dados_rn_pivot <-tidyr::pivot_longer(
     data=dados_rn,
     cols=c("Partos","RN_Aleitamento_1aHora","RN_pele_a_pele"),
     names_to = "categoria",
     values_to = "valores"
   )
   
   diagnostico_tratadas=dados_rn_pivot %>% group_by(periodo,categoria) %>%
     summarise(
       diagnos_trata=sum(valores),.groups = 'drop'
     )
   ?scale_y_continuous
  
   
   aa=tidyr::pivot_wider(data=diagnostico_tratadas,
                        names_from = categoria,
                        values_from=diagnos_trata) %>%
     mutate(
       perc_aleitamento=RN_Aleitamento_1aHora/Partos,
       perc_contacto=RN_pele_a_pele/Partos
     )
   
  b=tidyr::pivot_longer(
     data=aa,
     cols=c("perc_aleitamento","perc_contacto"),
     names_to = "category",
     values_to = "percentagem"
   )
  grafico_de_linha=ggplot(data=b,aes(x=periodo,y=percentagem,col=category))+
    geom_line()+
     scale_y_continuous(limits =c(0,1.5),
                        breaks=c(0,0.5,1,1.5)
                        ,labels = c("0%","50%","100%","150%"))+
    # geom_text(aes(label=percentagem),position =position_stack(vjust =1.1))+
     theme_bw()+
     xlab("")+
     ylab("% de RN")+
     theme(
      # axis.text.y =element_blank(),
       panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )+
  guides(color=guide_legend(title="Legenda"))
   return(grafico_de_linha)
 }
 
 
 # grafico de barra entre asfixia e reanimados com sucessos
 
 funcao_grafico_barra_asfixia_reanimados=function(ANO,MES,DISTRITO,US){
   dados_asfixia=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,Asfixia_Grave,Reanimados_Sucesso)%>%
     filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
   
   
   asfixia_pivot <-tidyr::pivot_longer(
     data=dados_asfixia,
     cols=c("Asfixia_Grave","Reanimados_Sucesso"),
     names_to = "asfixia_reanimados",
     values_to = "valores"
   )
   
   asfixia_sumario=asfixia_pivot %>% group_by(asfixia_reanimados) %>%
     summarise(
       total=sum(valores)
     ) %>%
     arrange(desc(total))%>%
     mutate(asfixia_reanimados=factor(x=asfixia_reanimados,levels = asfixia_reanimados))
   
   graficobarra_asfixia= ggplot(data=asfixia_sumario,aes(y=total,x=asfixia_reanimados))+
     geom_col(fill="#00CCCC")+
     geom_text(aes(label=total),position =position_stack(vjust =1.1))+
     theme_bw()+
     xlab("")+
     ylab("")+
     theme(
       axis.text.y =element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )
   return(graficobarra_asfixia)
 }
 
 
 # box de info de % de reanimados com sucessos

 
 funcao_percentagem_reanimacao=function(ANO,MES,DISTRITO,US){
   dados_asfixia=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,Asfixia_Grave,Reanimados_Sucesso)%>%
     filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
    
     
     asfixia_pivot <-tidyr::pivot_longer(
       data=dados_asfixia,
       cols=c("Asfixia_Grave","Reanimados_Sucesso"),
       names_to = "asfixia_reanimados",
       values_to = "valores"
     )
     
     asfixia_sumario=asfixia_pivot %>% group_by(asfixia_reanimados) %>%
       summarise(
         total=sum(valores)
       ) 
   
   tbl_reanimados=asfixia_sumario %>%
     mutate(
       pertg=asfixia_sumario[asfixia_sumario["asfixia_reanimados"]=="Reanimados_Sucesso","total"]/asfixia_sumario[asfixia_sumario["asfixia_reanimados"]=="Asfixia_Grave","total"],
     )
   
   reanimados_percentagem=tbl_reanimados[tbl_reanimados["asfixia_reanimados"]=="Reanimados_Sucesso","pertg"]*100
   
   return(round(reanimados_percentagem,0))
 }
 
 # % de RN com asfixia grave e reanimados com sucessos por distrito

 
 funcao_reanimados_distrito=function(ANO,MES){
   dados_reanimados_asfixia=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,Asfixia_Grave,Reanimados_Sucesso) %>%
     filter(Ano==ANO, Mes %in% MES)
 
   
   sumario_distrito=dados_reanimados_asfixia %>% group_by(Distrito) %>%
     summarise(
       rn_asfixia=sum(Asfixia_Grave),
       rn_reanimados=sum(Reanimados_Sucesso)
     ) %>%
     mutate(Perc_reanimados=rn_reanimados/rn_asfixia) %>%
     arrange(Perc_reanimados)%>%
     mutate(Distrito=factor(x=Distrito,levels = Distrito))
   
   grafico_reanimados_distrito=ggplot(sumario_distrito,aes(x=Perc_reanimados,y=Distrito))+
     geom_col(fill="#00CCCC")+
     theme_bw()+
     theme(
       axis.text.x = element_text(angle = 90, vjust = 0.8,size = 10),
       axis.text.y =element_text(vjust = 0.2,size=7),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )+
     xlab("% de RN")+
     ylab("")+
    scale_x_continuous(limits=c(0,1.2),
                            breaks=c(0,0.15,0.3,0.45,0.60,0.75,0.9,1,1.2),
                             labels = c("0%","15%","30%","45%","60%","75%","90%","100%","120%"))
   
   return(grafico_reanimados_distrito)
 }
 
 complicacoes_neonatais=function(ANO,MES,DISTRITO,US){
    groupo <- dados1 %>% 
       select(Distrito,Nome_Us,periodo,Mes,Ano,"RN com sepsi","RN com baixo peso","RN prematuro",Asfixia_Grave) %>%
      filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
   # filter(Ano==2021)
    
    dados_reanimacao <-tidyr::pivot_longer(
       data=groupo,
       cols=c("RN com sepsi","RN com baixo peso","RN prematuro","Asfixia_Grave"),
       names_to = "complicacoes",
       values_to = "Nr_complicacoes"
    )
    
    grupo1 <-dados_reanimacao %>% group_by(complicacoes) %>%
       summarise(
          Total_complicacoes=sum(Nr_complicacoes)
       ) %>%
       arrange(desc(Total_complicacoes)) %>%
       mutate(complicacoes=factor(x=complicacoes,levels = complicacoes))
    
    grupo1$perc=grupo1$Total_complicacoes/sum(grupo1$Total_complicacoes)
    
  g=ggplot(data=grupo1,aes(x=1,y=perc,fill=complicacoes))+
       geom_bar(stat = "identity")+
       geom_text(
          aes(label=paste(round(perc,1)*100,"%")),
          position =position_stack(vjust =.5))+
       coord_polar(theta = "y")+
       theme_void()
  return(g)
  
 }

 
 
 
 
 
 #fim de recem nascido
 
 # inicio de Complicacoes e Mortes Maternas
 
 #grafico circular de causas maternas

 
 causas_mm =function(ANO,MES,DISTRITO,US){
    dados_MM=dados1 %>%
       select(Distrito,Nome_Us,periodo,Mes,Ano,"Complicacoes Abortos com certas complicacoes (Hemorragia Sepses","Complicacoes Gravidez ectopica",
              "Complicacoes Hemorragia Ante Parto","Complicacoes Hemorragia Pos Parto","Complicacoes Parto prolongado/obstruido",
              "Complicacoes Roputra Uterina","Complicacoes Sespsi posparto","Complicacoes pre-eclampsia/eclampsia","Mortes Abortos com certas complicacoes (Hemorragia Sepses","Mortes Gravidez ectopica",
              "Mortes Hemorragia Ante Parto","Mortes Hemorragia Pos Parto","Mortes Parto prolongado/obstruido","Mortes pre-eclampsia/eclampsia","Mortes Roputra Uterina","Mortes Sespsi posparto") %>%
       filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
    
    mm_pivot <-tidyr::pivot_longer(
       data=dados_MM,
       cols=c(6:21),
       names_to = "Complicacoes/MM",
       values_to = "valores"
    )
    
    d=separate(mm_pivot,col =6,into=c("Tipo","causas obstetricas"),sep=" ",extra="merge")
    b=tidyr::pivot_wider(d,names_from = 6,values_from = 8)
    
    dd=b %>% group_by(`causas obstetricas`,Distrito) %>%
       summarise(
          comp=sum(Complicacoes),
          Mortes=sum(Mortes), .groups = "drop"
       ) %>%
       mutate(letalidade=(Mortes/comp)*100) 
    
    grafico_causas_mm=plot_ly(dd, 
                              labels = ~`causas obstetricas`, 
                              values = ~Mortes, type = 'pie') %>%
       layout(title = 'Causas de Morte Materna',
              xaxis = list(showgrid = FALSE, 
                           zeroline = FALSE, 
                           showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, 
                           zeroline = TRUE, 
                           showticklabels = FALSE))
    return(grafico_causas_mm)
 }
 
 
 # tabela de mortes maternas
 tabela_mm=function(ANO,MES){
    dados_MM=dados1 %>%
       select(Distrito,Nome_Us,periodo,Mes,Ano,"Complicacoes Abortos com certas complicacoes (Hemorragia Sepses","Complicacoes Gravidez ectopica",
              "Complicacoes Hemorragia Ante Parto","Complicacoes Hemorragia Pos Parto","Complicacoes Parto prolongado/obstruido",
              "Complicacoes Roputra Uterina","Complicacoes Sespsi posparto","Complicacoes pre-eclampsia/eclampsia","Mortes Abortos com certas complicacoes (Hemorragia Sepses","Mortes Gravidez ectopica",
              "Mortes Hemorragia Ante Parto","Mortes Hemorragia Pos Parto","Mortes Parto prolongado/obstruido","Mortes pre-eclampsia/eclampsia","Mortes Roputra Uterina","Mortes Sespsi posparto") %>%
       filter(Ano==ANO,Mes %in% MES)
    
    mm_pivot <-tidyr::pivot_longer(
       data=dados_MM,
       cols=c(6:21),
       names_to = "Complicacoes/MM",
       values_to = "valores"
    )
    
    d=separate(mm_pivot,col =6,into=c("Tipo","causas obstetricas"),sep=" ",extra="merge")
    b=tidyr::pivot_wider(d,names_from = 6,values_from = 8)
    
    bb= b%>% group_by(Distrito) %>%
       summarise(
          Complicacoes=sum(Complicacoes),
          Mortes=sum(Mortes)
       )%>%
       mutate(letalidade=(Mortes/Complicacoes)*100) %>%
       arrange(desc(Mortes))%>%
       mutate(Distrito=factor(x=Distrito,levels = Distrito))
    
    
   return(tabela1=head(bb,n=9))
    
 }
 

 
 #grafico de tendencia de letalidade
 grafico_tendencia_mm("ERATI","CS Alua")
 
 grafico_tendencia_mm=function(DISTRITO,US){
    dados_MM=dados1 %>%
       select(Distrito,Nome_Us,periodo,Mes,Ano,"Complicacoes Abortos com certas complicacoes (Hemorragia Sepses","Complicacoes Gravidez ectopica",
              "Complicacoes Hemorragia Ante Parto","Complicacoes Hemorragia Pos Parto","Complicacoes Parto prolongado/obstruido",
              "Complicacoes Roputra Uterina","Complicacoes Sespsi posparto","Complicacoes pre-eclampsia/eclampsia","Mortes Abortos com certas complicacoes (Hemorragia Sepses","Mortes Gravidez ectopica",
              "Mortes Hemorragia Ante Parto","Mortes Hemorragia Pos Parto","Mortes Parto prolongado/obstruido","Mortes pre-eclampsia/eclampsia","Mortes Roputra Uterina","Mortes Sespsi posparto") %>%
    filter(Ano %in% c(2018,2019,2020,2021,2022),Distrito %in% DISTRITO,Nome_Us %in% US)
    
    mm_pivot <-tidyr::pivot_longer(
       data=dados_MM,
       cols=c(6:21),
       names_to = "Complicacoes/MM",
       values_to = "valores"
    )
    
    d=separate(mm_pivot,col =6,into=c("Tipo","causas obstetricas"),sep=" ",extra="merge")
    b=tidyr::pivot_wider(d,names_from = 6,values_from = 8)
    
    bb= b%>% group_by(periodo) %>%
       summarise(
          Complicacoes=sum(Complicacoes),
          Mortes=sum(Mortes)
       )%>%
       mutate(letalidade=(Mortes/Complicacoes)*100)
    
    
   gg01= ggplot(data=bb,aes(x=periodo,y=letalidade))+
       geom_line(fill="yellow4")+
       theme_bw()+
       theme(
          axis.text.x = element_text(angle = 90, vjust = 0.8,size = 10),
          axis.text.y =element_text(vjust = 0.2,size=7),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
       )+
       xlab("")+
       ylab("Taxa de letalidade")+
      scale_y_continuous(limits = c(0,5),
                         breaks = c(0,1,2,3,4,5),
                         labels = c("0%","1%","2%","3%","4%","5%"))
   
 #  gg02= ggplot(data=bb,aes(x=periodo,y=Mortes))+
 #     geom_line(fill="yellow4")+
 #     theme_bw()+
 #     theme(
 #        axis.text.x = element_text(angle = 90, vjust = 0.8,size = 10),
 #        axis.text.y =element_text(vjust = 0.2,size=7),
 #        panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank()
#      )+
#      xlab("")+
#      ylab("Taxa de letalidade")+
#      scale_y_continuous(limits = c(0,NA))
   
#  gg=grid.arrange(gg01,gg02,nrow=2)
   
    return(gg01)
 }

 
Numero_mm=function(ANO,MES,DISTRITO,US){
   dados_MM=dados1 %>%
      select(Distrito,Nome_Us,periodo,Mes,Ano,"Complicacoes Abortos com certas complicacoes (Hemorragia Sepses","Complicacoes Gravidez ectopica",
             "Complicacoes Hemorragia Ante Parto","Complicacoes Hemorragia Pos Parto","Complicacoes Parto prolongado/obstruido",
             "Complicacoes Roputra Uterina","Complicacoes Sespsi posparto","Complicacoes pre-eclampsia/eclampsia","Mortes Abortos com certas complicacoes (Hemorragia Sepses","Mortes Gravidez ectopica",
             "Mortes Hemorragia Ante Parto","Mortes Hemorragia Pos Parto","Mortes Parto prolongado/obstruido","Mortes pre-eclampsia/eclampsia","Mortes Roputra Uterina","Mortes Sespsi posparto") %>%
      filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
   
   mm_pivot <-tidyr::pivot_longer(
      data=dados_MM,
      cols=c(6:21),
      names_to = "Complicacoes/MM",
      values_to = "valores"
   )
   
   d=separate(mm_pivot,col =6,into=c("Tipo","causas obstetricas"),sep=" ",extra="merge")
   b=tidyr::pivot_wider(d,names_from = 6,values_from = 8)
   
   bb= b%>% group_by(periodo) %>%
      summarise(
         Complicacoes=sum(Complicacoes),
         Mortes=sum(Mortes))
         
         return(sum(b$Mortes))
}
Numero_complicacoes=function(ANO,MES,DISTRITO,US){
   dados_MM=dados1 %>%
      select(Distrito,Nome_Us,periodo,Mes,Ano,"Complicacoes Abortos com certas complicacoes (Hemorragia Sepses","Complicacoes Gravidez ectopica",
             "Complicacoes Hemorragia Ante Parto","Complicacoes Hemorragia Pos Parto","Complicacoes Parto prolongado/obstruido",
             "Complicacoes Roputra Uterina","Complicacoes Sespsi posparto","Complicacoes pre-eclampsia/eclampsia","Mortes Abortos com certas complicacoes (Hemorragia Sepses","Mortes Gravidez ectopica",
             "Mortes Hemorragia Ante Parto","Mortes Hemorragia Pos Parto","Mortes Parto prolongado/obstruido","Mortes pre-eclampsia/eclampsia","Mortes Roputra Uterina","Mortes Sespsi posparto") %>%
      filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
   
   mm_pivot <-tidyr::pivot_longer(
      data=dados_MM,
      cols=c(6:21),
      names_to = "Complicacoes/MM",
      values_to = "valores"
   )
   
   d=separate(mm_pivot,col =6,into=c("Tipo","causas obstetricas"),sep=" ",extra="merge")
   b=tidyr::pivot_wider(d,names_from = 6,values_from = 8)
   
   bb= b%>% group_by(periodo) %>%
      summarise(
         Complicacoes=sum(Complicacoes),
         Mortes=sum(Mortes))
   
   return(sum(b$Complicacoes))
}
 
 #Inicio de CPN
 
 
 #  ***grafico de barra entre 1a CPN e menor de 12 semanas
 grafico_menor12=function(ANO,MES,DISTRITO,US){
   dados_cpn=dados1 %>%
     select(Distrito,Nome_Us,periodo,Mes,Ano,ANC1,Menor12Semanas,ANC4) %>%
    filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO, Nome_Us %in% US)
     #filter(Ano==2021)
   
   Menor_12s_pivot <-tidyr::pivot_longer(
     data=dados_cpn,
     cols=c("ANC1","Menor12Semanas"),
     names_to = "anc1_menor12",
     values_to = "valores"
   )
   
   sumario_anc1_12s=Menor_12s_pivot %>% group_by(anc1_menor12) %>%
     summarise(
      anc1_12s=sum(valores)
     )
   
   grafico_pertagem_menor12s= ggplot(data=sumario_anc1_12s,aes(y=anc1_12s,x=anc1_menor12))+
     geom_col(fill="#99CCFF")+
     geom_text(aes(label=anc1_12s),position =position_stack(vjust =1.1))+
     theme_bw()+
     xlab("")+
     ylab("")+
     theme(
       axis.text.y =element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )
   return(grafico_pertagem_menor12s)
 }
 
 #Cartao de info de % de menor de 12 semanas
 cpn_box=function(ANO,MES,DISTRITO,US){
   dados_cpn=dados1 %>%
     select(Distrito,Nome_Us,periodo,Mes,Ano,ANC1,Menor12Semanas,ANC4) %>%
     filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
     #filter(Ano==2021)
   
   menor12s_pivot <-tidyr::pivot_longer(
     data=dados_cpn,
     cols=c("ANC1","Menor12Semanas"),
     names_to = "anc1_menor12s",
     values_to = "valores"
   )
   
   sumario_anc1_12ss=menor12s_pivot %>% group_by(anc1_menor12s) %>%
     summarise(
       total=sum(valores)
     )
   
   menor12perc=sumario_anc1_12ss %>%
     mutate(
       pertg=sumario_anc1_12ss[sumario_anc1_12ss["anc1_menor12s"]=="Menor12Semanas","total"]/sumario_anc1_12ss[sumario_anc1_12ss["anc1_menor12s"]=="ANC1","total"]
     )
   
   Menor12s_percentagem=menor12perc[menor12perc["anc1_menor12s"]=="Menor12Semanas","pertg"]*100
   
   return(round(Menor12s_percentagem,0))
 }
 
 
 # grafico de % de menor de 12 semanas por ano
 funcao_grafico_menor12_distrito("ERATI","CS Alua")
 funcao_grafico_menor12_distrito=function(DISTRITO,US){
   dados_cpn=dados1 %>%
     select(Distrito,Nome_Us,periodo,Mes,Ano,ANC1,Menor12Semanas,ANC4) %>%
     filter(Ano %in% c(2018,2019,2020,2021,2022),Distrito %in% DISTRITO, Nome_Us %in% US)
    # filter(Ano==2021)
   
   sumario_menor12_distrito=dados_cpn %>% group_by(periodo) %>%
     summarise(
       anc1=sum(ANC1),
       menor_12=sum(Menor12Semanas)
     ) %>%
     mutate(Perc_menor12=menor_12/anc1) #%>%
    # arrange(desc(Perc_menor12)) #%>%
   #  mutate(Distrito=factor(x=Distrito,levels = Distrito)) 
   
   
   menor12_distrito=ggplot(sumario_menor12_distrito,aes(x=periodo,y=Perc_menor12))+
     geom_area(fill="#99CCFF")+
    #geom_text(aes(label=round(Perc_menor12*100,0)),position =position_stack(vjust =1.1))+
     theme_bw()+
     theme(
      # axis.text.x = element_text(angle = 90,size=8),
       axis.text.y =element_text(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )+
     ylab("% de MG")+
     xlab("")+
      scale_y_continuous(limits=c(0,0.6),
                         breaks=c(0,0.10,0.20,0.25,0.30,0.4,0.5,0.6),
                         labels = c("0%","10%","20%","25%","30%","40%","50%","60%"))
   
   return(menor12_distrito)
 }
 
 
 
 # grafico de barra entre 4a CPN e 1a CPN Coorte
 
 funcao_cpn4=function(ANO,MES,DISTRITO,US){
   dados_cpn=dados1 %>%
     select(Distrito,Nome_Us,periodo,Mes,Ano,ANC1,ANC4) %>%
     filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
     
   anc_anc4_pivot <- tidyr::pivot_longer(
     data=dados_cpn,
     cols=c("ANC1","ANC4"),
     names_to = "anc1_anc4",
     values_to = "valores"
   )
   
   anc_anc4=anc_anc4_pivot %>% group_by(anc1_anc4) %>%
     summarise(
       total=sum(valores)
     ) %>%
     arrange(desc(total))%>%
     mutate(anc1_anc4=factor(x=anc1_anc4,levels = anc1_anc4))
   
   grafico_anc4= ggplot(data=anc_anc4,aes(y=total,x=anc1_anc4))+
     geom_col(fill="#00CCCC")+
     geom_text(aes(label=total),position =position_stack(vjust =1.1))+
     theme_bw()+
     xlab("")+
     ylab("")+
     theme(
       axis.text.y =element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )
   return(grafico_anc4)
 }
 
 
 # box de info de % de 4a cpn
 anc4_box=function(ANO,MES,DISTRITO,US){
   dados_cpn=dados1 %>%
     select(Distrito,Nome_Us,periodo,Mes,Ano,ANC1,ANC4) %>%
     filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
   
   anc_anc4_pivot <- tidyr::pivot_longer(
     data=dados_cpn,
     cols=c("ANC1","ANC4"),
     names_to = "anc1_anc4",
     values_to = "valores"
   )
   
   anc_anc4=anc_anc4_pivot %>% group_by(anc1_anc4) %>%
     summarise(
       total=sum(valores)
     )
   
   cp=anc_anc4 %>%
     mutate(
       pertg=anc_anc4[anc_anc4["anc1_anc4"]=="ANC4","total"]/anc_anc4[anc_anc4["anc1_anc4"]=="ANC1","total"],
     )
   
   anc4_percentagem=cp[cp["anc1_anc4"]=="ANC1","pertg"]*100
   return(round(anc4_percentagem,0))
 }
 
 
 ?icon
 # Tendencia de % de gap(1aCPN/4a CPN) ao longo do tempo
 funcao_gap_cpn("ERATI","CS Alua")
 funcao_gap_cpn=function(DISTRITO,US){
   dados_cpn=dados1 %>%
     select(Distrito,Nome_Us,periodo,Mes,Ano,ANC1,ANC4) %>%
   filter(Ano %in% c(2021,2022),Distrito %in% DISTRITO,Nome_Us %in% US)
   
   
   anc1_anc4_pivot <-tidyr::pivot_longer(
     data=dados_cpn,
     cols=c("ANC1","ANC4"),
     names_to = "anc1_anc4",
     values_to = "valores"
   )
   
   sumario_anc1_anc4 <- anc1_anc4_pivot %>% group_by(periodo,anc1_anc4) %>%
     summarise(
       total=sum(valores),.groups = 'drop'
     )
   
   d=dados_cpn %>% group_by(Distrito,periodo) %>%
     summarise(
       anc11=sum(ANC1),
       anc44=sum(ANC4),.groups = 'drop'
     ) %>%
     mutate(per=((anc11-anc44)/anc11))
   
  grafico= ggplot(data=sumario_anc1_anc4)+
     geom_line(aes(x=periodo,y=total,col=anc1_anc4))+
    # geom_text(aes(label=per),position =position_stack(vjust =1.1))+
     theme_bw()+
     theme(
      # axis.text.y =element_blank(),
       #panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank(),
       legend.position = "bottom"
     )+
     xlab("")+
     ylab("ANC1/ANC4")+
     scale_y_continuous(limits = c(0,NA))+
     guides(color=guide_legend(title="Legenda"))
   
   return(grafico)
 }
 
 #Fim de CPN
 
 # Inicio de Saude infantil
 
 #  ***grafico de barra entre diarreias diagnosticadas e tratadas com ambos compostos

 
 graficobarra_diagnostico_tratado=function(ANO,MES,DISTRITO,US){
   dados_diarreia=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,Diarreias_Diagnosticada,"SRO & Zinco",Pneumonia_Diagnosticada,Pneumonia_Tratada) %>%
     filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO, Nome_Us %in% US) 
   
   diarreias_tratadas_pivot <-tidyr::pivot_longer(
     data=dados_diarreia,
     cols=c("Diarreias_Diagnosticada","SRO & Zinco"),
     names_to = "diarreias_tratadas",
     values_to = "valores"
   )
   
   diagnostico_tratadas=diarreias_tratadas_pivot %>% group_by(diarreias_tratadas) %>%
     summarise(
       diagnos_trata=sum(valores)
     )
   
   grafico_tratadas= ggplot(data=diagnostico_tratadas,aes(y=diagnos_trata,x=diarreias_tratadas))+
     geom_col(fill="#99CCFF")+
     geom_text(aes(label=diagnos_trata),position =position_stack(vjust =1.1))+
     theme_bw()+
     xlab("")+
     ylab("")+
     theme(
       axis.text.y =element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )
   return(grafico_tratadas)
 }
 
 #Cartao de info de % de diarreias tratadas
 

 diarreia_box=function(ANO,MES,DISTRITO,US){
   dados_diarreias=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,Diarreias_Diagnosticada,"SRO & Zinco",Pneumonia_Diagnosticada,Pneumonia_Tratada)%>%
     filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
   
   diarreias_pivot <-tidyr::pivot_longer(
     data=dados_diarreias,
     cols=c("Diarreias_Diagnosticada","SRO & Zinco"),
     names_to = "dignostico_tratado",
     values_to = "valores"
   )
   
   diarreias=diarreias_pivot %>% group_by(dignostico_tratado) %>%
     summarise(
       tratadas_valor=sum(valores)
     ) %>%
     arrange(desc(tratadas_valor)) 
   
   dd=diarreias %>%
     mutate(
       pertg=diarreias[diarreias["dignostico_tratado"]=="SRO & Zinco","tratadas_valor"]/diarreias[diarreias["dignostico_tratado"]=="Diarreias_Diagnosticada","tratadas_valor"],
       dignostico_tratado=factor(x=dignostico_tratado,levels = dignostico_tratado)
     )
   
   tratadas_perctagem=dd[dd["dignostico_tratado"]=="SRO & Zinco","pertg"]*100
   tratadas_perctagem
   return(round(tratadas_perctagem,0))
 }
 #fim de cartao
 
 # Grafico de linha por tipo de tratamento de diarreias
 funcao_tipos_tratamento_diarreia("ERATI","CS Alua")

 funcao_tipos_tratamento_diarreia=function(DISTRITO,US){
   dados_mat1=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,"SRO & Zinco","So Zn","So SRO")%>%
     filter(Ano %in% c(2021,2022),Distrito %in% DISTRITO,Nome_Us %in% US)
   
   uterotorico_pivot <-tidyr::pivot_longer(
     data=dados_mat1,
     cols=c("SRO & Zinco","So Zn","So SRO"),
     names_to = "Tipos_tratamento",
     values_to = "valores"
   )
   
   sumario=uterotorico_pivot %>% group_by(periodo,Tipos_tratamento) %>%
     summarise(
       tratamentos=sum(valores),.groups = 'drop'
     )
   
   grafico_tiposdiarreias=ggplot(data=sumario,aes(x=periodo,y=tratamentos,col=factor(Tipos_tratamento)))+
     geom_line()+
     theme_bw()+
     xlab("")+
     ylab("Total de criancas 0-59 meses")+
     guides(color=guide_legend(title="Legenda"))+
     theme(
       axis.text.y =element_text(size = 7),
       #panel.grid.major = element_blank(),
       #panel.grid.minor = element_blank(),
       panel.border = element_blank(),
       legend.position = "bottom"
     )
   return(grafico_tiposdiarreias)
 }
 ?guides
 # grafico de barra entre pneumonia e amoxilina
 
   funcao_grafico_barra_pneumonia_amoxilina=function(ANO,MES,DISTRITO,US){
   dados_pneumonia=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,Diarreias_Diagnosticada,"SRO & Zinco",Pneumonia_Diagnosticada,Pneumonia_Tratada)%>%
     filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
    # filter(Ano==2021,Mes=="June",Distrito=="ERATI",Nome_Us=="CS Alua")
    
   pneumonia_pivot <-tidyr::pivot_longer(
     data=dados_pneumonia,
     cols=c("Pneumonia_Diagnosticada","Pneumonia_Tratada"),
     names_to = "diagnos_tratadas",
     values_to = "valores"
   )
   
   pneumo_sumario=pneumonia_pivot %>% group_by(diagnos_tratadas) %>%
     summarise(
       pneumo_tto=sum(valores)
     ) %>%
     arrange(desc(pneumo_tto))%>%
     mutate(diagnos_tratadas=factor(x=diagnos_tratadas,levels = diagnos_tratadas))
   
   #FFCC99
   #99CCFF
   graficobarra_pneumonia= ggplot(data=pneumo_sumario,aes(y=pneumo_tto,x=diagnos_tratadas))+
     geom_col(fill="#FFCC99")+
     geom_text(aes(label=pneumo_tto),position =position_stack(vjust =1.1))+
     theme_bw()+
     xlab("")+
     ylab("")+
     theme(
       axis.text.y =element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )
 return(graficobarra_pneumonia)
 }
 
 
 # box de info de % de amoxilina
  
   funcao_percentagem_amoxilina=function(ANO,MES,DISTRITO,US){
     dados_pneumonia=dados1 %>%
       select(Distrito,Nome_Us,periodo, Mes, Ano,Diarreias_Diagnosticada,"SRO & Zinco",Pneumonia_Diagnosticada,Pneumonia_Tratada)%>%
      filter(Ano==ANO,Mes %in% MES,Distrito %in% DISTRITO,Nome_Us %in% US)
      # filter(Ano==2021,Mes=="June",Distrito=="ERATI",Nome_Us=="CS Alua")
     
     pneumonia_pivot <-tidyr::pivot_longer(
       data=dados_pneumonia,
       cols=c("Pneumonia_Diagnosticada","Pneumonia_Tratada"),
       names_to = "diagnos_tratadas",
       values_to = "valores"
     )
     
     pneumo_sumario=pneumonia_pivot %>% group_by(diagnos_tratadas) %>%
       summarise(
         pneumo_tto=sum(valores)
       ) %>%
       arrange(desc(pneumo_tto))%>%
       mutate(diagnos_tratadas=factor(x=diagnos_tratadas,levels = diagnos_tratadas))
     
     tbl_amox=pneumo_sumario %>%
       mutate(
         pertg=pneumo_sumario[pneumo_sumario["diagnos_tratadas"]=="Pneumonia_Tratada","pneumo_tto"]/pneumo_sumario[pneumo_sumario["diagnos_tratadas"]=="Pneumonia_Diagnosticada","pneumo_tto"],
       #  Nados_cpp=factor(x=Nados_cpp,levels = Nados_cpp)
       )
     
     amoxilina_percentagem=tbl_amox[tbl_amox["diagnos_tratadas"]=="Pneumonia_Tratada","pertg"]*100
     
     return(round(amoxilina_percentagem,0))
   }
   
   
 # % de diarreias tratadas
   funcao_diarreiastratadas_distrito(2021,"June")
 
 funcao_diarreiastratadas_distrito=function(ANO,MES){
   dados_diarreias=dados1 %>%
     select(Distrito,Nome_Us,periodo, Mes, Ano,Diarreias_Diagnosticada,"SRO & Zinco",Pneumonia_Diagnosticada,Pneumonia_Tratada)%>%
     filter(Ano==ANO,Mes %in% MES)
     #filter(Ano==2021,Mes=="June")
   
   sumario_distrito=dados_diarreias %>% group_by(Distrito) %>%
     summarise(
       Diarreias_Diagnosticada=sum(Diarreias_Diagnosticada),
       Diarreias_Tratadas=sum(`SRO & Zinco`)
     ) %>%
     mutate(Perc_tratadas=(Diarreias_Tratadas/Diarreias_Diagnosticada)*100) %>%
     arrange(Perc_tratadas)%>%
     mutate(Distrito=factor(x=Distrito,levels = Distrito))
   
   grafico_diarr_tratadas_distrito=ggplot(sumario_distrito,aes(x=Perc_tratadas,y=Distrito))+
     geom_col(fill="#99CCFF")+
     theme_bw()+
     theme(
       axis.text.x = element_text(angle = 90, vjust = 0.8,size = 10),
       axis.text.y =element_text(vjust = 0.2,size=7),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()
     )+
     xlab("% de criancas 0-59 meses tratadas apenas com SRO & zinco")+
     ylab("")+
      scale_x_continuous(limits = c(0,120),
                         breaks = c(0,15,30,45,60,75,90,100,120),
                         labels = c("0%","15%","30%","45%","60%","75%","90%","100%","120%"))
   
   return(grafico_diarr_tratadas_distrito)
 }
 
 
 
 #Fim de saude infantil
 
 #theme(axis.text.x = element_text(angle = 90, vjust = .5))

 