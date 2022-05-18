library(shiny)
library(shinydashboard)
library(fresh)



# lista de meses
meschoices = c("Janeiro"="January", 
            "Fevereiro"="February",
            "Marco"="March",
            "Abril"="April",
            "Maio"= "May", 
            "Junho"= "June", 
            "Julho"= "July", 
            "Agosto"= "August",
            "Setembro"="September",
            "Outubro"="October",
            "Novembro"="November",
            "Dezembro"="December")
#lista de distritos
distritochoices=unique(dados1$Distrito)

#lista de unidades sanitarias


#uschoices=unique(select_mes())
#uschoices=unique(dados1$Nome_Us)
#uschoices=select_mes(input$distrito)



ui <- dashboardPage(
 
 #skin="green",
  header=dashboardHeader(),
  sidebar=dashboardSidebar(
    menuItem(
      text="Area Programatica",
      icon=icon("plus"),
      menuSubItem(text="CPN",tabName="cpn"),
      menuSubItem(text="SAUDE INFANTIL",tabName ="ccd"),
      menuSubItem(text="MATERNIDADE",tabName = "mat"),
      menuSubItem(text="RECEM-NASCIDOS",tabName ="rn"),
      menuSubItem(text="Mortes Maternas",tabName ="mm"),
      selected ="mat"
    ),
    
    #slider do ano
    sliderInput("Ano",
                "Ano:",
                min = 2018,
                max = 2022, 
                value = 2021,
                animate = TRUE,
                sep = ""),
    
    #slider do mes
    selectizeInput("mes", "Selecione o MES:",
                choices = meschoices,
                multiple = TRUE ),
    
    checkboxInput("all","Select all",value = TRUE),
  
  # slider de distrito
  selectizeInput("distrito", "Selecione o distrito:",
              choices =distritochoices,
              selected=list(c("ANGOCHE")),
              multiple = TRUE),
  checkboxInput("alll","Select all",value = FALSE),
  
  # slider de unidade samitaria
  selectizeInput("us", "Selecione a Unidade Sanitaria:",
              choices =NULL,
              multiple = TRUE),
     checkboxInput("allll","Select all",value = TRUE)
),
  
  body = dashboardBody(
   
    #D3D3D3
    #ECEBEE
    #FFFFFF
    tags$head(tags$style(
      HTML('
      .content-wrapper, .right-side {
                                background-color: #FFFFFF; 
                                }
           ')
    )),
    

    tabItems(
      tabItem(
        tabName ="cpn",
       # fluidRow(
       #   infoBoxOutput("c1",width = 4),
        #  infoBoxOutput("c2",width = 4),
        #  infoBoxOutput("c3",width = 4)
      #  ),
       
        fluidRow(
          column(width = 3,
                 box(plotlyOutput(outputId ="anc1",height = 300),width = NULL)),
          column(width = 3,
                 valueBoxOutput("menor12",width = 7)),
          column(width = 6,
                 box(title = "Meses com mais subida ou baixa de % de MG com IG menor ou igual a 12S",status = "primary",plotlyOutput(outputId = "anc2",height = 300),width = NULL))
          ),
      
      fluidRow(
        column(width = 3,
               box(plotlyOutput(outputId ="anc3",height = 300),width = NULL)),
        column(width = 3,
               valueBoxOutput("anc4",width = 7)),
               column(width = 6,
                      box(title = "Percentagens de gap na CPN",status = "primary",plotlyOutput(outputId = "anc5",height = 300),width = NULL)))
         ),
      
      tabItem(
        tabName ="rn",
         # fluidRow(
          #  infoBoxOutput("c4",width = 4),
           # infoBoxOutput("c5",width = 4),
            #infoBoxOutput("c6",width = 4)
         # ),
        fluidRow(
          column(width = 6,
                 box(title="Tendecia de % de cuidados essenciais ao RN",plotlyOutput(outputId ="aleitamento",height = 300),width = NULL)),
          column(width = 3,
                 box(title = "",status = "primary",plotlyOutput(outputId = "asfixia01",height = 300),width = NULL)),
          column(width = 3,
                 valueBoxOutput("asfixia_perc",width = 7))
        ),
        
        fluidRow(
          column(width = 6,
                 box(title="Distritos com mais ou baixa % de RN reanimados com sucessos",status="primary",plotlyOutput(outputId ="asfixia02",height = 300),width = NULL)),
          column(width = 6,
                 box(title = "Distribuicao % de Complicacoes neontais por causa",status = "primary",plotOutput(outputId = "asfixia03",height = 300),width = NULL)))
      ),
      
      tabItem(
        tabName ="mat",
        fluidRow(
          column(width = 3,
                 box(plotlyOutput(outputId = "parto1",height = 300),width = NULL)),
          column(width = 3,
                 valueBoxOutput("parto2",width = 7)), 
           column(width = 6,
                  box(title = "Meses com baixa ou subida % de partos com partograma preenchidos",status="primary",plotlyOutput(outputId = "parto3",height = 300),width = NULL))
        ),
  
      
        fluidRow(
          column(width = 3,
                 box(footer="Fonte:SISMA",plotlyOutput(outputId = "cpp1",height =300),width = NULL)),
          column(width = 3,
                 valueBoxOutput("cpp2",width = 7)), 
          column(width = 6,
                 box(title = "Tendencia de partos com partograma por mes",status = "primary",plotlyOutput(outputId = "cpp3",height = 300),width = NULL))
          )
      ),
      tabItem(
        tabName ="ccd",
        fluidRow(
          column(width = 3,
                 box(title="",plotlyOutput(outputId = "diarreia1",height = 300),width = NULL)),
          column(width = 3,
                 valueBoxOutput("perctgem_diarreia",width = 7)),
          column(width = 6,
                 box(title = "Tendencias de casos de diarreias por tipo",status="primary",plotlyOutput(outputId = "diarreia2",height = 300),width = NULL))
          
        ),
        
        fluidRow(
          column(width = 3,
                 box(footer="Fonte:SISMA-Ultima actualizacao: 05 de Maio 22",plotlyOutput(outputId = "pneumonia1",height =300),width = NULL)),
          column(width = 3,
                 valueBoxOutput("perctagem_pneumonia",width = 7)), 
          column(width = 6,
                 box(title = "Distritos com percentagens baixas ou altas no tratamento de diarreias com SRO e Zinco",status = "primary",plotlyOutput(outputId = "diarreia_distrito",height = 300),width = NULL))
        )
      ),
      
     tabItem(
      tabName = "mm",
      fluidRow(
      infoBoxOutput("c4",width = 2),
      infoBoxOutput("c5",width = 5),
      infoBoxOutput("c6",width = 5)
      ),
      fluidRow(
        column(width = 12,
      box(title="",plotlyOutput(outputId = "mm1",height = 300),width = NULL)),
    #  column(width = 3,
     #     valueBoxOutput("mm2",width = 7)),
    #  column(width = 6,
         #  box(title = "Tendencias de casos de diarreias por tipo",status="primary",plotlyOutput(outputId = "mm3",height = 300),width = NULL))
          
      ),
        
        
    fluidRow(
      column(width = 5,
                box(tableOutput(outputId = "mm4"),width = NULL)),
    #    column(width = 3,
      #       valueBoxOutput("mm5",width = 7)), 
       column(width = 7,
             box(title = "% de criancas com diarreias e tratadas apenas com SRO e Zinco",
                 status = "primary",
                  plotOutput(outputId = "mm6",height = 300),
                 width = NULL)))
  )
  )
  )
)


server <-function(input,output,session){
  output$g1 <-renderPlotly({
    ggplotly(
      ggplot(dados_cpn_ag_periodo)+
        geom_line(aes(x=periodo,y=ANC1),color="blue")+
        theme_bw()
    )

  })
  output$g2 <- renderPlotly({
    plot_ly(dados_cpn_ag_periodo,x=~periodo,y=~Perc_ANC4,type="bar")
  })
  output$g3 <- renderPlotly({
   ggplotly(ggplot(dados_cpn_ag_periodo)+
              geom_area(aes(x=periodo,y=Perc_Menor12Sem),fill="yellowgreen"))
  })
  output$g4 <- renderPlot({
    ggplot(data=dados_cp_ag_distrito3)+
      geom_col(aes(x=Perc_Menor12Sem,y=Distrito,fill="blue",alpha=0.5))
  })
  output$g5 <- renderPlotly({
    r=ggplot(data=dados_ccd_periodo)+
      geom_area(aes(x=periodo,y=Perc_diarreias_tratadas),fill="steelblue")+
      scale_color_viridis(discrete = T)+
      labs(x="Periodo",y="% de ccas diagnosticadas e tratadas com SRO e Zn")+
      theme_ipsum()
    ggplotly(r)
  })
  output$g6 <- renderPlotly({
   ggplotly(dados_ccd_distrito %>%
              arrange(Perc_diarreias_tratadas) %>%
              mutate(Distrito=factor(x=Distrito,levels = Distrito)) %>%
              ggplot(aes(x=Perc_diarreias_tratadas,y=Distrito))+
              geom_col(fill="steelblue")+
              theme_bw())
  })
  output$g7 <- renderPlotly({
    ggplotly(
      ggplot(data=dados_ccd_periodo)+
        geom_area(aes(x=periodo,y=Perc_pneumonia_tratada),fill="green4")+
        theme_bw()
    )
  })
  
  output$g8 <- renderPlotly({
    ggplotly(
      dados_ccd_distrito %>%
        arrange(Perc_pneumonia_tratada) %>%
        mutate(Distrito=factor(x=Distrito,levels = Distrito)) %>%
        ggplot(aes(x=Perc_pneumonia_tratada,y=Distrito))+
        geom_col(fill="tan1")+
        geom_text(aes(label=round(Perc_pneumonia_tratada,1)*100))+
        theme_bw()
    )
  })
    
  output$c1=renderInfoBox({
    infoBox(title="Numero de Obitos",value=200,subtitle="Mulheres",icon =icon("chart-bar"),color = "aqua")
  })
  output$c2=renderInfoBox({
    infoBox(title="Partos realizados",value=300,subtitle="Mulheres",icon =icon("users"),color = "red")
  })
  output$c3=renderInfoBox({
    infoBox(title="Numeros de distritos",value=200,subtitle="Nampula",icon =icon("chart-bar"),color = "yellow")
  })
  
  
  
  output$c7 =renderInfoBox({
    infoBox(title="Partos realizados",value=300,subtitle="Mulheres",icon =icon("users"),color = "red")
  })
  output$c8 =renderInfoBox({
    infoBox(title="Partos realizados",value=300,subtitle="Mulheres",icon =icon("users"),color = "red")
  })
  
  output$c9 =renderInfoBox({
    infoBox(title="Partos realizados",value=300,subtitle="Mulheres",icon =icon("users"),color = "red")
  })
  
  output$c10 =renderInfoBox({
    infoBox(title="Partos realizados",value=300,subtitle="Mulheres",icon =icon("users"),color = "red")
  })
  output$pc11 =renderInfoBox({
    infoBox(title="Partos realizados",value=300,subtitle="Mulheres",icon =icon("users"),color = "red")
  })
  # Maternidade
  
 # observe({
  #  if("select all " %in% input$mes)
  #    selected_choices=choices[-1]
 #   else
 #     selected_choices=input$mes
 #   updateSelectInput(session,"mes",selected = selected_choices)
#  })
  
  observe({
    updateSelectInput(
      session,"mes",choices=meschoices,
      selected=if(input$all) meschoices
    )
  })
  
  observe({
    updateSelectInput(
      session,"distrito",choices=distritochoices,
      selected=if(input$alll) distritochoices
    )
  })
  
  
 # observe({
 #   updateSelectInput(
   #   session,"us",choices=uschoices,
   #   selected=if(input$allll) uschoices
 #   )
 # })
  
  observe({
  # print(input$distrito)
    x=dados1 %>% filter(Distrito %in% input$distrito) %>%
      select(Nome_Us)
    updateSelectInput(session,"us",choices=unique(x),
    selected=if(input$allll) unique(dados1$Nome_Us) 
  )
  })
  
  
  
  output$parto1 =renderPlotly({
    
    ggplotly(grafico(input$Ano,input$mes,input$distrito,input$us))
  })
  
  output$parto2=renderValueBox({
    valueBox(
      value=paste(mat_box(ANO=input$Ano,MES=input$mes,DISTRITO=input$distrito,US=input$us),"%"),
      subtitle ="dos partos realizados fizeram manejo activo",
      icon=icon("bed"),
      color="blue",
      width = 4)

  })
  
  output$parto3 =renderPlotly({
    
    ggplotly(funcao_grafico_tendencia_partograma(input$distrito,input$us))
  })
  
  #RN
  output$aleitamento =renderPlotly({
    ggplotly(grafico_aleitamento_pele(input$distrito,input$us))
  })
  
  output$asfixia_perc=renderValueBox({
    valueBox(
      value=paste(funcao_percentagem_reanimacao(ANO=input$Ano,MES=input$mes,DISTRITO=input$distrito,US=input$us),"%"),
      subtitle ="dos RN com asfixia foram reanimados com sucessos",
      icon=icon("baby"),
      color="teal",
      width = 4)
    
  })
  
  output$asfixia01 =renderPlotly({
    
    ggplotly(funcao_grafico_barra_asfixia_reanimados(input$Ano,input$mes,input$distrito,input$us))
  })
  
  output$asfixia02 =renderPlotly({
    ggplotly(funcao_reanimados_distrito(input$Ano,input$mes))
  })
  
  output$asfixia03 =renderPlot({
    complicacoes_neonatais(input$Ano,input$mes,input$distrito,input$us)
  })
  
  #mortes maternas
 
  output$c5=renderInfoBox({
    infoBox(title="Complicacoes Obstetricas",value=Numero_complicacoes(input$Ano,input$mes,input$distrito,input$us),subtitle="Mulheres tiveram complicacoes directas",
            icon =icon("users"),color = "red")
  })
  output$c6=renderInfoBox({
    infoBox(title="Mortes Maternas",value=Numero_mm(input$Ano,input$mes,input$distrito,input$us),subtitle="mortes maternas devido a causas obstetricas directas",
            icon =icon("chart-bar"),color = "yellow")
  })
  
  output$mm1 =renderPlotly({
   causas_mm(input$Ano,input$mes,input$distrito,input$us)
  })
  
  output$mm4=renderTable({
    tabela_mm(input$Ano,input$mes)
  })
  
  output$mm6 =renderPlot({
    grafico_tendencia_mm(input$distrito,input$us)
  })
  
  #cpp
  output$cpp1 =renderPlotly({
    ggplotly(funcao_cpp(input$Ano,input$mes,input$distrito,input$us))
  })
  
  output$cpp2=renderValueBox({
    valueBox(
      value=paste(cpp_box(input$Ano,input$mes,input$distrito,input$us),"%"),
      subtitle ="dos RN fizeram consulta ate 2o dia",
      icon=icon("baby"),
      color="navy",
      width = 4)
  })
  
  output$cpp3 =renderPlotly({
    ggplotly(funcao_partos_tendencia(input$distrito,input$us))
  })
  
  # CPN
  # grafico de barra anc1 e menor 12
  output$anc1 =renderPlotly({
    ggplotly(grafico_menor12(input$Ano,input$mes,input$distrito,input$us))
  })
  
  output$menor12=renderValueBox({
    valueBox(
      value=paste(cpn_box(ANO=input$Ano,MES=input$mes,DISTRITO=input$distrito,US=input$us),"%"),
      subtitle ="de MG fizeram a 1a CPN com menor ou igual a 12 semanas",
      icon=icon("dress",lib ="glyphicon"),
      color="aqua",
      width = 4)
    
  })
  
  output$anc2 =renderPlotly({
    
    ggplotly(funcao_grafico_menor12_distrito(input$distrito,input$us))
  })
  
  # grafico de barra 1a cpn e 4a cpn
  output$anc3 =renderPlotly({
    ggplotly(funcao_cpn4(input$Ano,input$mes,input$distrito,input$us))
  })
  
  output$anc4=renderValueBox({
    valueBox(
      value=paste(anc4_box(input$Ano,input$mes,input$distrito,input$us),"%"),
      subtitle ="de MG fizeram 4a ou mais CPN",
      icon=icon("baby"),
      color="teal",
      width = 4)
  })
  
  output$anc5 =renderPlotly({
    ggplotly(funcao_gap_cpn(input$distrito,input$us))
  })
  #FIM de CPN
  
  
  #diarreias
   # grafico de barra
  output$diarreia1 =renderPlotly({
    ggplotly(graficobarra_diagnostico_tratado(ANO = input$Ano,MES=input$mes,DISTRITO = input$distrito,US=input$us))
    
  })
  
  # % de diarreias tratatadas
  output$perctgem_diarreia =renderValueBox({
    valueBox(
      value=paste(diarreia_box(ANO = input$Ano,MES=input$mes,DISTRITO = input$distrito,US=input$us),"%"),
      subtitle ="das criancas 0-59meses foram tratadas com apenas SRO e Zinco",
      icon=icon("baby"),
      color="aqua",
      width = 4)
  })
  
  # grafico de % de diarreias tratdas por distrito
  output$diarreia_distrito =renderPlotly({
    ggplotly(funcao_diarreiastratadas_distrito(ANO=input$Ano,MES=input$mes))

  })
  
  #
  output$diarreia2 =renderPlotly({
    ggplotly(funcao_tipos_tratamento_diarreia(DISTRITO = input$distrito,US=input$us))
    
  })
  
  # pneumonia
  # grafico de barra
  output$pneumonia1 =renderPlotly({
    ggplotly(funcao_grafico_barra_pneumonia_amoxilina(ANO = input$Ano,MES=input$mes,DISTRITO = input$distrito,US=input$us))
    
  })
  
  # % de pneumonia tratadas
  output$perctagem_pneumonia =renderValueBox({
    valueBox(
      value=paste(funcao_percentagem_amoxilina(ANO = input$Ano,MES=input$mes,DISTRITO = input$distrito,US=input$us),"%"),
      subtitle ="das criancas 0-59meses foram tratadas com amoxilina",
      icon=icon("baby"),
      color="orange",
      width = 4)
  })
}


shinyApp(ui,server)
