# # app.R ## lancio: 29 marzo      
library(shiny)
library(shinydashboard)
library(DT)
library(knitr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(rvest)
library(kableExtra)
library(deSolve)
library(EpiEstim)
library(incidence)
library(distcrete)
library(projections)
library(tidyr)
library(naniar)
library(purrr)
library(earlyR)
library(ggthemes)
library(readr)
library(RCurl)
#library(utils)
library(epitrix)
# library(httr)

ui <- dashboardPage(
    dashboardHeader(title = "epiRmolise"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Regione", tabName = "regione", icon = icon("stethoscope")),
            menuItem("Province", tabName = "province", icon = icon("ambulance")),
            menuItem("Analisi regione", tabName = "analisiregione", icon = icon("chart-bar")),
            menuItem("Analisi province", tabName = "analisiprovince", icon = icon("chart-bar")),
            menuItem("Informazioni", tabName = "info", icon = icon("user-md"))
            
        )
    ),
    
    
    ## Body content
    dashboardBody(
      tabItems( tabItem(tabName = "regione",  h1("COVID-19: la situazione in Molise", align="center"),
                        fluidPage(fluidRow(column(6, h3("Nelle ultime 24 ore:"),
                                                  DT::dataTableOutput("tabella_oggi")),
                                  
                                  column(6, h3("In totale dall'inizio dell'epidemia:"),
                                                  DT::dataTableOutput("tabella_totale")))),
                                  fluidPage(fluidRow(column(h3("Totale dei casi confermati in Molise:"),
                                    plotOutput(outputId = "incregplot", width= "100%", height = "300px"), width=8))),
                       fluidPage(fluidRow( textOutput("ultimo_aggiornamento1"), align="right")))
                                  , 
               
               #fluidPage(radioButtons('format', 'Document format', c( 'HTML', 'Word'),  #'PDF',
                       #                   inline = TRUE),
                  #           downloadButton('downloadReport')
               #)),
            # PROVINCE
          tabItem(tabName = "province",h1("COVID-19: la situazione in provincia", align="center"),
                    fluidPage(fluidRow(column(6, h3("Nuovi casi confermati:"),
                                              DT::dataTableOutput("tbl_nuovi_prov"))
                                     
                    ,column(6, h3("Totale dei casi confermati:"),
                                      DT::dataTableOutput("tbl_prov_tot")))),
                      
                      fluidPage(fluidRow(column(12,h3("Totale dei casi confermati nelle province molisane:")),
                      column(selectInput("sel", "Scegli la provincia:",
                                          choices = c("Campobasso", "Isernia")), width=8), 
                      column(plotOutput(outputId = "incprovplot", width= "100%", height = "300px"), width=8)
            )
            ),
            fluidPage(fluidRow( textOutput("ultimo_aggiornamento2"), align="right"))
          
            
          ),
                
                          ### ANALISI REGIONE molise
            tabItem(tabName = "analisiregione", h1("COVID-19: analisi regione Molise", align="center"),
                    fluidPage(fluidRow(column(12,h4("Modello di regressione: tasso di crescita, tempo di raddoppio in giorni"))),
                      fluidRow(column(6, verbatimTextOutput("early")),  column(6,box(width=12,div("Le analisi sono state effettuate con il pacchetto", a("incidence.", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6509961/")), h5("R0 è il numero totale di nuove infezioni causate da un singolo individuo contagioso durante l'intera durata del suo periodo contagioso, se consideriamo l'intera popolazione suscettibile."), "Fonte:", a("Klaus Krickeberg, Pham Van Trong, Pham Thi My Hanh; Epidemiology-Key to Public Health", href="https://www.springer.com/gp/book/9783030163679")))),
                             column(h4("Nuovi casi confermati giornalieri"), plotOutput(outputId = "earlyplot", width= "100%", height = "300px"), width=12),
                             fluidRow(column(h4("Andamento di R0 nel tempo"), plotOutput(outputId = "rzeroplot", width= "100%", height = "300px"), width=12)),
                          fluidRow(column(12,h4("Calcolo di R0"),verbatimTextOutput("rzero"))
                            
                             
                             )),
                    fluidPage(fluidRow( textOutput("ultimo_aggiornamento3"), align="right"))
                    
                    ),
                            
          



#### analisi province
          tabItem(tabName = "analisiprovince", h1("COVID-19: analisi province", align="center"),
                  fluidPage( fluidRow(  column(selectInput("selmod", "Scegli la provincia:",
                                     choices = c("Campobasso", "Isernia")), width=6),
                                       
                                     ),
                             fluidPage(fluidRow(column(12,h4("Modello di regressione: tasso di crescita, tempo di raddoppio in giorni"))),
               fluidRow(column(6,  verbatimTextOutput("earlyprov")),  column(6,box(width=12,div("Le analisi sono state effettuate con il pacchetto", a("incidence.")),div( href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6509961/"), h5("R0 è il numero totale di nuove infezioni causate da un singolo individuo contagioso durante l'intera durata del suo periodo contagioso, se consideriamo l'intera popolazione suscettibile."), "Fonte:", a("Klaus Krickeberg, Pham Van Trong, Pham Thi My Hanh; Epidemiology-Key to Public Health", href="https://www.springer.com/gp/book/9783030163679")))),
                        column(h4("Nuovi casi confermati giornalieri"), plotOutput(outputId = "earlyplot2", width= "100%", height = "300px"), width=12),
                        fluidRow(column(h4("Andamento di R0 nel tempo"), plotOutput(outputId = "rzeroplot2", width= "100%", height = "300px"), width=12)),
                                 fluidRow(column(12,h4("Calcolo di R0"),verbatimTextOutput("rzero2"))  )
                 )),
               fluidPage(fluidRow( textOutput("ultimo_aggiornamento4"), align="right"))
               
              
      ),

tabItem(tabName = "info", h1("EpiRmolise", align="center"),
        fluidPage(fluidRow(column(12,box("EpiRmolise è una piattaforma aggiornata giornalmente con i", a("dati del Dipartimento della Protezione Civile", href="https://github.com/pcm-dpc/COVID-19"), "realizzata dal",
                                         a("dr. Carlo Piparo", href="https://it.linkedin.com/in/carlo-piparo-05a13a178"),
                                         "utilizzando i pacchetti prodotti da", a("RECON.", href="https://www.repidemicsconsortium.org/"),
                                         div("Quest'opera è distribuita con Licenza", a("Creative Commons Attribuzione 3.0 Italia.", href="https://creativecommons.org/licenses/by/3.0/it/"),
                                             "No warranties.",shiny::div(textOutput("ultimo_aggiornamento5"))),width=12))), 
                                      ))

                                             
           
               
               
  
    
                                

                    )
            
    
    )
)


#column(dateRangeInput('dateRange',
                #      label = paste('Filtra per data:'),
                 #     start = NULL, end = NULL,
                 #     separator = " - ", format = "yyyy-mm-dd",
                 #     startview = 'year', language = 'it'), width = 6) 



  

server <- function(input, output) {


      ###dati regione###
    dati_it <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
    
    dati_it <- read.csv(dati_it) 
    
    molise <- dati_it %>% filter(denominazione_regione=="Molise")
    
    ###dati province###
    
    dati_prov<- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"
    
    dati_prov <- read.csv(dati_prov)
    dati_prov <- dati_prov %>% filter(denominazione_regione=="Molise")
    
    ###dati province  latest###
    
    dati_prov_lat <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-latest.csv"
    
    dati_prov_lat <- read.csv(dati_prov_lat)
    dati_prov_lat <- dati_prov_lat %>% filter(denominazione_regione=="Molise")
      
    
    ### source
    source("incidenza.R")
    
    output$ultimo_aggiornamento1 <- renderText({ ultima_data<- tail(as.Date(molise$data),n=1) 
    
ultima_data<- as.character( ultima_data)
paste("ultimo aggiornamento:", ultima_data)
})
    
    output$ultimo_aggiornamento2 <- renderText({ ultima_data<- tail(as.Date(molise$data),n=1) 
    
    ultima_data<- as.character( ultima_data)
    paste("ultimo aggiornamento:", ultima_data)
    })
    
    
    output$ultimo_aggiornamento3 <- renderText({ ultima_data<- tail(as.Date(molise$data),n=1) 
    
    ultima_data<- as.character( ultima_data)
    paste("ultimo aggiornamento:", ultima_data)
    })
    
    
    
    output$ultimo_aggiornamento4 <- renderText({ ultima_data<- tail(as.Date(molise$data),n=1) 
    
    ultima_data<- as.character( ultima_data)
    paste("ultimo aggiornamento:", ultima_data)
    })
    
    output$ultimo_aggiornamento5 <- renderText({ ultima_data<- tail(as.Date(molise$data),n=1) 
    
    ultima_data<- as.character( ultima_data)
    paste("Ultimo aggiornamento:", ultima_data)
    })
    ####### REPORT RMARKDOWN ####### start
  #  output$markdown <- renderUI({  
  #      rmarkdown::render(input = "descrittiva_molise.Rmd",
    #                      output_format = "html_document",
 #                             output_file = 'descrittiva_molise.html')
  #      shiny::includeHTML('descrittiva_molise.html') 
        
  #  }) 
    
    ####### REPORT RMARKDOWN ####### end
    
    ######### analisi regione ###### start
    
    tamponi  <- tail(molise$tamponi, n=1)
    tamponi_oggi <- tail(diff(molise$tamponi), n=1)
    
    nuovi_positivi <-  tail(molise$nuovi_positivi, n=1)
    nuovi_positivi_oggi <-  tail(diff(molise$nuovi_positivi), n=1)
    
    totale_positivi <- tail(molise$totale_positivi, n=1)
    totale_positivi_oggi <- tail(diff(molise$totale_positivi), n=1)
    
    isolamento_domiciliare  <- tail(molise$isolamento_domiciliare, n=1)
    isolamento_domiciliare_oggi <- tail(diff(molise$isolamento_domiciliare), n=1)
    
    totale_ospedalizzati  <- tail(molise$totale_ospedalizzati, n=1)
    totale_ospedalizzati_oggi <- tail(diff(molise$totale_ospedalizzati), n=1)
    
    ricoverati_con_sintomi  <- tail(molise$ricoverati_con_sintomi, n=1)
    ricoverati_con_sintomi_oggi <- tail(diff(molise$ricoverati_con_sintomi), n=1)
    
    terapia_intensiva  <- tail(molise$terapia_intensiva, n=1)
    terapia_intensiva_oggi <- tail(diff(molise$terapia_intensiva), n=1)
    
    deceduti  <- tail(molise$deceduti, n=1)
    deceduti_oggi <- tail(diff(molise$deceduti), n=1)
    
    dimessi_guariti  <- tail(molise$dimessi_guariti, n=1)
    dimessi_guariti_oggi <- tail(diff(molise$dimessi_guariti), n=1)
    
    totale_casi  <- tail(molise$totale_casi, n=1)
    
    
    #tabella oggi 
    df2 <- data.frame(tamponi_oggi, nuovi_positivi, isolamento_domiciliare_oggi, ricoverati_con_sintomi_oggi, terapia_intensiva_oggi,  dimessi_guariti_oggi, deceduti_oggi )
    names(df2) <- c("tamponi", "nuovi positivi","isolamento domiciliare", "ricoverati con sintomi", "terapia intensiva", "guariti","deceduti")
    row.names(df2) <- c("oggi")
   df2 <- t(df2)
     #tabella totale    
     df1 <- data.frame(tamponi, totale_positivi, isolamento_domiciliare, ricoverati_con_sintomi, terapia_intensiva, dimessi_guariti,deceduti)
    names(df1) <- c("tamponi", "positivi", "isolamento domiciliare", "ricoverati con sintomi", "terapia intensiva", "guariti","deceduti")
    row.names(df1) <- c("totale")
    df1 <- t(df1)
    
 
    
  
    #output oggi
    
    output$tabella_oggi <- renderDataTable(df2,options = list(dom='t', lengthChange = FALSE, autoHideNavigation=TRUE))
    
    
    #output totale
    output$tabella_totale <- renderDataTable(df1,options = list(dom='t', lengthChange = FALSE, autoHideNavigation=TRUE))
    
    #output guariti/deceduti
    
    output$tabella_deceduti_oggi <- renderDataTable(df3,options = list(dom='t', lengthChange = FALSE, autoHideNavigation=TRUE))
    output$tabella_deceduti <- renderDataTable(df4,options = list(dom='t', lengthChange = FALSE, autoHideNavigation=TRUE))
    
    
  #### grafico incidenza giornaliera
    
    library(lubridate)
    sic_date<- as.Date(molise$data,format="%Y-%m-%d")
    molise <- molise %>% mutate(date=sic_date)
    
    
    output$incregplot <- renderPlot({
          ggplot(data= molise,  aes(x=date, totale_casi)) + xlab("Data")+ ylab("Totale dei casi confermati") + geom_point(color='red3') + geom_line(color='red3') + theme(legend.position = "none", 
                                                                                                                  strip.text.y = element_text(size=11))
    })

  
   
  
    
    ###### ANALISI REGIONE #### end
    
    
 
    
    #### TABELLA CASI TOTALI PROVINCE ####
    
    provincia <- dati_prov_lat$denominazione_provincia
    totale_casi <- dati_prov_lat$totale_casi
    
tabella_casi_totali_province <- data.frame(provincia, totale_casi)
colnames(tabella_casi_totali_province) <- c("provincia", "totale casi")

#output
    output$tbl_prov_tot <- renderDataTable(tabella_casi_totali_province,options = list(dom='t',pageLength = 10, lengthChange = FALSE))
    
    
    
   ### TABELLA NUOVI CASI PROVINCE ####
    
    
   last_day<- as.Date(tail(dati_prov$data,n=1))   #ultimo giorno ottenuto dal databe regioni
    yesterday <- format(last_day -1,"%Y/%m/%d") #scelgo il giorno precedente
    dati_prov$data <- as.Date(dati_prov$data)
    
        
    dati_prov_ieri    <- dati_prov %>% filter( data==yesterday)
    dati_prov_ieri <- data.frame(dati_prov_ieri$denominazione_provincia, dati_prov_ieri$totale_casi)
    nomi_province <- arrange(dati_prov_ieri, dati_prov_ieri.denominazione_provincia) %>% select(dati_prov_ieri.denominazione_provincia)
        dati_prov_ieri <- arrange(dati_prov_ieri, dati_prov_ieri.denominazione_provincia) %>% select(dati_prov_ieri.totale_casi)
    
    dati_prov_oggi <- dati_prov %>% filter( data==last_day)
    dati_prov_oggi <- data.frame(dati_prov_oggi$denominazione_provincia, dati_prov_oggi$totale_casi)
    dati_prov_oggi <- arrange(dati_prov_oggi, dati_prov_oggi.denominazione_provincia) %>% select(dati_prov_oggi.totale_casi)
    
    dati_prov_last2 <- data.frame(dati_prov_ieri, dati_prov_oggi)

    ieri<- as.numeric(dati_prov_last2$dati_prov_ieri.totale_casi)
    oggi<- as.numeric(dati_prov_last2$dati_prov_oggi.totale_casi)
    nuovi_casi <- oggi-ieri
    
    
    tbl_nuovi_prov <- data.frame(nomi_province, nuovi_casi)
    colnames(tbl_nuovi_prov) <- c("provincia", "nuovi casi")
    
    #output
    
    
    output$tbl_nuovi_prov <- renderDataTable(tbl_nuovi_prov,options = list(dom='t',pageLength = 10, lengthChange = FALSE))
    
    
    
    
    #### PROVINCE - INCIDENZA CUMULATIVA GIORNALIERA #### start
    
    
    prov_date<- as.Date(dati_prov$data,format="%Y-%m-%d")
    dati_prov <- dati_prov %>% mutate(data=prov_date)
     totale_casi <- as.numeric(as.vector(dati_prov$totale_casi))
    

    
    
    
    output$incprovplot <- renderPlot({
      dati_prov<- switch(input$sel, 
                              "Campobasso" =  filter(dati_prov, denominazione_provincia=="Campobasso"),
                         "Isernia" =  filter(dati_prov, denominazione_provincia=="Isernia")
                         )
                    
                ggplot( data= dati_prov,aes(x=data, totale_casi)) + xlab("Data")+ ylab("Totale dei casi confermati") + geom_point(color='red3') + geom_line(color='red3')+ theme(legend.position = "none", strip.text.y = element_text(size=11))
                    
                          })
    
    
    #### PROVINCE - INCIDENZA #### end
    
    ##### INCIDENCE ##### start
    #ottengo le date per costruire l'incidence object
    molise$nuovi_positivi <- as.numeric(as.vector(molise$nuovi_positivi))
    molise <- molise %>% filter(nuovi_positivi>=0)
    
    
    date_sic <- uncount(molise, weights = nuovi_positivi, .remove = FALSE)
    
    date_sic$data <- ymd_hms(date_sic$data)
    
    # fitto incidence
    inc_obj <- incidence(date_sic$data)
    early.fit <- fit(inc_obj)
   
    plot(early.fit)
    
    
   

    output$early <- renderPrint({ 
   
    print(early.fit)
      
    })
    
    ### grafico early fit (incidence)
 
    
    ###### INCIDENCE ##### end
    
    
    #### incidenza giornaliera regione ####  start
    onset <- date_sic$data
    
    today <- today()
    
    i <- incidence(onset, last_date=today)
    
    
    i$dates <- as.Date(i$dates)


    
    
    output$earlyplot <- renderPlot({
      plot(i, color = "red", border = "white") 
    })
    
    
    
    
    #### incidenza giornaliera regione ####   end
    
    ##### EpiEstim ###### start
    
    i$dates <- as.Date(i$dates)

    
    res_parametric_si <- estimate_R(i, 
                                    method="parametric_si",
                                    config = make_config(list(
                                      mean_si = 3.96, 
                                      std_si = 4.75))
    )

    
    
    output$rzeroplot <- renderPlot({
    plot(res_parametric_si, legend = FALSE, "R")
    
     })
    
    ##### EpiEstim ###### end
    
    ### calcolo R0 #### start
    
    
    output$rzero <- renderPrint({ 
      
      print(res_parametric_si)
      
    })
    
    
    ### calcolo R0 #### end
    
    ############################################# INCIDENCE PROVINCE #################### START
    
    
          
    
    
    
    output$earlyprov <- renderPrint({ 
      dati_prov$data <- as.Date(dati_prov$data,format="%Y-%m-%d")
      
      #dati_prov <- dati_prov %>% filter(data >= input$dateRange[1] & data <= input$dateRange[2])
      
    dati_prov  <-  switch(input$selmod, 
                         "Campobasso" =  filter(dati_prov, denominazione_provincia=="Campobasso"),
                         "Isernia" =  filter(dati_prov, denominazione_provincia=="Isernia")
                         )
    
    "2020-04-07"<- nuovi_casi(dati_prov, "2020-04-06", "2020-04-07")
    "2020-04-06"<- nuovi_casi(dati_prov, "2020-04-05", "2020-04-06")
    "2020-04-05"<- nuovi_casi(dati_prov, "2020-04-04", "2020-04-05")
    "2020-04-04"<- nuovi_casi(dati_prov, "2020-04-03", "2020-04-04")
    "2020-04-03"<- nuovi_casi(dati_prov, "2020-04-02", "2020-04-03")
    "2020-04-02"<- nuovi_casi(dati_prov, "2020-04-01", "2020-04-02")
    "2020-04-01"<- nuovi_casi(dati_prov, "2020-03-31", "2020-04-01")
    "2020-03-31"<- nuovi_casi(dati_prov, "2020-03-30", "2020-03-31")
    "2020-03-30"<- nuovi_casi(dati_prov, "2020-03-29", "2020-03-30")
    "2020-03-29"<- nuovi_casi(dati_prov, "2020-03-28", "2020-03-29")
    "2020-03-28"<- nuovi_casi(dati_prov, "2020-03-27", "2020-03-28")
    "2020-03-27"<- nuovi_casi(dati_prov, "2020-03-26", "2020-03-27")
    "2020-03-26"<- nuovi_casi(dati_prov, "2020-03-25", "2020-03-26")
    "2020-03-25"<- nuovi_casi(dati_prov, "2020-03-24", "2020-03-25")
    "2020-03-24"<- nuovi_casi(dati_prov, "2020-03-23", "2020-03-24")
    "2020-03-23"<- nuovi_casi(dati_prov, "2020-03-22", "2020-03-23")
    "2020-03-22"<- nuovi_casi(dati_prov, "2020-03-21", "2020-03-22")
    "2020-03-21"<- nuovi_casi(dati_prov, "2020-03-20", "2020-03-21")
    "2020-03-20"<- nuovi_casi(dati_prov, "2020-03-19", "2020-03-20")
    "2020-03-19"<- nuovi_casi(dati_prov, "2020-03-18", "2020-03-19")
    "2020-03-18"<- nuovi_casi(dati_prov, "2020-03-17", "2020-03-18")
    "2020-03-17"<- nuovi_casi(dati_prov, "2020-03-16", "2020-03-17")
    "2020-03-16"<- nuovi_casi(dati_prov, "2020-03-15", "2020-03-16")
    "2020-03-15"<- nuovi_casi(dati_prov, "2020-03-14", "2020-03-15")
    "2020-03-14"<- nuovi_casi(dati_prov, "2020-03-13", "2020-03-14")
    "2020-03-13"<- nuovi_casi(dati_prov, "2020-03-12", "2020-03-13")
    "2020-03-12"<- nuovi_casi(dati_prov, "2020-03-11", "2020-03-12")
    "2020-03-11"<- nuovi_casi(dati_prov, "2020-03-10", "2020-03-11")
    "2020-03-10"<- nuovi_casi(dati_prov, "2020-03-09", "2020-03-10")
    "2020-03-09"<- nuovi_casi(dati_prov, "2020-03-08", "2020-03-09")
    "2020-03-08"<- nuovi_casi(dati_prov, "2020-03-07", "2020-03-08")
    "2020-03-07"<- nuovi_casi(dati_prov, "2020-03-06", "2020-03-07")
    "2020-03-06"<- nuovi_casi(dati_prov, "2020-03-05", "2020-03-06")
    "2020-03-05"<- nuovi_casi(dati_prov, "2020-03-04", "2020-03-05")
    "2020-03-04"<- nuovi_casi(dati_prov, "2020-03-03", "2020-03-04")
    "2020-03-03"<- nuovi_casi(dati_prov, "2020-03-02", "2020-03-03")
    "2020-03-02"<- nuovi_casi(dati_prov, "2020-03-01", "2020-03-02")
    "2020-03-01"<- nuovi_casi(dati_prov, "2020-02-29", "2020-03-01")
    "2020-02-29"<- nuovi_casi(dati_prov, "2020-02-28", "2020-02-29")
    "2020-02-28"<- nuovi_casi(dati_prov, "2020-02-27", "2020-02-28")
    "2020-02-27"<- nuovi_casi(dati_prov, "2020-02-26", "2020-02-27")
    "2020-02-26"<- nuovi_casi(dati_prov, "2020-02-25", "2020-02-26")
    "2020-02-25"<- nuovi_casi(dati_prov, "2020-02-24", "2020-02-25")
    
    
    
    incidenza_prov <- data.frame(
      `2020-04-07`,
      `2020-04-06`,
      `2020-04-05`,
      `2020-04-04`,
      `2020-04-03`,
      `2020-04-02`,
      `2020-04-01`,
      `2020-03-31`,
      `2020-03-30`,
      `2020-03-29`,
      `2020-03-28`,
      `2020-03-27`,
      `2020-03-26`,
      `2020-03-25`,
      `2020-03-24`,
      `2020-03-23`,
      `2020-03-22`,
      `2020-03-21`,
      `2020-03-20`,
      `2020-03-19`,
      `2020-03-18`,
      `2020-03-17`,
      `2020-03-16`,
      `2020-03-15`,
      `2020-03-14`,
      `2020-03-13`,
      `2020-03-12`,
      `2020-03-11`,
      `2020-03-10`,
      `2020-03-09`,
      `2020-03-08`,
      `2020-03-07`,
      `2020-03-06`,
      `2020-03-05`,
      `2020-03-04`,
      `2020-03-03` )
    
    
    
    date_inc_prov <- c(
      "2020-04-07",
      "2020-04-06",
      "2020-04-05",
      "2020-04-04",
      "2020-04-03",
      "2020-04-02",
      "2020-04-01",
      "2020-03-31",
      "2020-03-30",
      "2020-03-29",
      "2020-03-28",
      "2020-03-27",
      "2020-03-26",
      "2020-03-25",
      "2020-03-24",
      "2020-03-23",
      "2020-03-22",
      "2020-03-21",
      "2020-03-20",
      "2020-03-19",
      "2020-03-18",
      "2020-03-17",
      "2020-03-16",
      "2020-03-15",
      "2020-03-14",
      "2020-03-13",
      "2020-03-12",
      "2020-03-11",
      "2020-03-10",
      "2020-03-09",
      "2020-03-08",
      "2020-03-07",
      "2020-03-06",
      "2020-03-05",
      "2020-03-04",
      "2020-03-03" 
    )
    
    
    incidenza_prov <- t(incidenza_prov)
    incidenza_prov <- as.numeric(incidenza_prov)
    date_inc_prov <- as.Date(date_inc_prov)
    df_inc_prov <- data.frame(date_inc_prov,incidenza_prov)
    df_inc_prov <- filter(df_inc_prov, incidenza_prov>=0)
    
      
    date_prov <- uncount(df_inc_prov, weights = incidenza_prov, .remove = FALSE)
    
    date_incidenza_prov <- date_prov$date_inc_prov
    inc_obj <- incidence(date_incidenza_prov)
    early.fit <- fit(inc_obj)
    
    print(early.fit)
    
    
  
       
      
    })
    

    

    
    
    
    
    
    
    
    ############################################# INCIDENCE PROVINCE #################### END
    
    
    
    
    ####### INCIDENZA GIORNALIERA PROVINCE ###### START
   
    output$earlyplot2 <- renderPlot({
      
    
     dati_prov  <-  switch(input$selmod, 
                          "Campobasso" =  filter(dati_prov, denominazione_provincia=="Campobasso"),
                          "Isernia" =  filter(dati_prov, denominazione_provincia=="Isernia")
    )
     
     "2020-04-07"<- nuovi_casi(dati_prov, "2020-04-06", "2020-04-07")
     "2020-04-06"<- nuovi_casi(dati_prov, "2020-04-05", "2020-04-06")
     "2020-04-05"<- nuovi_casi(dati_prov, "2020-04-04", "2020-04-05")
     "2020-04-04"<- nuovi_casi(dati_prov, "2020-04-03", "2020-04-04")
     "2020-04-03"<- nuovi_casi(dati_prov, "2020-04-02", "2020-04-03")
     "2020-04-02"<- nuovi_casi(dati_prov, "2020-04-01", "2020-04-02")
     "2020-04-01"<- nuovi_casi(dati_prov, "2020-03-31", "2020-04-01")
     "2020-03-31"<- nuovi_casi(dati_prov, "2020-03-30", "2020-03-31")
     "2020-03-30"<- nuovi_casi(dati_prov, "2020-03-29", "2020-03-30")
     "2020-03-29"<- nuovi_casi(dati_prov, "2020-03-28", "2020-03-29")
     "2020-03-28"<- nuovi_casi(dati_prov, "2020-03-27", "2020-03-28")
     "2020-03-27"<- nuovi_casi(dati_prov, "2020-03-26", "2020-03-27")
     "2020-03-26"<- nuovi_casi(dati_prov, "2020-03-25", "2020-03-26")
     "2020-03-25"<- nuovi_casi(dati_prov, "2020-03-24", "2020-03-25")
     "2020-03-24"<- nuovi_casi(dati_prov, "2020-03-23", "2020-03-24")
     "2020-03-23"<- nuovi_casi(dati_prov, "2020-03-22", "2020-03-23")
     "2020-03-22"<- nuovi_casi(dati_prov, "2020-03-21", "2020-03-22")
     "2020-03-21"<- nuovi_casi(dati_prov, "2020-03-20", "2020-03-21")
     "2020-03-20"<- nuovi_casi(dati_prov, "2020-03-19", "2020-03-20")
     "2020-03-19"<- nuovi_casi(dati_prov, "2020-03-18", "2020-03-19")
     "2020-03-18"<- nuovi_casi(dati_prov, "2020-03-17", "2020-03-18")
     "2020-03-17"<- nuovi_casi(dati_prov, "2020-03-16", "2020-03-17")
     "2020-03-16"<- nuovi_casi(dati_prov, "2020-03-15", "2020-03-16")
     "2020-03-15"<- nuovi_casi(dati_prov, "2020-03-14", "2020-03-15")
     "2020-03-14"<- nuovi_casi(dati_prov, "2020-03-13", "2020-03-14")
     "2020-03-13"<- nuovi_casi(dati_prov, "2020-03-12", "2020-03-13")
     "2020-03-12"<- nuovi_casi(dati_prov, "2020-03-11", "2020-03-12")
     "2020-03-11"<- nuovi_casi(dati_prov, "2020-03-10", "2020-03-11")
     "2020-03-10"<- nuovi_casi(dati_prov, "2020-03-09", "2020-03-10")
     "2020-03-09"<- nuovi_casi(dati_prov, "2020-03-08", "2020-03-09")
     "2020-03-08"<- nuovi_casi(dati_prov, "2020-03-07", "2020-03-08")
     "2020-03-07"<- nuovi_casi(dati_prov, "2020-03-06", "2020-03-07")
     "2020-03-06"<- nuovi_casi(dati_prov, "2020-03-05", "2020-03-06")
     "2020-03-05"<- nuovi_casi(dati_prov, "2020-03-04", "2020-03-05")
     "2020-03-04"<- nuovi_casi(dati_prov, "2020-03-03", "2020-03-04")
     "2020-03-03"<- nuovi_casi(dati_prov, "2020-03-02", "2020-03-03")
     "2020-03-02"<- nuovi_casi(dati_prov, "2020-03-01", "2020-03-02")
     "2020-03-01"<- nuovi_casi(dati_prov, "2020-02-29", "2020-03-01")
     "2020-02-29"<- nuovi_casi(dati_prov, "2020-02-28", "2020-02-29")
     "2020-02-28"<- nuovi_casi(dati_prov, "2020-02-27", "2020-02-28")
     "2020-02-27"<- nuovi_casi(dati_prov, "2020-02-26", "2020-02-27")
     "2020-02-26"<- nuovi_casi(dati_prov, "2020-02-25", "2020-02-26")
     "2020-02-25"<- nuovi_casi(dati_prov, "2020-02-24", "2020-02-25")
     
     
     
     incidenza_prov <- data.frame(
       `2020-04-07`,
       `2020-04-06`,
       `2020-04-05`,
       `2020-04-04`,
       `2020-04-03`,
       `2020-04-02`,
       `2020-04-01`,
       `2020-03-31`,
       `2020-03-30`,
       `2020-03-29`, 
       `2020-03-28`,
       `2020-03-27`,
       `2020-03-26`,
       `2020-03-25`,
       `2020-03-24`,
       `2020-03-23`,
       `2020-03-22`,
       `2020-03-21`,
       `2020-03-20`,
       `2020-03-19`,
       `2020-03-18`,
       `2020-03-17`,
       `2020-03-16`,
       `2020-03-15`,
       `2020-03-14`,
       `2020-03-13`,
       `2020-03-12`,
       `2020-03-11`,
       `2020-03-10`,
       `2020-03-09`,
       `2020-03-08`,
       `2020-03-07`,
       `2020-03-06`,
       `2020-03-05`,
       `2020-03-04`,
       `2020-03-03` )
     
     
     
     date_inc_prov <- c(
       "2020-04-07",
       "2020-04-06",
       "2020-04-05",
       "2020-04-04",
       "2020-04-03",
       "2020-04-02",
       "2020-04-01",
       "2020-03-31",
       "2020-03-30",
       "2020-03-29",
       "2020-03-28",
       "2020-03-27",
       "2020-03-26",
       "2020-03-25",
       "2020-03-24",
       "2020-03-23",
       "2020-03-22",
       "2020-03-21",
       "2020-03-20",
       "2020-03-19",
       "2020-03-18",
       "2020-03-17",
       "2020-03-16",
       "2020-03-15",
       "2020-03-14",
       "2020-03-13",
       "2020-03-12",
       "2020-03-11",
       "2020-03-10",
       "2020-03-09",
       "2020-03-08",
       "2020-03-07",
       "2020-03-06",
       "2020-03-05",
       "2020-03-04",
       "2020-03-03" 
     )
     
     
     incidenza_prov <- t(incidenza_prov)
     incidenza_prov <- as.numeric(incidenza_prov)
     date_inc_prov <- as.Date(date_inc_prov)
     df_inc_prov <- data.frame(date_inc_prov,incidenza_prov)
     df_inc_prov <- filter(df_inc_prov, incidenza_prov>=0)
     
     
     date_prov <- uncount(df_inc_prov, weights = incidenza_prov, .remove = FALSE)
     
    onset <- date_prov$date_inc_prov
     
   
    
    today <- today()
    
    i <- incidence(onset, last_date=today)
    
  
    
    
    #epiestim start per definire R della regione
    i$dates <- as.Date(i$dates)

    
   plot(i, color = "red", border = "white")
    })
    
    

    
    
    
    
    ####### INCIDENZA GIORNALIERA PROVINCE ###### END
    
    ######## PLOT R0 PROVINCE ###### START
    
    output$rzeroplot2 <- renderPlot({
      
      dati_prov  <-  switch(input$selmod, 
                            "Campobasso" =  filter(dati_prov, denominazione_provincia=="Campobasso"),
                            "Isernia" =  filter(dati_prov, denominazione_provincia=="Isernia")    )
      
      "2020-04-07"<- nuovi_casi(dati_prov, "2020-04-06", "2020-04-07")
      "2020-04-06"<- nuovi_casi(dati_prov, "2020-04-05", "2020-04-06")
      "2020-04-05"<- nuovi_casi(dati_prov, "2020-04-04", "2020-04-05")
      "2020-04-04"<- nuovi_casi(dati_prov, "2020-04-03", "2020-04-04")
      "2020-04-03"<- nuovi_casi(dati_prov, "2020-04-02", "2020-04-03")
      "2020-04-02"<- nuovi_casi(dati_prov, "2020-04-01", "2020-04-02")
      "2020-04-01"<- nuovi_casi(dati_prov, "2020-03-31", "2020-04-01")
      "2020-03-31"<- nuovi_casi(dati_prov, "2020-03-30", "2020-03-31")
      "2020-03-30"<- nuovi_casi(dati_prov, "2020-03-29", "2020-03-30")
      "2020-03-29"<- nuovi_casi(dati_prov, "2020-03-28", "2020-03-29")
      "2020-03-28"<- nuovi_casi(dati_prov, "2020-03-27", "2020-03-28")
      "2020-03-27"<- nuovi_casi(dati_prov, "2020-03-26", "2020-03-27")
      "2020-03-26"<- nuovi_casi(dati_prov, "2020-03-25", "2020-03-26")
      "2020-03-25"<- nuovi_casi(dati_prov, "2020-03-24", "2020-03-25")
      "2020-03-24"<- nuovi_casi(dati_prov, "2020-03-23", "2020-03-24")
      "2020-03-23"<- nuovi_casi(dati_prov, "2020-03-22", "2020-03-23")
      "2020-03-22"<- nuovi_casi(dati_prov, "2020-03-21", "2020-03-22")
      "2020-03-21"<- nuovi_casi(dati_prov, "2020-03-20", "2020-03-21")
      "2020-03-20"<- nuovi_casi(dati_prov, "2020-03-19", "2020-03-20")
      "2020-03-19"<- nuovi_casi(dati_prov, "2020-03-18", "2020-03-19")
      "2020-03-18"<- nuovi_casi(dati_prov, "2020-03-17", "2020-03-18")
      "2020-03-17"<- nuovi_casi(dati_prov, "2020-03-16", "2020-03-17")
      "2020-03-16"<- nuovi_casi(dati_prov, "2020-03-15", "2020-03-16")
      "2020-03-15"<- nuovi_casi(dati_prov, "2020-03-14", "2020-03-15")
      "2020-03-14"<- nuovi_casi(dati_prov, "2020-03-13", "2020-03-14")
      "2020-03-13"<- nuovi_casi(dati_prov, "2020-03-12", "2020-03-13")
      "2020-03-12"<- nuovi_casi(dati_prov, "2020-03-11", "2020-03-12")
      "2020-03-11"<- nuovi_casi(dati_prov, "2020-03-10", "2020-03-11")
      "2020-03-10"<- nuovi_casi(dati_prov, "2020-03-09", "2020-03-10")
      "2020-03-09"<- nuovi_casi(dati_prov, "2020-03-08", "2020-03-09")
      "2020-03-08"<- nuovi_casi(dati_prov, "2020-03-07", "2020-03-08")
      "2020-03-07"<- nuovi_casi(dati_prov, "2020-03-06", "2020-03-07")
      "2020-03-06"<- nuovi_casi(dati_prov, "2020-03-05", "2020-03-06")
      "2020-03-05"<- nuovi_casi(dati_prov, "2020-03-04", "2020-03-05")
      "2020-03-04"<- nuovi_casi(dati_prov, "2020-03-03", "2020-03-04")
      "2020-03-03"<- nuovi_casi(dati_prov, "2020-03-02", "2020-03-03")
      "2020-03-02"<- nuovi_casi(dati_prov, "2020-03-01", "2020-03-02")
      "2020-03-01"<- nuovi_casi(dati_prov, "2020-02-29", "2020-03-01")
      "2020-02-29"<- nuovi_casi(dati_prov, "2020-02-28", "2020-02-29")
      "2020-02-28"<- nuovi_casi(dati_prov, "2020-02-27", "2020-02-28")
      "2020-02-27"<- nuovi_casi(dati_prov, "2020-02-26", "2020-02-27")
      "2020-02-26"<- nuovi_casi(dati_prov, "2020-02-25", "2020-02-26")
      "2020-02-25"<- nuovi_casi(dati_prov, "2020-02-24", "2020-02-25")
      
      
      
      incidenza_prov <- data.frame(
        `2020-04-07`,
        `2020-04-06`,
        `2020-04-05`,
        `2020-04-04`,
        `2020-04-03`,
        `2020-04-02`,
        `2020-04-01`,
        `2020-03-31`,
        `2020-03-30`,
        `2020-03-29`,
        `2020-03-28`,
        `2020-03-27`,
        `2020-03-26`,
        `2020-03-25`,
        `2020-03-24`,
        `2020-03-23`,
        `2020-03-22`,
        `2020-03-21`,
        `2020-03-20`,
        `2020-03-19`,
        `2020-03-18`,
        `2020-03-17`,
        `2020-03-16`,
        `2020-03-15`,
        `2020-03-14`,
        `2020-03-13`,
        `2020-03-12`,
        `2020-03-11`,
        `2020-03-10`,
        `2020-03-09`,
        `2020-03-08`,
        `2020-03-07`,
        `2020-03-06`,
        `2020-03-05`,
        `2020-03-04`,
        `2020-03-03` )
      
      
      
      date_inc_prov <- c(
        "2020-04-07",
        "2020-04-06",
        "2020-04-05",
        "2020-04-04",
        "2020-04-03",
        "2020-04-02",
        "2020-04-01",
        "2020-03-31",
        "2020-03-30",
        "2020-03-29",
        "2020-03-28",
        "2020-03-27",
        "2020-03-26",
        "2020-03-25",
        "2020-03-24",
        "2020-03-23",
        "2020-03-22",
        "2020-03-21",
        "2020-03-20",
        "2020-03-19",
        "2020-03-18",
        "2020-03-17",
        "2020-03-16",
        "2020-03-15",
        "2020-03-14",
        "2020-03-13",
        "2020-03-12",
        "2020-03-11",
        "2020-03-10",
        "2020-03-09",
        "2020-03-08",
        "2020-03-07",
        "2020-03-06",
        "2020-03-05",
        "2020-03-04",
        "2020-03-03" 
      )
      
      
      incidenza_prov <- t(incidenza_prov)
      incidenza_prov <- as.numeric(incidenza_prov)
      date_inc_prov <- as.Date(date_inc_prov)
      df_inc_prov <- data.frame(date_inc_prov,incidenza_prov)
      df_inc_prov <- filter(df_inc_prov, incidenza_prov>=0)
      
      
      date_prov <- uncount(df_inc_prov, weights = incidenza_prov, .remove = FALSE)
      
      onset <- date_prov$date_inc_prov
      
      
      
      today <- today()
      
      i <- incidence(onset, last_date=today)
      
      
      
      
    i$dates <- as.Date(i$dates)

    
    res_parametric_si <- estimate_R(i, 
                                    method="parametric_si",
                                    config = make_config(list(
                                      mean_si = 3.96, 
                                      std_si = 4.75))
    )
    
    
    
      plot(res_parametric_si, legend = FALSE, "R")
      
    })
    
    
    
    
    
    
    ######## PLOT R0 PROVINCE ###### END
    ### calcolo R0 #### start
    
    
    output$rzero2 <- renderPrint({ 
      
      
      dati_prov  <-  switch(input$selmod, 
                            "Campobasso" =  filter(dati_prov, denominazione_provincia=="Campobasso") ,
                            "Isernia" =  filter(dati_prov, denominazione_provincia=="Isernia")
                              )
      
      "2020-04-07"<- nuovi_casi(dati_prov, "2020-04-06", "2020-04-07")
      "2020-04-06"<- nuovi_casi(dati_prov, "2020-04-05", "2020-04-06")
      "2020-04-05"<- nuovi_casi(dati_prov, "2020-04-04", "2020-04-05")
      "2020-04-04"<- nuovi_casi(dati_prov, "2020-04-03", "2020-04-04")
      "2020-04-03"<- nuovi_casi(dati_prov, "2020-04-02", "2020-04-03")
      "2020-04-02"<- nuovi_casi(dati_prov, "2020-04-01", "2020-04-02")
      "2020-04-01"<- nuovi_casi(dati_prov, "2020-03-31", "2020-04-01")
      "2020-03-31"<- nuovi_casi(dati_prov, "2020-03-30", "2020-03-31")
      "2020-03-30"<- nuovi_casi(dati_prov, "2020-03-29", "2020-03-30")
      "2020-03-29"<- nuovi_casi(dati_prov, "2020-03-28", "2020-03-29")
      "2020-03-28"<- nuovi_casi(dati_prov, "2020-03-27", "2020-03-28")
      "2020-03-27"<- nuovi_casi(dati_prov, "2020-03-26", "2020-03-27")
      "2020-03-26"<- nuovi_casi(dati_prov, "2020-03-25", "2020-03-26")
      "2020-03-25"<- nuovi_casi(dati_prov, "2020-03-24", "2020-03-25")
      "2020-03-24"<- nuovi_casi(dati_prov, "2020-03-23", "2020-03-24")
      "2020-03-23"<- nuovi_casi(dati_prov, "2020-03-22", "2020-03-23")
      "2020-03-22"<- nuovi_casi(dati_prov, "2020-03-21", "2020-03-22")
      "2020-03-21"<- nuovi_casi(dati_prov, "2020-03-20", "2020-03-21")
      "2020-03-20"<- nuovi_casi(dati_prov, "2020-03-19", "2020-03-20")
      "2020-03-19"<- nuovi_casi(dati_prov, "2020-03-18", "2020-03-19")
      "2020-03-18"<- nuovi_casi(dati_prov, "2020-03-17", "2020-03-18")
      "2020-03-17"<- nuovi_casi(dati_prov, "2020-03-16", "2020-03-17")
      "2020-03-16"<- nuovi_casi(dati_prov, "2020-03-15", "2020-03-16")
      "2020-03-15"<- nuovi_casi(dati_prov, "2020-03-14", "2020-03-15")
      "2020-03-14"<- nuovi_casi(dati_prov, "2020-03-13", "2020-03-14")
      "2020-03-13"<- nuovi_casi(dati_prov, "2020-03-12", "2020-03-13")
      "2020-03-12"<- nuovi_casi(dati_prov, "2020-03-11", "2020-03-12")
      "2020-03-11"<- nuovi_casi(dati_prov, "2020-03-10", "2020-03-11")
      "2020-03-10"<- nuovi_casi(dati_prov, "2020-03-09", "2020-03-10")
      "2020-03-09"<- nuovi_casi(dati_prov, "2020-03-08", "2020-03-09")
      "2020-03-08"<- nuovi_casi(dati_prov, "2020-03-07", "2020-03-08")
      "2020-03-07"<- nuovi_casi(dati_prov, "2020-03-06", "2020-03-07")
      "2020-03-06"<- nuovi_casi(dati_prov, "2020-03-05", "2020-03-06")
      "2020-03-05"<- nuovi_casi(dati_prov, "2020-03-04", "2020-03-05")
      "2020-03-04"<- nuovi_casi(dati_prov, "2020-03-03", "2020-03-04")
      "2020-03-03"<- nuovi_casi(dati_prov, "2020-03-02", "2020-03-03")
      "2020-03-02"<- nuovi_casi(dati_prov, "2020-03-01", "2020-03-02")
      "2020-03-01"<- nuovi_casi(dati_prov, "2020-02-29", "2020-03-01")
      "2020-02-29"<- nuovi_casi(dati_prov, "2020-02-28", "2020-02-29")
      "2020-02-28"<- nuovi_casi(dati_prov, "2020-02-27", "2020-02-28")
      "2020-02-27"<- nuovi_casi(dati_prov, "2020-02-26", "2020-02-27")
      "2020-02-26"<- nuovi_casi(dati_prov, "2020-02-25", "2020-02-26")
      "2020-02-25"<- nuovi_casi(dati_prov, "2020-02-24", "2020-02-25")
      
      
      
      incidenza_prov <- data.frame(
        `2020-04-07`,
        `2020-04-06`,
        `2020-04-05`,
        `2020-04-04`,
        `2020-04-03`,
        `2020-04-02`,
        `2020-04-01`,
        `2020-03-31`,
        `2020-03-30`,
        `2020-03-29`,
        `2020-03-28`,
        `2020-03-27`,
        `2020-03-26`,
        `2020-03-25`,
        `2020-03-24`,
        `2020-03-23`,
        `2020-03-22`,
        `2020-03-21`,
        `2020-03-20`,
        `2020-03-19`,
        `2020-03-18`,
        `2020-03-17`,
        `2020-03-16`,
        `2020-03-15`,
        `2020-03-14`,
        `2020-03-13`,
        `2020-03-12`,
        `2020-03-11`,
        `2020-03-10`,
        `2020-03-09`,
        `2020-03-08`,
        `2020-03-07`,
        `2020-03-06`,
        `2020-03-05`,
        `2020-03-04`,
        `2020-03-03` )
      
      
      
      date_inc_prov <- c(
        "2020-04-07",
        "2020-04-06",
        "2020-04-05",
        "2020-04-04",
        "2020-04-03",
        "2020-04-02",
        "2020-04-01",
        "2020-03-31",
        "2020-03-30",
        "2020-03-29",
        "2020-03-28",
        "2020-03-27",
        "2020-03-26",
        "2020-03-25",
        "2020-03-24",
        "2020-03-23",
        "2020-03-22",
        "2020-03-21",
        "2020-03-20",
        "2020-03-19",
        "2020-03-18",
        "2020-03-17",
        "2020-03-16",
        "2020-03-15",
        "2020-03-14",
        "2020-03-13",
        "2020-03-12",
        "2020-03-11",
        "2020-03-10",
        "2020-03-09",
        "2020-03-08",
        "2020-03-07",
        "2020-03-06",
        "2020-03-05",
        "2020-03-04",
        "2020-03-03" 
      )
      
      
      incidenza_prov <- t(incidenza_prov)
      incidenza_prov <- as.numeric(incidenza_prov)
      date_inc_prov <- as.Date(date_inc_prov)
      df_inc_prov <- data.frame(date_inc_prov,incidenza_prov)
      df_inc_prov <- filter(df_inc_prov, incidenza_prov>=0)
      
      
      date_prov <- uncount(df_inc_prov, weights = incidenza_prov, .remove = FALSE)
      
      onset <- date_prov$date_inc_prov
      
      
      
      today <- today()
      
      i <- incidence(onset, last_date=today)
      
      
      
      
      i$dates <- as.Date(i$dates)
      
      
      res_parametric_si <- estimate_R(i, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 3.96, 
                                        std_si = 4.75))
      )
      
      
      
      
      
      print(res_parametric_si)
      
    })
    
    
    ### calcolo R0 #### end
    
    
    
}

shinyApp(ui, server)