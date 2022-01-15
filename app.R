
library(readr)
library(shiny)
library(dplyr)
library(shinythemes)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(org.Gg.eg.db)
library(org.EcK12.eg.db)
library(goProfiles)
library(tidyverse)
library(BiocManager)

ui <- fluidPage(theme=("bootstrap.min3.css"),
    br(),
    titlePanel(h2(strong("GoProfiles app"))),
    br(),
    navlistPanel(
        widths = c(1,10),
        tabPanel(h3(strong("Home", img(src="home.png",  height = 20,width = 50,style="float:left; padding-right:25px"))),
                h1(strong("Welcome to GoProfiles app!")),
                h3("This application implements the package goProfiles to compare lists of genes based on their functional profiles."),
                br(),
            sidebarPanel(
                h2(strong("GoProfiles Package:")),
                br(),
                h3("Author: Alex Sanchez, Jordi Ocana and Miquel Salicru"),
                br(),
                h3(strong(p("For a manual of the package and live examples, visit:",
                  a("goProfiles package homepage.", 
                    href = "https://www.bioconductor.org/packages/release/bioc/html/goProfiles.html")))),
                br(),
                code(h2(strong("Other packages required:"))),
                h3("shiny"),
                h3("shinythemes"),
                h3("dplyr"),
                h3("BiocManager"),
                h3("org.Hs.eg.db"),
                h3("org.Mm.eg.db"),
                h3("org.Gg.eg.db"),
                h3("org.EcK12.eg.db"),
                h3("tidyverse"),
                h3("readr"),
                br(),
                br(),
                h4(p("The code and all the attached files are included in a github repository:",
                            a("goProfilesApp", 
                              href = "https://github.com/xevialarconmasferrer/goProfilesApp"))),

        ),  
            sidebarPanel(
                  h1(strong("First steps:")),
                  br(),
                  h2(strong("1.- Go to",span("Data", style = "color:blue"))),
                  h3("Upload your lists."),
                  h2(strong("2.- Continue  to",span("Presets", style = "color:blue"))),
                  h3("Select among all the options we have to customize your analysis in your own way."),
                  h2(strong("3.- Jump to", span("Results", style = "color:blue"))),
                  h3("Visualize and download the results of your analysis."),
       )),
        
        tabPanel(h3(strong("Data", img(src="upload.png",  height = 25,width = 48,style="float:left; padding-right:25px"))),
            sidebarPanel(
                h2(strong(("Choose file to upload"))),
                br(),
                h3("It must be a .csv file. Lists must be in separated files and uploaded all together in the browser."),
                fileInput("upload1", NULL,
                          multiple = TRUE,
                          buttonLabel = "Browse",
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                checkboxInput("header", h3(strong("Header")), TRUE),
                radioButtons("sep", h4(strong("Separator")),
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                radioButtons("quote", h4(strong("Quote")),
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                           selected = '"'),
                radioButtons("disp", h4(strong("Display")),
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head")),
          tableOutput("head"),
          tableOutput("head2")),
                
        tabPanel(h3(strong("Presets",img(src="settings.png",  height = 25,width = 50,style="float:left; padding-right:25px"))),
                 
          sidebarPanel( 
            selectInput("select2", h2(strong("Select type of entry:")),
                        choices = list("Entrez", "BioCprobes", "GOTermsFrames"), selected ="Entrez"),
            h3("Select the type of identificator of your gene lists"),
            br(),
            selectInput("select22", h2(strong("Select species:")),
                        choices = list("Hommo sapiens" = "org.Hs.eg.db", "Mus musculus" = "org.Mm.eg.db" ,
                                       "Gallus gallus" = "org.Gg.eg.db", "Escherichia coli" = "org.EcK12.eg.db", "Other"="other"), selected = "org.Hs.eg.db"),
            h3("Select the species of your gene lists."),
            h3("If your lists belong to one species that is not in there, click 'Others'. Write the name of the bioconductor annotation package for that species. 
                        It will be installed and ready to use! "),
            uiOutput("other"),
            br(),
            checkboxGroupInput("checkGroup3", h2(strong("Select which ontologycal category")),
                               choices = c("Biological process" ="BP",
                                           "Molecular function" = "MF",
                                           "Cellular component" = "CC")),
            h3("Select which ontological category or categories would you like to include."),
            br(),
            sliderInput("slider",h2(strong("Select the level of ontology:")),
                        min= 1, max=10, value=1),
            h3("Select which level of ontology would you like to reach."),
            br(),
            actionButton("run", "Run Analysis", icon("paper-plane"), 
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
        tabPanel(h3(strong("Results",img(src="results.png",  height = 25,width = 50,style="float:left; padding-right:25px"))),
          sidebarPanel(
            h2(strong("Plot basic profiles by ontology")),
            uiOutput("other1"),
            plotOutput("plot2"),
            plotOutput("plot3"),
            plotOutput("plot1"),
            downloadButton("download1")),
          sidebarPanel(
            h2(strong("Functional profiles")),
            verbatimTextOutput("table3"),
            downloadButton("download2"),
          ),
          sidebarPanel(
            h2(strong("Global test: Euclidean distance")),
            verbatimTextOutput("table1"),
            checkboxInput("header2", h5(strong("Fisher Test")), FALSE),
            h2(strong("Class-by-class test: Fisher Test")),
            uiOutput("fish"),
            verbatimTextOutput("table2"),
            downloadButton("download4")))
    )
)

server <- function(input, output) {
  
  ##Other species other packages
  output$other <- renderUI({
    if(input$select22 =="other"){textInput("text2", strong("Other species"), value = 
                                             "Write the Bioconductor annotation package for the species...")}
  })
  
  ##Other species other packages
  output$other1 <- renderUI({
    req(input$run)
    if(length(input$checkGroup3) > 1 ){radioButtons("graphs", "",choices = c(BP= "BP",MF= "MF", CC = "CC"), selected = "BP",inline=T)}
    else{return()}
  })
    
  #Install new packages
  observeEvent(input$run, {
    if(!is.null(input$text2)){(BiocManager::install(input$text2))}})
  observeEvent(input$run, {
    if(!is.null(input$text2)){
      foo <- input$text2
      library(foo, character.only = TRUE)
    }})
    
  ##File uploading
  datass <- reactive({
    req(input$upload1)
    file_path = input$upload1$datapath
    file_contentsss = c()
    lil= c()
    l <- 1
    for(i in 1: length(file_path))
    {
      file_contentsss[[i]] =  read.csv(file = file_path[[i]], quote = input$quote, header= input$header, sep = input$sep)
    }
    for(j  in seq_along(file_contentsss)){
      lil[[j]] <- as.numeric(file_contentsss[[j]][[l]])
    }
    for(l in seq_along(lil)){
      nams= paste0("r", 1:l)
      names(lil) = nams
    }
    return(lil)
  })
    
  #Descriptive table of the files uploaded
  output$head <- renderTable({
    if(is.null(input$upload1)){return()}
    input$upload1
  })
    
  #Data frame of the lists
  output$head2 <- renderTable({
    req(input$upload1)
    if(input$disp == "head") {
      fun1 <- function(lst, n){
        sapply(lst, `[`, n)
      }
      f1 <- fun1(datass(), 1:10)
      print(f1)
    }
    else {
      fun1 <- function(lst, n){
        sapply(lst, `[`, n)
      }
     f2 <- fun1(datass(), 1:1000)
     print(f2)
      }
  })
   
    #Creating and visualizing the functional profiles and merged

    output$plot1 <- renderPlot({
      req(input$run)
        MF1 <- basicProfile(datass()$r1, orgPackage =input$select22,onto = input$checkGroup3, level=input$slider)
        MF2 <- basicProfile(datass()$r2, orgPackage =input$select22,onto = input$checkGroup3, level=input$slider)
        merges <- mergeProfilesLists(MF1,MF2, profNames = c("MF1", "MF2"), emptyCats = FALSE)
        output$table3 <- renderPrint({merges})
        output$download2 <- downloadHandler(
          filename = "merges.csv",
          content = function(file) {
            write.csv(merges, file , row.names = TRUE)
          }
        )
        output$plot2 <- renderPlot({
          req(input$run)
        if(length(input$checkGroup3) > 1){
          if(input$graphs == "BP"){plotProfiles(MF1$BP, aTitle = "Basic profile for BP ontology: List 1")}
          if(input$graphs== "MF"){plotProfiles(MF1$MF, aTitle = "Basic profile for MF ontology: List 1")}
          if(input$graphs== "CC"){plotProfiles(MF1$CC, aTitle = "Basic profile for CC ontology: List 1")}
        }
          else{print(plotProfiles(MF1, aTitle = "Basic profile: List 1"))}
        })
        output$plot3 <- renderPlot({
          req(input$run)
        if(length(input$checkGroup3) > 1){
          if(input$graphs == "BP"){plotProfiles(MF2$BP, aTitle = "Basic profile for BP ontology: List 2")}
          if(input$graphs== "MF"){plotProfiles(MF2$MF, aTitle = "Basic profile for MF ontology: List 2")}
          if(input$graphs== "CC"){plotProfiles(MF2$CC, aTitle = "Basic profile for CC ontology: List 2")}
        }
          else{print(plotProfiles(MF2, aTitle = "Basic profile: List 2"))
            }
        })
        if(length(input$checkGroup3) > 1){
        if(input$graphs == "BP"){plotProfiles(merges$BP, aTitle = "Merged of basic profiles for BP ontology")}
        if(input$graphs== "MF"){plotProfiles(merges$MF, aTitle = "Merged of basic profiles for MF ontology")}
        if(input$graphs== "CC"){plotProfiles(merges$CC, aTitle = "Merged of basic profiles for CC ontology")}
        }
        else{print(plotProfiles(merges, aTitle = "Merged of basic profiles"))
        }
    })

    #Calculation and visualizacion of the results form Euclidean distances Test
    output$table1 <- renderPrint({
    if(length(input$upload1) < 3){
      req(input$run)
      comSummary(compareGeneLists(datass()$r1, datass()$r2,idType = input$select2, 
                     onto = input$checkGroup3, level = input$slider, orgPackage = input$select22))
    }
    if(length(input$upload1) > 2){
      list3 = list()
      c = 0
      for( i in 1:(length(datass())-1)){
        for(j in (i+1):length(datass())){
          c = c+1
          list3[[c]] = comSummary(compareGeneLists(datass()[[i]], datass()[[j]],idType = input$select2, 
                        onto = input$checkGroup3, level = input$slider, orgPackage = input$select22))
          names(list3[c])= paste0(names(datass()[i]),"_",names(datass()[j]))
        }
      }
    }
    
  })
  
    })

     #Fisher Graphs
  output$fish <- renderUI({
    req(input$run)
    if(length(input$checkGroup3) > 1 ){radioButtons("fish1", "",choices = c(BP= "BP",MF= "MF", CC = "CC"), selected = "BP",inline=T)}
  })

    
    #Calculation and visualization results from Fisher Test
    output$table2 <- renderPrint({
      req(input$run)
        if( input$header2 == TRUE){
          MF1 <- basicProfile(datass()$r1, orgPackage =input$select22,onto = input$checkGroup3, level=input$slider)
          MF2 <- basicProfile(datass()$r2, orgPackage =input$select22,onto = input$checkGroup3, level=input$slider)
          if (length(intersect(datass()$r1, datass()$r2)) == 0) {
            stop("There are no categories")
          }
          else{
            if(length(input$checkGroup3) == 1){
              commProf <- basicProfile(intersect(datass()$r1, datass()$r2),
                                       onto=input$checkGroup3, level=input$slider, orgPackage = input$select22)[[input$checkGroup3]]
              fisher <- fisherGOProfiles(MF1[[input$checkGroup3]], MF2[[input$checkGroup3]], commProf, method="holm")
              output$download4 <- downloadHandler(
                filename = "fisher.csv",
                content = function(file) {
                  write.csv(fisher, file , row.names = TRUE)
                }
              )
              print(fisher)
            }
            else{
              if(input$fish1 == "BP"){
                commProf <- basicProfile(intersect(datass()$r1, datass()$r2),
                                     onto=input$fish1, level=input$slider, orgPackage = input$select22)[[input$fish1]]
               fisher <- fisherGOProfiles(MF1[[input$fish1]], MF2[[input$fish1]], commProf, method="holm")
               output$download4 <- downloadHandler(
                 filename = "fisher.csv",
                 content = function(file) {
                   write.csv(fisher, file , row.names = TRUE)
                 }
               )
               print(fisher)
             }
              if(input$fish1 == "MF"){
                commProf <- basicProfile(intersect(datass()$r1, datass()$r2),
                                       onto=input$fish1, level=input$slider, orgPackage = input$select22)[[input$fish1]]
                fisher <- fisherGOProfiles(MF1[[input$fish1]], MF2[[input$fish1]], commProf, method="holm")
                output$download4 <- downloadHandler(
                  filename = "fisher.csv",
                  content = function(file) {
                    write.csv(fisher, file , row.names = TRUE)
                  }
                )
                print(fisher)
              }
              if(input$fish1 == "CC"){
                commProf <- basicProfile(intersect(datass()$r1, datass()$r2),
                                       onto=input$fish1, level=input$slider, orgPackage = input$select22)[[input$fish1]]
                fisher <- fisherGOProfiles(MF1[[input$fish1]], MF2[[input$fish1]], commProf, method="holm")
                output$download4 <- downloadHandler(
                  filename = "fisher.csv",
                  content = function(file) {
                    write.csv(fisher, file , row.names = TRUE)
                  }
                )
                print(fisher)
              }
            }
          }  
        }
    })
    
    #Download results
    output$download1 <- downloadHandler(
          filename = "Merge.pdf",
          content = function(file) {
            pdf(file)
              MF1 <- basicProfile(datass()$r1, orgPackage =input$select22,onto = input$checkGroup3, level=input$slider)
              MF2 <- basicProfile(datass()$r2, orgPackage =input$select22,onto = input$checkGroup3, level=input$slider)
              merges <- mergeProfilesLists(MF1,MF2, profNames = c("MF1", "MF2"), emptyCats = FALSE)
              plotProfiles(MF1, aTitle = "List 1")
              plotProfiles(MF2, aTitle = "List 2")
              plotProfiles(merges, aTitle = "Merged")
            dev.off()
          }
    )


 

}

# Run the application 
shinyApp(ui = ui, server = server)
