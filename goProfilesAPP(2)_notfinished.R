##IMPORTANT! Not Finished!

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
                             h3("xlsx"),
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
                             h2(strong("3.- Jump to", span("Graphs", style = "color:blue"))),
                             h3("Visualize and download the functional profiles of your lists."),
                             h2(strong("4.- Land to", span ("Results", style="color:blue"))),
                             h3("See the results of your analysis.")
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
                  
                  tabPanel(h3(strong("Presets",img(src="settings.png",  height = 23,width = 47,style="float:left; padding-right:25px"))),
                           
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
                  tabPanel(h3(strong("Graphs",img(src="graph.png",  height = 25,width = 50,style="float:left; padding-right:25px"))),
                           sidebarPanel(width = 6,
                              h2(strong("Functional profiles")),
                              uiOutput("other1"),
                              uiOutput(outputId="plots"),
                              downloadButton("download1"),
                              br(),
                              h2(strong("Merged profiles")),
                              uiOutput(outputId = "plots2"),
                              downloadButton("download2")),
                           sidebarPanel(width = 6,
                              h2(strong("Functional profiles")),
                              verbatimTextOutput("text"),
                              downloadButton("download3"),
                              br(),
                              h2(strong("Merged profiles")),
                              verbatimTextOutput("text2"),
                              downloadButton("download4"))
                           ),

                tabPanel(h3(strong("Results",img(src="results.png",  height = 25,width = 50,style="float:left; padding-right:25px"))),
                         sidebarPanel(width=6,
                           h2(strong("Global test: Euclidean distance")),
                           verbatimTextOutput("table1"),
                           checkboxInput("header2", h5(strong("Fisher Test")), FALSE),
                           h2(strong("Class-by-class test: Fisher Test")),
                           uiOutput("fishs"),
                           verbatimTextOutput("table2"),
                           downloadButton("download5")),
                         sidebarPanel(width = 6,
                            uiOutput("dendo")
              
                           
                         )
                    )
                         
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
    if(length(input$checkGroup3) > 1 ){radioButtons("graphs", "",choices = c(BP= "BP",MF= "MF", CC = "CC"), selected = "BP",inline=T)}
    else{return()}
  })
  
  ##Other species other packages
  output$fishs <- renderUI({
    if(length(input$checkGroup3) > 1 ){radioButtons("fish", "",choices = c(BP= "BP",MF= "MF", CC = "CC"), selected = "BP",inline=T)}
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
      names(file_contentsss)[i] = input$upload1$name[[i]]
    }
    for(j  in seq_along(file_contentsss)){
      lil[[j]] <- as.character(file_contentsss[[j]][[l]])
      names(lil)[j] = names(file_contentsss)[j]
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
  
  #Dendograma
  
  output$dendo <- renderUI({
    req(input$run)
    if(length(input$upload) > 2){plotOutput("dendo2")}
    output$dendo2 <- renderPlot({
      if(input$fish == "MF"){
        plot(equivClust(input$slider, "MF", datass(), orgPackage=input$select22),main = "Dendrogram (method = complete)", ylab = "Equivalence threshold distance")
      }
      if(input$fish == "BP"){
        plot(equivClust(input$slider, "BP", datass(), orgPackage=input$select22),main = "Dendrogram (method = complete)", ylab = "Equivalence threshold distance")
      }
      if(input$fish == "CC"){
        plot(equivClust(input$slider, "CC", datass(), orgPackage=input$select22),main = "Dendrogram (method = complete)", ylab = "Equivalence threshold distance")
      }
    })
  }) 
 
  ##Basic Profiles
  dt <- reactive({
    req(input$run)
    c=0
    lisss <- datass()
    list1 <- list()
    for(i in 1:length(lisss)){
      c=c+1
      list1[[c]]= basicProfile(lisss[[i]], orgPackage =input$select22,onto = input$checkGroup3, level=input$slider)
      names(list1)[i]= names(lisss)[i]
      
    }
    return(list1)
   
    
  })
  
  #Download functional profiles tables
  output$download3 <- downloadHandler(
    filename = function() {"Bprofiles.txt"},
    content = function(file) {
      xlsx::write.xlsx
      sink(file); print(dt()); sink();
    }
  )
 
  
  #Merged
  m <- reactive({
    req(input$run)
    c=0
    dt2 <- dt()
    list1 <- list()
    for( i in 1:(length(dt2)-1)){
      for(j in (i+1):length(dt2)){
        c=c+1
        ii <- i
        jj <- j
        list1[[c]] = mergeProfilesLists(dt2[[ii]],dt2[[jj]], profNames = c(names(dt2)[ii],names(dt2)[jj]))
        names(list1)[c]= paste0(names(dt2)[ii],"_",names(dt2)[jj])
      }
    }
    print(list1)
    
  })
  
  #Download merges tables
  output$download4 <- downloadHandler(
    filename = function() {"merge.txt"},
    content = function(file) {
      xlsx::write.xlsx
      sink(file); print(m()); sink();
    }
  )
  #Graphs visualization functional profiles 
  output$plots <- renderUI({
    req(input$run)
    out <- list()
    lisss <- datass()
    for(i in 1:length(lisss)){
      out[[i]] <-  plotOutput(outputId = paste0("plot_",i))
    }
    return(out)
  })
observe({ 
  req(input$run)
    for (i in 1:length(dt())){  
      local({ 
        ii <- i
        output[[paste0('plot_',ii)]] <- renderPlot({
          if(input$graphs == "BP"){
            print(plotProfiles(dt()[[ii]][["BP"]], aTitle = paste0("Functional Profile:",names(dt())[ii],"_BP"), multiplePlots =TRUE, labelWidth = 100))
          }
          if(input$graphs == "MF"){
            print(plotProfiles(dt()[[ii]][["MF"]], aTitle = paste0("Functional Profile:",names(dt())[ii],"_MF"), multiplePlots =TRUE, labelWidth = 100)) 
          }
          if(input$graphs == "CC"){
            print(plotProfiles(dt()[[ii]][["CC"]], aTitle = paste0("Functional Profile:",names(dt())[ii],"_CC"), multiplePlots =TRUE, labelWidth = 100)) 
          }
        })
      })
    }                                  
  })

output$download1 <- downloadHandler(
  filename = "Bprofiles.pdf",
  content = function(file) {
    pdf(file)
    for(i in 1:length(dt())){
        plotProfiles(dt()[[i]],aTitle = paste0("Functional Profile:","",names(dt())[i]))
    }
    dev.off()
  }
)

## Tables visualization functional profiles
output$text <- renderPrint({
 for( i in 1:length(dt())){
   local({
    if(input$graphs == "BP"){
      print(paste0(names(dt())[i],"_BP"))
      print(dt()[[i]][["BP"]])
    }
    if(input$graphs == "MF"){
      print(paste0(names(dt())[i],"_MF"))
      print(dt()[[i]][["MF"]]) 
    }
    if(input$graphs == "CC"){
      print(paste0(names(dt())[i],"_cc"))
      print(dt()[[i]][["CC"]]) 
    }
  })
 }
})


#Creating and visualizing merged
output$plots2 <- renderUI({
  out2 <- list()
  c=0
  for( i in 1:(length(lisss)-1)){
    for(j in (i+1):length(lisss)){
      c=c+1
      out2[[c]] <-  plotOutput(outputId = paste0("plot2_",i,"_",j))
    }
  }
  return(out2)
})
observe({  
  for( i in 1:(length(dt())-1)){
    local({
      for(j in (i+1):length(dt())){
        local({
          ii <- i
          jj <- j
          output[[paste0('plot2_',ii,"_",jj)]] <- renderPlot({
            if(input$graphs == "BP"){
              print(plotProfiles(m()[[paste0(names(dt())[ii],"_",names(dt())[jj])]][["BP"]], aTitle = paste0("Merged:","",names(dt())[ii],"_",names(dt())[jj],"_BP"), multiplePlots =TRUE, labelWidth = 100))
            }
            if(input$graphs == "MF"){
              print(plotProfiles(m()[[paste0(names(dt())[ii],"_",names(dt())[jj])]][["MF"]], aTitle = paste0("Merged:","",names(dt())[ii],"_",names(dt())[jj],"_MF"), multiplePlots =TRUE, labelWidth = 100))
            }
            if(input$graphs == "CC"){
              print(plotProfiles(m()[[paste0(names(dt())[ii],"_",names(dt())[jj])]][["CC"]], aTitle = paste0("Merged:","",names(dt())[ii],"_",names(dt())[jj],"_CC"), multiplePlots =TRUE, labelWidth = 100))
            }
          })
        })
      }                                  
    })
  }
})

output$download2 <- downloadHandler(
  filename = "merge.pdf",
  content = function(file) {
    pdf(file)
    for(i in 1:length(m())){
      plotProfiles(m()[[i]],aTitle = paste0("Merge:","",names(m())[i]))
    }
    dev.off()
  }
)

## Tables visualization functional profiles
output$text2 <- renderPrint({
  for( i in 1:length(m())){
    local({
    if(input$graphs == "BP"){
      print(paste0(names(m())[i],"_BP"))
      print(m()[[i]][["BP"]])
    }
    if(input$graphs == "MF"){
      print(paste0(names(m())[i],"_MF"))
      print(m()[[i]][["MF"]]) 
    }
    if(input$graphs == "CC"){
      print(paste0(names(m())[i],"_CC"))
      print(m()[[i]][["CC"]]) 
      }
    })
  }
})


#Euclidean distance test

output$table1 <- renderPrint({
  list35 = list()
  list33 = list()
  list34 = list()
  lisss <- datass()
  c=0
  for( j in 1:(length(lisss)-1)){
    for(l in (j+1):length(lisss)){
      c = c+1
      list35[[c]] = compSummary(compareGeneLists(lisss[[j]],lisss[[l]] , idType = input$select2, level = input$slider, onto= input$checkGroup3, orgPackage = input$select22))
      names(list35)[c]= paste0(names(lisss[j]),"_",names(lisss[l]))
      
    }
  }
  
  print(list35)
})

#Test Fisher

output$table2 <- renderPrint({
  req(input$header2)
  list33 = list()
  list34 = list()
  lisss <- datass()
  c=0
  for( j in 1:(length(lisss)-1)){
    for(l in (j+1):length(lisss)){
      c = c+1
      if(length(intersect(lisss[[j]], lisss[[l]])) > 0){
        
        if(input$fish == "BP"){
          list34[[c]] = fisherGOProfiles(dt()[[j]][["BP"]], dt()[[l]][["BP"]], 
                                         basicProfile(intersect(lisss[[j]], lisss[[l]]), idType = input$select2, level = input$slider, onto= "BP", orgPackage = input$select22)[["BP"]], method="holm")
          names(list34)[c]= paste0(names(lisss[j]),"_",names(lisss[l]),"_BP")
        }
        if(input$fish == "MF"){
          list34[[c]] = fisherGOProfiles(dt()[[j]][["MF"]], dt()[[l]][["MF"]], 
                                         basicProfile(intersect(lisss[[j]], lisss[[l]]), idType = input$select2, level = input$slider, onto= "MF", orgPackage = input$select22)[["MF"]], method="holm")
          names(list34)[c]= paste0(names(lisss[j]),"_",names(lisss[l]),"_MF")
          
        }
        if(input$fish == "CC"){
          list34[[c]] = fisherGOProfiles(dt()[[j]][["CC"]], dt()[[l]][["CC"]], 
                                         basicProfile(intersect(lisss[[j]], lisss[[l]]), idType = input$select2, level = input$slider, onto= "CC", orgPackage = input$select22)[["CC"]], method="holm")
          names(list34)[c]= paste0(names(lisss[j]),"_",names(lisss[l]),"_CC")
          
        }
        
      }
    }
  }
  print(list34)
}
)}


# Run the application 
shinyApp(ui = ui, server = server)
