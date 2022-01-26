#  goProfiles App

# "goProfiles" is a R package, created by: Alex Sánchez, Jordi Ocana and Miquel Salicru (2011). Ths package  creates functional profiles from lists of genes and does a comparative 
#  analysis to evaluate how equivalent those profiles are. The main reason this application was created is to allow scientist to compare lists of genes obtained from similar 
#  experiments. Lets say for example that to laboratories make the same experiment. We could think that the expected results should be the same, but some times do to differences on
#  the methodology, the technoloy used or the "human" manipulation it is posible that does results may differ. Sometimes it is interesting to combine results in order to have more
#  data so the study become more robust. We need a tool that allows as to evaluate how equal are this results to be combined. goProfiles does precisly that.
#  The methodology  behind this functionality consists on a global test of Euclidean distance in order to evaluate globally the proximity of those lists, and in case that we see 
#  differences, a Fisher test class-by-class to see in which related  ontological categories we see those diferences.

#  This Application includes all the funcionalities of this package in order to make this tool avaliable to everyone who need to perform this comparative analysis and does not have 
#  notions of programming.

# Here you have the list of all the R packages required to run this application.
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


#  The first component of a shiny app is the "User Interface". Here in, there is all the code related to how the application is visualized. Furthermore, is where we define the 
#  "widgets", so the user is able to upload the lists, and select from different options those that he/she prefers for the analysis.  Those options selected will be inputs that 
#  will be sended to the server, where the calculations are done.
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
                             sliderInput("slider2",h2(strong("Select a treshold for the analysis:")),
                                         min= 0.01, max=0.10, value=0.05),
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
                           downloadButton("download5"),
                           checkboxInput("header2", h5(strong("Fisher Test")), FALSE),
                           h2(strong("Class-by-class test: Fisher Test")),
                           uiOutput("fishs"),
                           verbatimTextOutput("table2"),
                           downloadButton("download6")),
                         sidebarPanel(width = 6,
                            uiOutput("dendo")
              
                           
                         )
                    )
                         
                )
)

# The second component of a shiny App is the server. Here in are included the itinerary of fucntions and calculations that the application to obtain the results, taking the inputs 
# as arguments.
server <- function(input, output) {
  
  # From a UIoutput/RenderUI widget, we define a widget type radiobutton. When the user select of which ontological categories wants to do the comparison, this widget created a 
  # option button on the "Graphs" panel for every category selected (input$checkGroup3). After the server calculate the results the user can select from which category want to see
  # the basic profiles tables and graphs and the merge table and graphs.
  output$other1 <- renderUI({{radioButtons("graphs", "",choices = c(input$checkGroup3), selected = input$checkGroup3[[1]],inline=T)}})
  
  # From a UIoutput/RenderUI widget, we define a widget type radiobutton. When the user select of which ontological categories wants to do the comparison, this widget created a 
  # option button on the "Results" panel for every category selected (input$checkGroup3). After the server calculate the results the user can select from which category want to see
  # the Fisher test results.
  output$fishs <- renderUI({{radioButtons("fish", "",choices = c(input$checkGroup3), selected = input$checkGroup3[[1]],inline=T)}})
  
  #  Install other annotation packages:
  
  #  In case the user needed a bioconductor annotation package from a different species out of the four options, the option "others" is a UIoutput/renderUi widget that if selected 
  #  will create a textinput so the user can write the name of the anotation package required.
  output$other <- renderUI({if(input$select22 =="other"){textInput("text2", strong("Other species"), value = "Write the Bioconductor annotation package for the species...")}})
  
  #  The name written is taken as an input. Observe event() allows as to give an order after observing an event. The event is "input$run" which is the acction button that triggers
  #  the server to run. When input$run is clicked, if input$text2(The name of the annotation package required), isn't null, exists, download and install the package from bioconductor
  #  through BioManager.
  observeEvent(input$run, {if(!is.null(input$text2)){(BiocManager::install(input$text2))}})
 
  #  Onces the package is installed, observe "input$run", if input$text2 is not null, exists, define a variable "foo" containing the name of the package and call it from the libary
  #  making sure that character.only() is true.
  observeEvent(input$run, {
    if(!is.null(input$text2)){
      foo <- input$text2
      library(foo, character.only = TRUE)
    }})
  
  
  #  1.- File uploading:
  
  # datass is a reactive object
  datass <- reactive({
    # In order to proceed with this code it is required input$upload1, files uploades to the inteface.
    req(input$upload1)
    # We define a new variable "file_path" that contains input$upload1$datapath, the datapath of all the files uploades.
    file_path = input$upload1$datapath
    # We define an empty list called "file_contentsss" and an empty list call "lil". 
    file_contentsss = c()
    lil= c()
    # We define a variable "l" with value 1.
    l <- 1
    # for each element through all the elements from the first to the last included in file_path(the files):
    for(i in 1: length(file_path))
    {
      # The element i of the list "file contentsss" is the element i of file_path, the file readed with read.csv(). input$quote is the option the user can select regarding quoted
      # data, input$sep is the option selected regarding the separators, and input$header is the option regarding if the list has or not headers.
      # The name of the element i in "file_contentss" is the name of the file i.
      file_contentsss[[i]] =  read.csv(file = file_path[[i]], quote = input$quote, header= input$header, sep = input$sep)
      names(file_contentsss)[i] = input$upload1$name[[i]]
    }
    # The product of this loop is a list of data.frames, as tables with a header. We need to convert those data frames in list objects. 
    # for each element through all the elements from the first to the last included in "file_contentsss"(the lists):
    for(j  in seq_along(file_contentsss)){
      # The element j of the list "lil" is the element on the position [[l]] (l=1) from  j of "file_contentss". 
      # The name of the element [[l]] of j in "lil" is the name of the element j in "file_contentsss"
      lil[[j]] <- as.character(file_contentsss[[j]][[l]])
      names(lil)[j] = names(file_contentsss)[j]
    }
    return(lil)
  })
  # The final product is the reactive object datasss() that is a list of the lists uploaded with the gene codes.
  
  
  # Descriptive table of the files uploaded:
  
  # We define the output for the widget tableoutput "head" as a table containing input$ulpload1, a table that contains all the files uploaded, the size and the datapath for each one
  # if is not null, there are files to show.
  output$head <- renderTable({
    if(is.null(input$upload1)){return()}
    input$upload1
  })
  
  # Data frame of the lists:
  
  # We define the output for the widget tableOutput "head2" as a data frame containing many columns as files uploaded and many rows as elements on the lists.
  output$head2 <- renderTable({
    # Requires that exist uploaded files.
    req(input$upload1)
    
    # If input$disp is equal to "head" which means that the user selected to only see the head of the lists:
    if(input$disp == "head") {
      # We define a function "fun1" to extract all the elements of a list using sappply()
      fun1 <- function(lst, n){
        sapply(lst, `[`, n)
      }
      # We apply the function to datass() so we extract the 10 first elements of each list in datass() and we print the resulting table.
      f1 <- fun1(datass(), 1:10)
      print(f1)
    }
     # If input$disp is equal to anything else which means that the user selected to see the entire lists:
    else {
      # We define a function "fun1" to extract all the elements of a list using sappply()
      fun1 <- function(lst, n){
        sapply(lst, `[`, n)
      }
      # We apply the function to datass() so we extract all the elements of each list in datass() and we print the resulting table.
      f1 <- fun1(datass(), 1:1000)
      print(f1)
    }
  })
  
  
  # Calcultaion of Basic Profiles:
  
  # We define a reactive variable dt()
  dt <- reactive({
    # Runing the application is required to proceed with this code.
    req(input$run)
    # We define a variable "c" with a value of 0. 
    c=0
    # We define list call lisss as datass(), in order to null the reactive properties of datasss().
    lisss <- datass()
    # We define list1 as an empty list.
    list1 <- list()
    # For all the elements included in "lisss" from first to last:
    for(i in 1:length(lisss)){
      # Each itineration sum 1 to the variable "c"
      c=c+1
      # The element "c" in "list1" is the resulting functional profile list of applying the function basicProfile to the list i. This function takes as arguments the inputs selected
      # by the user on the ui. The name of the element i in "list1" is equal to the name of the element i in the list "lisss"
      list1[[c]]= basicProfile(lisss[[i]], orgPackage =input$select22,onto = input$checkGroup3, level=input$slider)
      names(list1)[i]= names(lisss)[i]      
    }
    return(list1)   
  })
  # The object obtained is dt(). A list of lists containing the basic profiles of one list and for each ontological category selected.
  
  # Tables visualization functional profiles
  
  # The output of the input Verbatimtextoutput() "text" will show the tables of functional profiles
  output$text <- renderPrint({
    # for i, as a posicion from 1 to the length of dt(), from start to end
    for( i in 1:length(dt())){
     local({
       # Print a title for each table as the name of the list i in dt() pasted to input$graphs. Input$graphs is a radio$button widget created before containing all the ontological
       # categories selected so the user can choose of which category see the resulting tables.
        print(paste0(names(dt())[i],"_",input$graphs))
       # Print the the object of dt() in the position i of dt(), and the object included in the element of name "input$graphs" of i, the ontological category selected.
        print(dt()[[i]][[input$graphs]])
        })
      }
  })
  
  # Download basic profiles tables:
  
  # We define that te ouptput of clicking the widget downloadButton named "download3" is a downloadHandler that applies write.xlsx to the results from dt() and print them into a 
  # .txt file called "Bprofiles.txt".
  output$download3 <- downloadHandler(
    filename = function() {"Bprofiles.txt"},
    content = function(file) {
      xlsx::write.xlsx
      sink(file); print(dt()); sink();
    }
  )

  # Plot visualization of functional profiles :
  
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
    for (i in 1:length(dt())){  
      local({ 
        ii <- i
        output[[paste0('plot_',ii)]] <- renderPlot({
            print(plotProfiles(dt()[[ii]][[input$graphs]], aTitle = paste0("Functional Profile:",names(dt())[ii],"_",input$graphs), multiplePlots =TRUE, labelWidth = 100))

        })
      })
    }                                  
  })

# Download basic profiles plots:
#
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
 
  # Calculation of Merged basic profiles:
  
  #  We create a new reactive variable called "m".
  m <- reactive({
    # It requires clicking on action button "run" to proceed with this code.
    req(input$run)
    # We define a new variable "c" with value 0.
    c=0
    # We define dt2 as a variable including the data from dt() in order to null the reactive properties of dt()
    dt2 <- dt()
    # A empty list "list1" is definded.
    list1 <- list()
    # To calculate the merge of the functional profiles, we use the function mergeProfilesLists witch generates a new profile resulting from the merged of two basic profiles.
    # In order to do this, we need to apply this funcion to all unic posible combinations of two of the different basic profiles obtained. 
    # For i iterating through all the elements in dt2 from first to penultimate.
    for( i in 1:(length(dt2)-1)){
      # For j iterating from the element in the position i + 1 to last.
      for(j in (i+1):length(dt2)){
        # At each iteration c is equal to c+1
        c=c+1
        ii <- i
        jj <- j
        # The position "c" of the list1, correspond to the result of applying mergedProfiles two the basic profile of the element "i" and the basic profile of the element "j".
        list1[[c]] = mergeProfilesLists(dt2[[ii]],dt2[[jj]], profNames = c(names(dt2)[ii],names(dt2)[jj]))
        # We define the names of this list in the list as pasting the name that i has on dt2() and the name thas j has on dt2(). In that manner we know of which combination of lists
        # is the merged obtained.
        names(list1)[c]= paste0(names(dt2)[ii],"_",names(dt2)[jj])
      }
    }
    print(list1)
  })
  # The object obtained is m(). A list of lists containing the merged profiles of all unic posible combinations of two of the different basic profiles included in dt(), for each 
  # ontological categorie selected.

# Visualization tables merged
  
# The output of the input Verbatimtextoutput() "text2" will show the tables of the merged functional profiles  
output$text2 <- renderPrint({
  # for i, as a posicion from 1 to the length of m(), from start to end
  for( i in 1:length(m())){
    local({
       # Print a title for each table as the name of the list i in m() pasted to input$graphs. Input$graphs is a radio$button widget created before containing all the ontological
       # categories selected so the user can choose of which category see the resulting tables.
      print(paste0(names(m())[i],"_",input$graphs))
      # Print the the object of m() in the position i of m(), and the object included in the element of name "input$graphs" of i, the ontological category selected.
      print(m()[[i]][[input$graphs]])
    })
  }
})
  
# Download merged tables:
  
  # We define that te ouptput of clicking the widget downloadButton named "download4" is a downloadHandler that applies write.xlsx to the results from m() and print them into a 
  # .txt file called "merged.txt".
  output$download4 <- downloadHandler(
    filename = function() {"merge.txt"},
    content = function(file) {
      xlsx::write.xlsx
      sink(file); print(m()); sink();
    }
  )
  
# Visualizing merged Plots
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
              print(plotProfiles(m()[[paste0(names(dt())[ii],"_",names(dt())[jj])]][[input$graphs]], aTitle = paste0("Merged:","",names(dt())[ii],"_",names(dt())[jj],"_",input$graphs), multiplePlots =TRUE, labelWidth = 100))
          })
        })
      }
    })
  }
})

#Download merge plots
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

#Euclidean distance test

output$table1 <- renderPrint({
  req(input$run)
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
  
  #Download Euclidean distance test
  output$download5 <- downloadHandler(
    filename = function() {"Euclidean.txt"},
    content = function(file) {
      xlsx::write.xlsx
      sink(file); print(list35); sink();
    }
  )
  
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
          list34[[c]] = fisherGOProfiles(dt()[[j]][[input$fish]], dt()[[l]][[input$fish]], 
                                         basicProfile(intersect(lisss[[j]], lisss[[l]]), idType = input$select2, level = input$slider, onto= input$fish, orgPackage = input$select22)[[input$fish]], method="holm")
          names(list34)[c]= paste0(names(lisss[j]),"_",names(lisss[l]),"_",input$fish)
      }
    }
  }
  print(list34)
  
  #Download Fisher test
  output$download6 <- downloadHandler(
    filename = function() {"Fisher.txt"},
    content = function(file) {
      xlsx::write.xlsx
      sink(file); print(list34); sink();
      }
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
