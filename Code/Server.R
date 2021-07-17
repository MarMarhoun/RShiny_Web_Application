library(shiny)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(rpart)
library(rpart.plot)
library(e1071)
library(shinythemes)




########################################################

shinyServer(function(input, output, session) {
  
  
  source("Login.R",  local = TRUE)
  
  output$ui <- renderUI({
    if (USER$Logged == FALSE) {
      ##### UI code for login page
      fluidPage(theme = shinytheme("slate"),
        
        tagList(
          tags$head(
            tags$link(rel="stylesheet", type="text/css",href="style.css")
          )
        ),
        
        div(
          column(width = 4, offset = 4,
                 br(), br(), br(), br(),
                 br(), br(), br(), br(),
                 class = "login",
            uiOutput("uiLogin"),
            textOutput("pass"),
            tags$head(tags$style("#pass{color: red;"))
        ))
        
      )
    } else {
      
      fluidPage(shinythemes::themeSelector(),
        fluidRow(
          column(3,
                 div(class = "span1",      
                     uiOutput("obs")
                 )
          ),
          column(8,
                 div(class = "logininfo",
                     uiOutput("userPanel")
                 )
          )
          
        ),
        navbarPage("Welcome",
                   tabPanel("File Input",
                            sidebarLayout(
                              sidebarPanel(
                                fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
                                tags$hr(),
                                h5(helpText("Select the read.table parameters below")),
                                checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                br(),
                                radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ''),
                                radioButtons(inputId = 'dec', label = 'Decimal Numbers', choices = c(Comma=',',Point='.'), selected = '.')
            
            ),
          mainPanel(p("A web application made using ShinyR which is used to demonstrate the working of three famous algorithms
            called" ,strong(a("Decision Tree, SVM, and ACP" )), "and shows the effect of how the hyperparameters effect its classification perfomance."),
                    
            uiOutput("tb"))
            
            # use below code if you want the tabset programming in the main panel. If so, then tabset will appear when the app loads for the first time.
            #       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
            #                   tabPanel("Data", tableOutput("table")))
          )
                   ),

        
          tabPanel("Machine Learning",  
                   tabPanel("Algorithm",
                              sidebarPanel(
              # Drop down menu to select sample size
            # Will have mean and standard deviation choices if user inputs normal distribution
            conditionalPanel(condition = "input.Algorithm == 'ACP'",
                             h4("Please modify your data for ACP:"),
                             hr("Remember that PCA can only be used with quantitative data, indicate the variables used for PCA below:"),
                             h6("(All variables will be included by default if leave a blank)"),
                             textInput('variable','Enter a vector (comma delimmited) indicating the variables used for PCA (quantitative only)',
                                       ""),
                             h4("Eigenvalues"),
                             numericInput("nPC", "Number of PCs", min = 2, value =  5),
                             checkboxInput('standardize', label = 'standardize',value = TRUE),
                             h4("Individuals"),
                             numericInput("xpc3", "PC on X-axis", min = 1, value = 1),
                             numericInput("ypc3", "PC on Y-axis", min = 1, value = 2),
                             h4("Variables"),
                             numericInput("xpc4", "PC on X-axis", min = 1, value = 1),
                             numericInput("ypc4", "PC on Y-axis", min = 1, value = 2),
                             br(),
                             radioButtons('type1', 'Choose the type', choices = list('png', 'pdf'), selected = 'png')
            
            ),
             
                             
            # Will have lambda values if inputs exponential distribution
            conditionalPanel(condition = "input.Algorithm == 'Decision Tree'",
                             h4("Please modify your data for Decision Tree:",
                                #selectInput('response', 'Response variable', attributes),
                                hr(),
                                h6("Please look at your data and choose your class variable:"),
                                selectInput("columns", "Select Columns", choices = colnames(data)),                               
                                numericInput("minsplit", "Minimum number of obs in a node for a split", min = 1, value = 20),
                                numericInput("maxdepth", "Maximum depth", min = 0, value = 30),
                                sliderInput("margin", "Margin of tree plot", min = 0, max = 1, step = 0.01, value = 0.05),
                                sliderInput("branch", "Branch of tree plot", min = 0, max = 1, step = 0.1, value = 1),
                                checkboxInput("use.n", "Displays the number of observations of each class"),
                                sliderInput("minbranch", "The minimum height between levels", min = 0, max = 50, step = 1, value = 10)
                                ,
                                br(),
                                radioButtons('type2', 'Choose the type', choices = list('png', 'pdf'), selected = 'png'))
                                ),
          conditionalPanel(condition = "input.Algorithm == 'SVM'",
                           h4("Please modify your data for SVM:"),
                           br(),
                           selectInput("class", "Class Variable",choices = colnames(data)[5]),
                           selectInput("xcl", "Variable x",choices = colnames(data)[2]),
                           selectInput("ycl", "Variable y",choices = colnames(data)[3]),
                           selectInput("kernel", "kernel",
                                       c(Linear = "linear",
                                         polynomial = "polynomial",
                                         Radial = "radial",
                                         Sigmoid = "sigmoid")),
                           uiOutput("Ker"),
                           br(),br(),
                           radioButtons('type3', 'Choose the type', choices = list('png', 'pdf'), selected = 'png')
          
                           
                            
                           )
          )
          ,
          mainPanel(
           
            
            tabsetPanel(type = "tabs",
                        #####
                        tabPanel("ACP", value = "ACP",
                                 fluidRow(
                                   
                                   column(12,
                                          h2("How it works",align="left"),
                                          p("Principal component analysis (PCA) is a statistical procedure that 
                                            uses an orthogonal transformation to convert a set of observations of 
                                            possibly correlated variables (entities each of which takes on various 
                                            numerical values) into a set of values of linearly uncorrelated variables 
                                            called principal components.")
                                          
                                   )),
                                 h1("Data modified",align ="left"),
                                 dataTableOutput("Table"),
                                 h4("Summary of ACP",align ="left"),
                                 verbatimTextOutput("ACP_summary"),
                                 
                                 h1("Eigenvalues",align ="center"),
                                 plotOutput("ScreePlot"),
                                 h4("Summary Table"), 
                                 tableOutput("Eigentable"),
                                 
                                 h1("Individuals",align ="center"),
                                 plotOutput("IndPlot"),
                                 
                                 h1("Variables",align ="center"),
                                 plotOutput("VarPlot"),
                                 h4("Contributions"),
                                 tableOutput("Ctrvar"),
                                 downloadButton(outputId = "ACP",label= 'Download the Plot')),
                        #####
                        tabPanel("Decision Tree", value = "Decision Tree",
                                 fluidRow(
                                   
                                   column(12,
                                          h2("How it works",align="left"),
                                          p("A decision tree is a decision support tool representing a set of choices in the graphical form of a tree. The various possible decisions are located at the ends of the branches (the 'leaves' of the tree), and are reached according to decisions taken at each step. It has the advantage of being readable and quick to execute."),
                                          h3("Tuning Parameters",align="left",style="color:black"),
                                          p(" ",strong("max_depth :"),"The first parameter to tune is max_depth. This indicates how deep the tree can be.
                                            The deeper the tree, the more splits it has and it captures more information about the data."),
                                          p(" ",strong("min_samples_split :"),"min_samples_split represents the minimum number of samples required to split an internal node.")
                                          
                                          
                                          )),
                                 h3("Plot of Decision Tree",align ="left"),
                                plotOutput("treeplot"),
                                h4("Summary of Decision Tree",align ="left"),
                                verbatimTextOutput("DT_summary"),
                                downloadButton(outputId = "Decision_Tree",label= 'Download the Plot')),
                        #####
                        tabPanel("SVM", value = "SVM",
                                 fluidRow(
                                   
                                   column(12,
                                          h2("How it works",align="left"),
                                          p("SVM are used in classification problems to sagment and divide the feature space into different classes
                using hyperplanes or separator lines. We try to find a plane which saperates the classes in some feature space Xi."),
                                          h3("Tuning Parameters",align="left",style="color:black"),
                                          p("SVM use 2 different types of hyperparameters which we have to select and cannot be learned via training.
                It uses C known as Regularization parameter which is used to control the Bias and the Variance of the Model.
                Second is Gamma-γ which is also a tuning parameter which determines the smoothness of the Non linear decision boundary.")
                                          
                                          
                                          
                                          
                                   ) 
                                   
                                   
                                 ),
                                 h3("Plot of Support Vector Machine ",align ="left"),
                                 plotOutput("svmplot"),
                                 #h3("Confusion Matrix",align="left",style="color:red"),
                                 #verbatimTextOutput("Conf_matrix"),
                                 
                                 h3("Summary of SVM",align ="left"),
                                 verbatimTextOutput("SVM_summary"),
                                 downloadButton(outputId = "SVM",label= 'Download the Plot')
                                 ),
                        
                        
                        id = "Algorithm"
            )
            
            )
          ))


        ))     
      
  
    }
  })
 
  USER <- reactiveValues(Logged = FALSE , session = session$user) 
  
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1))
      {return()} 
    else{data<-read.table(file=file1$datapath, sep=input$sep, header = input$header, dec = input$dec)
    vars <- names(data)
    col <- colnames(data)
    updateSelectInput(session, "columns","Select Columns", choices = col)
    updateSelectInput(session, "class","Class Variable", choices = col)
    
    updateSelectInput(session,"xcl", "Variable x",choices = col )   
    updateSelectInput(session,"ycl", "Variable x",choices = col ) 
      
   
    
    
    }
    
    if(input$Algorithm == "ACP")
      {
      if(input$variable == ""){
      data_used <- data
    }
    else{
      data_used <- data[, c(as.numeric(unlist(strsplit(input$variable,','))))]
    }
    return(data_used)}
    else{return(data)} 
   
 
  
    
    
      
    
    
    
    
  })
  
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderPrint({
    if(is.null(data())){return ()}
    summary(data())
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
 
  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
    {return ()}
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),tabPanel("Summary", verbatimTextOutput("sum")))
  })
  
  
  # Algorithms
   # ACP

  
  pca<- reactive({
    if(is.null(data())){return ()}


        pca_u<-PCA(data(), scale.unit = input$standardize, ncp = input$nPC)
        return (pca_u)

  })
  
  output$ACP_summary <- renderPrint({
   if (is.null(data())){return ()}
    summary(pca())
  })

  output$Table<- renderDataTable({
    if(is.null(data())){return ()}
    else{
    
    if(input$Algorithm =="ACP")
     {
      data_display<- cbind.data.frame(name = rownames(data()), data())
      return(data_display)
      }
     else { return()}
      }
  })
  
  output$ScreePlot <- renderPlot({
    if(is.null(data())){return ()}
   
      #H <- pca()$eig[,1] 
       barchart<- barplot(pca()$eig[,1], las = 1, border = NA, names.arg = 1:length(pca()$eig[,1]),
                         ylim =c(0, 1.1* ceiling(max(pca()$eig[,1]))), ylab = "value",
                         xlab = "Eigenvalues (associated to the PCs)", main = "Scree plot")
      points(barchart, pca()$eig[,1], pch = 19, col = "gray50")
      lines(barchart, pca()$eig[,1], lwd = 2, col = "gray50")
      
  
  })
  
  output$Eigentable <- renderTable({
    if(is.null(data())){return ()}
    
      eig = cbind.data.frame(name = rownames(pca()$eig), pca()$eig)
      colnames(eig) = c('dim', 'eigenvalue', 'percentage', 'cumulative')
      return(eig)
  
  })
  
  output$IndPlot <- renderPlot({
    if(is.null(data())){return ()}
    
      plot(pca(), axes = c(input$xpc3, input$ypc3) ,choix = "ind")
  
  })
  
  output$VarPlot <- renderPlot({
    if(is.null(data())){return ()}
    
      plot(pca(), axes = c(input$xpc4, input$ypc4), choix = "var")
   
    
  })
  
  output$Ctrvar <- renderTable({
    if(is.null(data())){return ()}
    
      ctr = pca()$var$contrib
      ctr = cbind.data.frame(variable = rownames(ctr), ctr)
      return(ctr)
     
  })
  
  output$ACP <- downloadHandler(
    filename =  function() {
      paste("ACP", input$type1, sep=".") # replace input$var3 by input$type ty Abi
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$type1 == "pdf") # replace input$var3 by input$type ty Abi
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      data_display<- cbind.data.frame(name = rownames(data()), data())
      barchart<- barplot(pca()$eig[,1], las = 1, border = NA, names.arg = 1:length(pca()$eig[,1]),
                         ylim =c(0, 1.1* ceiling(max(pca()$eig[,1]))), ylab = "value",
                         xlab = "Eigenvalues (associated to the PCs)", main = "Scree plot")
      points(barchart, pca()$eig[,1], pch = 19, col = "gray50")
      lines(barchart, pca()$eig[,1], lwd = 2, col = "gray50")
      
      eig = cbind.data.frame(name = rownames(pca()$eig), pca()$eig)
      colnames(eig) = c('dim', 'eigenvalue', 'percentage', 'cumulative')
      
      plot(pca(), axes = c(input$xpc3, input$ypc3) ,choix = "ind")
      
      plot(pca(), axes = c(input$xpc4, input$ypc4), choix = "var")
      
      ctr = pca()$var$contrib
      ctr = cbind.data.frame(variable = rownames(ctr), ctr)
      
      
      
      
      dev.off()  # turn the device off
      
    } 
  )
  
  
  
   # Decision Tree
  tree_model<- reactive({
    if(is.null(data())){return ()}
    dt <-data()
    attach(dt)
    col <- colnames(dt)
    f <- as.formula(paste(paste(input$columns, "~"),paste(col[!col %in% input$columns], collapse = " + ")))
    DT <-rpart(f, data = dt, minsplit = input$minsplit,   maxdepth = input$maxdepth)

    return(DT)
    
  })
  
  output$DT_summary <- renderPrint({
    if(is.null(data())){return ()}
    print(summary(tree_model()))
  })
  
  
  output$treeplot <- renderPlot({
    if(is.null(data())){return ()}
    
    plot(tree_model(), margin= input$margin, branch = input$branch, minbranch = input$minbranch)
    text(tree_model(),  use.n = input$use.n )
    
  })
  
  output$Decision_Tree <- downloadHandler(
    filename =  function() {
      paste("Decision Tree", input$type2, sep=".") # replace input$var3 by input$type ty Abi
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$type2 == "png") # replace input$var3 by input$type ty Abi
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(h2("How it works",align="left"))
      print(p("A decision tree is a decision support tool representing a set of choices in the graphical form of a tree. The various possible decisions are located at the ends of the branches (the 'leaves' of the tree), and are reached according to decisions taken at each step. It has the advantage of being readable and quick to execute."))
            print(h3("Tuning Parameters",align="left",style="color:black"))
            print(p(" ",strong("max_depth :"),"The first parameter to tune is max_depth. This indicates how deep the tree can be.
                                            The deeper the tree, the more splits it has and it captures more information about the data."))
                  print( p(" ",strong("min_samples_split :"),"min_samples_split represents the minimum number of samples required to split an internal node."))
                         print(h2("Plot of Decision Tree ",align="left"))
      plot(tree_model(), margin= input$margin, branch = input$branch, minbranch = input$minbranch)
      text(tree_model(),  use.n = input$use.n )
      
      grid.table(summary(tree_model()))
      dev.off()  # turn the device off
      
    } 
  )

  # SVM
  output$Ker <- renderUI({
    if (is.null(input$kernel))
  return()
  
  switch(input$kernel,
  
  "linear" = tabPanel("Linear", 
  numericInput('C','Training parameter C', value = 1)),
  
  "polynomial" = tabPanel("Polinominal",
  numericInput('C','Training parameter C', value = 1),
  numericInput('gamma','Training parameter gamma', value = 0.25),
  numericInput('coef0','Training parameter coef0', value = 1),
  numericInput('degree','Training parameter degree', value = 3)),
  "radial" = tabPanel("Radial",
  numericInput('C','Training parameter C', value = 1),
  numericInput('gamma','Training parameter gamma', value = 0.25)),
  "sigmoid" = tabPanel("Sigmoid",
  numericInput('C','Training parameter C', value = 1),
  numericInput('gamma','Training parameter gamma', value = 0.25),
  numericInput('coef0','Training parameter coef0', value = 1))
  )
})
  
  SVM_model<- reactive({
    if(is.null(data())){return ()}
    dt <-data()
    attach(dt)
    cols <- colnames(dt)
    form <- as.formula(paste(paste(as.factor(input$class), "~",".")))
        switch(input$kernel,
           "linear" =  svm(form, data=dt, 
                           kernel=input$kernel, cost = input$C),
           
           "polynomial" =  svm(form, data=dt, 
                               kernel=input$kernel, cost = input$C, gamma = input$gamma, coef0 = input$coef0, degree = input$degree),
           
           "radial" = svm(form, data=dt, 
                          kernel=input$kernel, cost = input$C, gamma = input$gamma),
           
           "sigmoid" = svm(form, data=dt, 
                           kernel=input$kernel, cost = input$C, gamma = input$gamma, coef0 = input$coef0))
    #return(cl)
    
  })
  
  output$SVM_summary <- renderPrint({
    if(is.null(data())){return ()}
    print(summary(SVM_model()))
  })

  
  output$svmplot <- renderPlot({
    if(is.null(data())){return ()}
    
    form2 <- as.formula(paste(paste(as.factor(input$xcl)),"~",paste(as.factor(input$ycl))))
    print(plot(SVM_model(),data(),form2))
    
    
  })
  
 output$SVM <- downloadHandler(
    filename =  function() {
      paste("SVM", input$type3, sep=".") # replace input$var3 by input$type ty Abi
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$type3 == "png") # replace input$var3 by input$type ty Abi
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      form2 <- as.formula(paste(paste(as.factor(input$xcl)),"~",paste(as.factor(input$ycl))))
      plot(SVM_model(),data(),form2)
      
      print(summary(SVM_model()))
      
      dev.off()  # turn the device off
      
    } 
  )
  
  
 
    
 
  
})
