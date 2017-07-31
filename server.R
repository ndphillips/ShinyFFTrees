library(FFTrees)
library(shinydashboard)

shinyServer(function(input, output, session) {
  
  # ----------------
  # Tab: Introduction
  # ----------------
  {
  output$FFT_loading <- renderText({
    
    output <- paste(
      "install.packages('FFTrees')\n",
      "library('FFTrees')\n",
      "\n",
      "\n",
      "      O      \n",
      "     / \\     \n",
      "    F   O  \n",
      "       / \\   \n",
      "      F   T  \n",
      "\n",
      "\n",
      "# FFTrees v1.3.3\n")
      # "\n",
      # "# Keeping it fast and frugal since 2017\n",
      # "# FFTrees.guide() opens the package guide\n",
      # "# Citation info at citation('FFTrees')")
    
  })
  
  output$FFT_4steps <- renderText({
    
    output <- paste(
      "# Step 0: Install FFTrees from CRAN\n",
      "\n",
      "install.packages('FFTrees')\n",
      "# Step 1: Load the package and open the package guide\n",
      "\n",
      "library('FFTrees')\n",
      "FFTrees.guide()\n",
      "\n",
      "# Step 2: Create FFTs for the heartdisease data\n",
      "heart.fft <- FFTrees(formula = diagnosis ~ .\n",
      "                     data = heart.train,\n", # Training data
      "                     data.test = heart.test\n", # Testing data
      "                     main = 'Heart Disease'\n", # Optional labels
      "                     decision.labels = c('Low-Risk', 'High-Risk')")
    
  })
  }
  
  # ----------------
  # Tab: Select Data
  # ----------------
  {
  
  # Select dataset
  output$dataset <- renderUI({
    
    if(is.null(uploaddata())) {
      
      return(selectInput("dataset", 
                         "Choose A Dataset", 
                         as.list(c("heartdisease", "breastcancer"))))
      
    } else {
      
      return(selectInput("dataset", 
                         "Choose A Dataset", 
                         as.list(c("heartdisease", "breastcancer", input$file1$name))))
      
    }
    
  })
  
  # Select criterion
  output$criterion <- renderUI({
    
    if(is.null(input$dataset)) {return(NULL)} else {
      
      possible.cues <- names(current.data())[sapply(1:ncol(current.data()), FUN = function(x) {
        
        setequal(c(0, 1), unique(current.data()[,x]))
        
      })]
      
      # Move "diagnosis" or "criterion" to the top
      
      ord <- rep(0, length(possible.cues))
      ord[possible.cues %in% c("diagnosis", "criterion")] <- 1
      
      possible.cues <- possible.cues[order(ord, decreasing = TRUE)]
      
      selectInput("criterion", 
                  "Which binary variable is the criterion?", 
                  as.list(possible.cues))
    }
  })
  
  # File upload
  uploaddata <- reactive({
    
    if(is.null(input$file1)) {return(NULL)}
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(input$file1$datapath, header = TRUE, sep = ",")
    
    return(df)
  })
  
  # Define currentdata
  current.data <- reactive({
    
    if(input$dataset %in% c("heartdisease", "breastcancer")) {
      
      return(get(input$dataset))
      
    } else {
      
      return(uploaddata())
      
    }
    
  })
  
  # Data table
  output$tbl <- renderUI({
    
    output$mytbl <- DT::renderDataTable(current.data(), 
                                        options = list(lengthChange = FALSE))
    
    DT::dataTableOutput('mytbl')
    
  })
  
  
  
  output$dataandcriterion <- renderUI({
    
   
    if((is.null(input$dataset) | is.null(input$criterion)) == FALSE) {
    
      return(
        
        list(
          h4(paste("Data:", input$dataset)),
          h4(paste("Criterion:", input$criterion)),
          hr()
        
        )
      )
      
    }
        
      
    })
  
  output$dataandcriterion2 <- renderUI({
    
    
    return(
      
      list(
        h4(paste("Data:", input$dataset)),
        h4(paste("Criterion:", input$criterion)),
        hr()
        
      )
    )
    
    
  })
  
   output$dataandcriterion3 <- renderUI({
    
    if((is.null(input$dataset) | is.null(input$criterion)) == FALSE) {
      
    return(
      
      list(
        h4(paste("Data:", input$dataset)),
        h4(paste("Criterion:", input$criterion)),
        hr()
        
      )
    )
    
    }
    
  })
  
    

  }  

  # ----------------
  # Tab: Create FFTs
  # ----------------
  {
    
  
  # Create FFTrees object
  fft.object <- eventReactive(input$goButton, {
    
    if(all(input$decision.labels == "")) {
      
      decision.labels <- c("False", "True")} else {
        
        decision.labels <- unlist(strsplit(input$decision.labels, ","))}
    
    if(all(input$main == "")) {
      
      main <- c(input$dataset)} else {
        
        main <- input$main}
    
    
    if(is.null(input$dataset)) {data <- heartdisease} else {
      
      data <- current.data()}
    
    if(is.null(input$mytree) == FALSE & input$mytree != "") {my.tree <- input$mytree} else {my.tree <- NULL}
    
    withProgress(message = "Growing Trees...", value = 0, {
      
      incProgress(.5)
      
      my.formula <- as.formula(paste(input$criterion, "~ ."))
      
      #Set randomization seed.
      
      if(input$seed != 0) {
      set.seed(input$seed)
      }
      
      if(input$algorithm %in% c("ifan", "dfan")) {
        
        
        if(input$max.levels > 5) {my.max.levels <- 5}
        if(input$max.levels <= 5) {my.max.levels <- input$max.levels}
        
        
        
      } else {my.max.levels <- input$max.levels}
      
      object <- FFTrees::FFTrees(formula = my.formula, 
                                 data = data, 
                                 max.levels = my.max.levels,
                                 train.p = input$train.p,
                                 algorithm = input$algorithm,
                                 goal = input$goal,
                                 goal.chase = input$goal.chase,
                                 sens.w = input$sens.w,
                                 my.tree = my.tree,
                                 main = main,
                                 decision.labels = decision.labels)
      
      incProgress(1)
      
      
    })
    
    return(object)
    
  })
    
  output$createProgress <- renderUI({
    
    if(input$goButton == 0) {
      
      return(
        
        list(
          
          h3("Create an FFT"),
          p("On the left panel, select an FFT construction algorithm and parameters."),
          p("When you are ready, click Create! to create FFTs")

        )
        
        
      )
      
    } else {return(NULL)}
    
  })
  
  output$createHeader <- renderUI({
    
    if(input$goButton != 0) {
      
      return(
        
        list(
          
          h4("Summary Statistics")
          
        )
        
        
      )
      
    } else {return(NULL)}
    
  })
  
  output$summaryHeader <- renderUI({
    
    if(input$goButton != 0) {
      
      return(
        
        list(
          
          h4("Summary Statistics")
          
        )
        
        
      )
      
    } else {return(NULL)}
    
  })
  
  output$showcodecheck <- renderUI({
    
    # checkboxInput('showcode', 
    #               label = "Show R code?")
    
    if(input$goButton != 0) {

      return(

        list(

          checkboxInput('showcode',
                        label = "Show R code?")

        )


      )

    } else {return(NULL)}
    
  })
  
  output$inwordsHeader <- renderUI({
    
    if(input$goButton != 0) {
      
      return(
        
        list(
          
          h4("FFT #1 'in words':")
        )
        
        
      )
      
    } else {return(NULL)}
    
  })
  
  # Print FFT output
  
  output$printFFTrees <- renderPrint({
    
    print(fft.object())
    
  })
  
  # Print FFT output
  
  output$inwords <- renderText({
    
    output <- inwords(fft.object())$v1
    
    output <- paste(output, collapse = "\n")
    
  })
  
  output$FFTcode <- renderUI({
    
    if(input$goButton > 0) {
    
     if(input$showcode) {
    
    codetext <- paste0(
      
"# -------------------------------------------
# ", input$dataset, ".fft
# ---------------------------------------------

# Step 0: Install the FFTrees package

install.packages('FFTrees')  # Only if FFTrees is not yet installed from CRAN


# Step 1: Load the package
library(FFTrees)
      
      ", ifelse((input$dataset %in% c("breastcancer", "heartdisease") == FALSE), 
                paste0(
                  "# Load the data from an external file
                  ",
                  input$dataset, " <- read.table('", input$dataset, "')"), 
                ""), 
      
"set.seed(", input$seed, ") # For training / test replicability

# Step 2: Create an FFTrees object

", input$dataset, ".fft <- FFTrees(
                      formula = ", input$criterion, " ~. ,
                      data = ", input$dataset, ",
                      train.p = ", input$train.p, ",
                      algorithm = '", input$algorithm, "',
                      sens.w = ", input$sens.w, ",
                      max.levels = ", input$max.levels, ",
                      goal = '", input$goal, "',
                      goal.chase = '", input$goal.chase, "')


# Step 3: Summarise the object

print(", input$dataset, ".fft)  # Print the object
summary(", input$dataset, ".fft)  # Show summary statistics


# Step 4: Visualize

plot(", input$dataset, ".fft)  # Plot the FFT with the best training performance

#
#    O      
#   / \\     
#  F   O  
#     / \\   
#    F   T 
# FFTrees v1.3.3
# 
# Notes: 
#  This code was auto-generated with an experimental feature from 
#   https://econpsychbasel.shinyapps.io/FFTrees_Shiny/ 
#  It is not guaranteed to be accurate.
")
  
    output$code.render <- renderText(codetext)
    
    return(list(
      h4("Here is the R code that generated your FFTrees object:"),
      verbatimTextOutput('code.render')))
     
    }
    
    }
      
  })

}

  # ----------------
  # Tab: Visualize
  # ----------------
  {
  # Which tree input
  output$whichtree <- renderUI({
    selectInput("whichtree", "Which Tree?", as.list(1:nrow(fft.object()$tree.definitions)))
  })
  
  # Main FFT plot
  output$FFTPlotpng <- renderImage({
    
    width  <- session$clientData$output_FFTPlotpng_width
    height <- session$clientData$output_FFTPlotpng_height
    pixelratio <- session$clientData$pixelratio
    
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, 
        width = width*pixelratio * input$width * .5, 
        height = height*pixelratio * input$height * .75, 
        res = 72*pixelratio * input$res * .75)
    
    if(input$what %in% c("tree", "cues")) {
    
  
    if(is.null(input$whichtree)) {whichtree <- 1} else {whichtree <- as.numeric(input$whichtree)}
    
    if(all(input$main2 == "")) {main <- fft.object()$params$main} else {main <- input$main2}
    if(all(input$label.tree == "")) {label.tree <- NULL} else {label.tree <- input$label.tree}
    if(all(input$label.performance == "")) {label.performance <- NULL} else {label.performance <- input$label.performance}
    
    if(all(input$decision.labels2 == "")) {decision.labels <- fft.object()$params$decision.labels} else {
      
      decision.labels <- unlist(strsplit(input$decision.labels2, ","))}
    
    
    if(is.null(input$cuelabels) | all(input$cuelabels == "")) {cue.labels <- NULL} else {
      
      cue.labels <- unlist(strsplit(input$cuelabels, ","))
    }
    
    plot(fft.object(),
         data = input$trainortest,
         what = input$what,
         tree = whichtree,
         main = main,
         cue.labels = cue.labels,
         label.tree = label.tree,
         label.performance = label.performance,
         decision.labels = decision.labels,
         show.header = input$show.header,
         show.icons = input$show.icons,
         show.confusion = input$show.confusion,
         show.iconguide = input$show.iconguide,
         show.roc = input$show.roc,
         show.levels = input$show.levels
    )
    
    }
    
    if(input$what == "comparison") {
      
      accuracy.comparison <- data.frame(
        
        model = rep(c("FFTrees", "lr", "cart", "rf", "svm") ,4),
        data = rep(c("Training", "Testing"), each = 5, times = 2),
        statistic = rep(c("Absolute Accuracy 'acc'", "Balanced Accuracy 'bacc'"), each = 10),
        acc = c(fft.object()$tree.stats$train$acc[1],
                fft.object()$comp$lr$stats$acc.train,
                fft.object()$comp$cart$stats$acc.train,
                fft.object()$comp$rf$stats$acc.train,
                fft.object()$comp$svm$stats$acc.train,
                fft.object()$tree.stats$test$acc[1],
                fft.object()$comp$lr$stats$acc.test,
                fft.object()$comp$cart$stats$acc.test,
                fft.object()$comp$rf$stats$acc.test,
                fft.object()$comp$svm$stats$acc.test,
                fft.object()$tree.stats$train$bacc[1],
                fft.object()$comp$lr$stats$bacc.train,
                fft.object()$comp$cart$stats$bacc.train,
                fft.object()$comp$rf$stats$bacc.train,
                fft.object()$comp$svm$stats$bacc.train,
                fft.object()$tree.stats$test$bacc[1],
                fft.object()$comp$lr$stats$bacc.test,
                fft.object()$comp$cart$stats$bacc.test,
                fft.object()$comp$rf$stats$bacc.test,
                fft.object()$comp$svm$stats$bacc.test
                )
      )
      
      yarrr::pirateplot(acc ~ model + data + statistic, 
                        data = accuracy.comparison, bar.f.o = .5,
                        sortx = "s", 
                        bar.f.col = c(yarrr::piratepal("xmen")["green"], rep("white", 4)),
                        bar.b.col = "darkgray", 
                        bar.b.o = 1,
                        ylim = c(.5, 1)
                        )
      
      
      
      
    }
    
    
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = input$size,
         height = input$size,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  # Render FFTPlotpng
  output$FFTPlotpng.ui <- renderUI({
    
      plotOutput("FFTPlotpng")
 
  })
  
  output$plotsimple.ui <- renderUI({
    plotOutput("plotsimple", 
               width = paste0(input$simple.width * 10, "px"), 
               height = input$simple.height * 6)
  })
  
  output$plotsimple <- renderPlot({
    
    plot(fft.object(),
         data = input$trainortest,
         tree = 1,
         main = "",
         label.tree = "",
         label.performance = "",
         decision.labels = c("False", "True"),
         show.header = FALSE,
         show.icons = FALSE,
         show.confusion = FALSE,
         show.iconguide = FALSE,
         show.roc = FALSE,
         show.levels = FALSE
    )
    
    
    
  })
  
  output$visualizeHeader <- renderUI({
    
    if(input$goButton == 0) {
      
      return(
        
        list(
          h3("Waiting for an FFT..."),
          p("It looks like you haven't created an FFT in the Create FFTs tab yet..."),
         img(src = "https://cdn-enterprise.discourse.org/imgur/uploads/default/original/3X/c/4/c41094f8f00d51dce9e00247ed60ae1913c180e0.png", 
             align = 'left', width = "50%")
         
        )
        
        
      )
      
    } else {return(NULL)}
    
  })

  # Download
  {
  output$down <- downloadHandler(
    filename =  function() {
      "FFT.pdf"
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
     
        pdf(file) # open the png device

      if(is.null(input$whichtree)) {whichtree <- 1} else {whichtree <- as.numeric(input$whichtree)}
      
      if(all(input$main2 == "")) {main <- fft.object()$params$main} else {main <- input$main2}
      if(all(input$label.tree == "")) {label.tree <- NULL} else {label.tree <- input$label.tree}
      if(all(input$label.performance == "")) {label.performance <- NULL} else {label.performance <- input$label.performance}
      
      if(all(input$decision.labels2 == "")) {decision.labels <- fft.object()$params$decision.labels} else {
        
        decision.labels <- unlist(strsplit(input$decision.labels2, ","))}
      
      
      plot(fft.object(),
           data = input$trainortest,
           # stats = as.logical(input$stats),
           tree = whichtree,
           main = main,
           label.tree = label.tree,
           label.performance = label.performance,
           decision.labels = decision.labels,
           show.header = input$show.header,
           show.icons = input$show.icons,
           show.confusion = input$show.confusion,
           show.iconguide = input$show.iconguide,
           show.roc = input$show.roc,
           show.levels = input$show.levels
      )
      dev.off()  # turn the device off
      
    } 
  )
  }
}
  
  # ----------------
  # Tab: Code
  # ----------------
  {
  output$code <- renderText({
    
    
    
    paste0(
      
      "library(FFTrees)
      
      ", ifelse((input$dataset %in% c("breastcancer", "heartdisease") == FALSE), 
                paste0(
                  "# Load the data
                  ",
                  input$dataset, " <- read.table('", input$dataset, "')"), 
                ""), 
      
      "
      
      set.seed(", input$seed, ") # For training / test replicability
      
      ", input$dataset, ".fft <- FFTrees(
      formula = ", input$criterion, " ~. ,
      data = ", input$dataset, ",
      train.p = ", input$train.p, ",
      algorithm = '", input$algorithm, "',
      sens.w = ", input$sens.w, ",
      max.levels = ", input$max.levels, ",
      goal = '", input$goal, "',
      goal.chase = '", input$goal.chase, "')
      
      
      # Print the object
      print(", input$dataset, ".fft)
      
      # Get summary statistics
      summary(", input$dataset, ".fft)
      
      # Visualize the FFT
      plot(", input$dataset, ".fft)
      
      
      ")
    
  })
  }
  
  # ----------------
  # Tab: Learn More
  # ----------------
  
  output$learndisplay <- renderUI({
    
    if(input$whichlearn == "history") {
      
    return(
          
          list(
            h3("Fast-and-frugal trees (FFT)"),
            p("A fast-and-frugal tree (FFT) was defined by Martignon et al. (2008) as a decision tree with 
              exactly two branches from each node, where one branch (or in the case of the final node, 
              both branches) is exit branch leading to a terminal leaf."),
            p("FFTs have been successfully used to both describe decision processes and to provide prescriptive 
              guides for effective real-world decision making in a variety of domains, including medical 
              (Fischer et al., 2002; Jenny, Pachur, Williams, Becker & Margraf, 2013; Super, 1984; Wegwarth, 
              Gaissmaier & Gigerenzer, 2009), legal (Dhami, 2003; Dhami & Ayton, 2001; Dhami & Harries, 2001), 
              financial (Aikman et al., 2014; Woike, Hoffrage & Petty, 2015) and managerial (Luan & Reb, 2017) decision making.")
          )
        )
      
    }
    
    if(input$whichlearn == "accuracy") {
      
      return(
        
        list(
          h3("Accuracy Statistics"),
          p("coming soon..."),
          img(src = "confusiontable_png.png")
        )
      )
      
    }
    
    if(input$whichlearn == "algorithms") {
      
      return(
        
        list(
          h3("Algorithms"),
          p("A fast-and-frugal tree construction algorithm accomplishes the following tasks (not necessarily
            in this order:"),
          HTML("<ol><li>Select cues</li><li>Determine the order of cues</li><li>Determine a decision threshold for each cue</li><li>Determine the exit (positive or negative) for eaach cue</li></ol"),
          p("FFTrees contains four different tree construction algorithms that solve these four tasks in different ways.
            Here is a brief description of how the four different algorithms in FFTrees solve these tasks.
            For a full description, consult Martignon et al. (2008) and Phillips et al. (2017)."),
          h4("Max and Zig-zag (Martignon et al., 2008)"),
          p("The Max and Zig-Zag algorithms were created by Martignon and colleagues (2008) as extremely simple
            algorithms that could in principle be applied 'in the head' of a person with a pencil, paper
            and calculator."),
          p("They begin by calculating a decision threshold for each cue. For numeric cues, the median
            value is used, while for nominal cues, dummy coded values of individual cue values are used."),
          p("For the Max algorithm, decision thresholds are then ranked according to the maximum value of their positive predictive
            value (ppv) and negative predictive value (npv). Finally, each cue is given the exit corresponding to whether
            their negative or positive predictive values are higher."),
          h4("ifan and dfan (Phillips et al., 2017)"),
          p("The ifan and dfan algorithms were created by Phillips and colleagues (2017). They were inspired by
             max and zig-zag, but are more flexible and computationally demanding than max and zig-zag."),
          p("The ifan algorithm works as follows. First, a decision threshold is calculated for each cue that maximizes
            the goal.chase statistic (default is 'bacc'). Next, cues are rank ordered by goal.chase. The top
            max.levels cues are then selected and all remaining cues are discarded. The ifan algorithm then creates
            a 'fan' of all possible trees that could be created from those those max.levels cues. 
            Finally, the tree with the highest value of the goal statistic (default is 'bacc') is selected as the
            final tree."),
          p("The dfan algorithm works similarly to ifan. However, rather than calculating decision thresholds for each 
            cue only once based on all data, it recursively calculates new thresholds for cues based on unclassified
            cases as the trees are grown. Thus, dfan tries to optimize decision thresholds for different subsets
            of cases. For this reason it is computationally much more demanding than the other three algorithms."),
          h4("References"),
          p("Martignon, L., Katsikopoulos, K. V., & Woike, J. K. (2008). Categorization with limited resources: A family of simple heuristics. Journal of Mathematical Psychology, 52(6), 352–361."),
          HTML("<p>Phillips, N, D., Neth, Hansjoerg, Woike, J. K., & Gaissmaier, W. (2017). FFTrees: A FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees. Judgment and Decision Making, 12(4), 344-368. <a href = http://journal.sjdm.org/17/17217/jdm17217.pdf>PDF</a></p>")
        ))
      
    }
    
    if(input$whichlearn == "code") {
      
      return(
        
        list(
          h3("Application Code"),
          p("This application was written in R Shiny (Shiny Link)"),
          HTML("<p>Source code is available at LINK</p>")
        )
      )
      
    }
    
    if(input$whichlearn == "references") {
      
      return(
        
        list(
          h3("References"),
          p("The following are a list of papers relevant to fast-and-frugal trees."),
          HTML("<ul><li>Source code is available at LINK</li></ul>")
        )
      )
      
    }
    
  })
})