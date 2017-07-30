library(FFTrees)

shinyServer(function(input, output, session) {
  
  # Define FFTrees object
  
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
      set.seed(input$seed)
      
    object <- FFTrees::FFTrees(formula = my.formula, 
                               data = data, 
                               max.levels = input$max.levels,
                               train.p = input$train.p,
                               algorithm = input$algorithm,
                               goal = input$goal,
                               goal.chase = input$goal.chase,
                               my.tree = my.tree,
                               main = main,
                               decision.labels = decision.labels)
    
    incProgress(1)
    
    
    })
    
    return(object)
    
  })

  # File upload

  uploaddata <- reactive({

    if(is.null(input$file1)) {return(NULL)}
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(input$file1$datapath, header = TRUE, sep = ",")


    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.

    # updateSelectInput(session, inputId = 'dataset', selected = "upload",
    #                   choices)
    # updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
    #                   choices = names(df), selected = names(df)[2])

    return(df)
  })
  
  
  output$FFT_loading <- renderText({
    
    output <- paste(
    "install.packages('FFTrees')\n",
    "library('FFTrees')\n",
    "\n",
    "FFTrees Loading Message...\n",
    "\n",
    "   O      \n",
    "  / \\     \n",
    " F   O  \n",
    "    / \\   \n",
    "   F   T  \n",
    "\n",
    "FFTrees v1.3.3\n",
    "\n",
    "Keeping it fast and frugal since 2017\n",
    "FFTrees.guide() opens the package guide\n",
    "Citation info at citation('FFTrees')")
    
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
  
  # Define currentdata
  current.data <- reactive({
    
    if(input$dataset %in% c("heartdisease", "breastcancer")) {
      
      return(get(input$dataset))
      
    } else {
      
        
        return(uploaddata())
        
      
    }
    
  })
  

  # Data table
  output$tbl <- DT::renderDataTable(current.data(), 
                                    options = list(lengthChange = FALSE))
  
  
  
  
  # Main FFT plot
  output$FFTPlotpng <- renderImage({
    
    width  <- session$clientData$output_FFTPlotpng_width
    height <- session$clientData$output_FFTPlotpng_height
    pixelratio <- session$clientData$pixelratio
    
    
    outfile <- tempfile(fileext='.png')
    
    # Generate the PNG
    png(outfile, 
        width = width*pixelratio * input$width * .5, 
        height = height*pixelratio * input$height * .75, 
        res = 72*pixelratio * input$res * .75)
    
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
         # stats = as.logical(input$stats),
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
    
    
    
    
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = input$size,
         height = input$size,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$FFTPlotpng.ui <- renderUI({
    
    # plotOutput("FFTPlot.png", width = paste0(input$width, "%"), height = input$height)
    plotOutput("FFTPlotpng")
    
    
  })
  
  output$FFTPlot <- renderPlot({

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
         # stats = as.logical(input$stats),
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
     

    
  })
  
  
#   output$FFTplot.Simple <- renderPlot({
#     
#     plot(fft.object(),
#          data = input$trainortest,
#          tree = 1,
#          main = "",
#          label.tree = "",
#          label.performance = "",
#          decision.labels = c("False", "True"),
#          show.header = FALSE,
#          show.icons = FALSE,
#          show.confusion = FALSE,
#          show.iconguide = FALSE,
#          show.roc = FALSE,
#          show.levels = FALSE
#     )
# 
# 
# 
# 
#   }, 
#   width = 600, #input$simple.width, 
#   height =  400 #,input$simple.height)
# )
#   
  
  
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
  
  
  output$createProgress <- renderUI({
    
    input$goButton
    
    if(is.null(fft.object())) {return(HTML("Click Go to create FFTs"))} else {
      
      return(HTML("<h3>Here is a summary of the FFT(s)</h3>"))
      
    }
    
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
  
  
  output$code <- renderText({
    
    

paste0(

"library(FFTrees)
   
", ifelse((input$dataset %in% c("breastcancer", "heartdisease") == FALSE), 
           paste0(input$dataset, " <- read.table(", input$dataset, ")"), 
           ""), 

"

 set.seed(", input$seed, ") # For training / test replicability
    
 ", input$dataset, ".fft <- FFTrees(
         formula = ", input$criterion, " ~. ,
         data = ", input$dataset, ",
         train.p = ", input$train.p, ",
         algorithm = '", input$algorithm, "',
         max.levels = ", input$max.levels, ",
         goal = '", input$goal, "',
         goal.chase = '", input$goal.chase, "')

")
    
  })
  
  
  output$testplot <- renderPlot({
    
    plot(1)
    
  })
  
  output$whichtree <- renderUI({
    selectInput("whichtree", "Which Tree?", as.list(1:nrow(fft.object()$tree.definitions)))
  })
  
  
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
  
  output$criterion <- renderUI({
    selectInput("criterion", 
                "Which is the criterion?", 
                as.list(names(current.data())[sapply(1:ncol(current.data()), FUN = function(x) {
                  
                  setequal(c(0, 1), unique(current.data()[,x]))
                  
                })]))
  })

  
  # Download
  
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


})