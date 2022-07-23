###Load necessary packages
library(shiny, quietly = "T")
library(shinyjs, quietly = "T")
library(tidyverse, quietly = "T")
library(DT, quietly = "T")
library(magrittr, quietly = "T")
library(ggrepel, quietly = "T")
library(shinycssloaders, quietly = "T")
library(shinyalert, quietly = "T")
library(shinymanager, quietly = "T")
library(xml2, quietly = "T")
library(rvest, quietly = "T")
library(shinyalert, quietly = "T")

##js code to open external browsers
js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"

###user interface
ui  <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = js_code, functions = 'browseURL'),
  
  #title
  titlePanel("Differential Gene Expression Analysis"),
  
  #sidebar
  sidebarLayout(
    #side panel
    sidebarPanel(
      width = 3,
      
      #about page
      #Uploaded data section
      conditionalPanel (
        condition = "input.tab == 'About'",
        h5("1. General Information"),
        h5("2. Application Functionality"),
        h5("3. Credits"),
        h5("4. Contact Details")
      ),
      #Uploaded data section
      conditionalPanel (
        condition = "input.tab == 'Data'",
        radioButtons (
          "data_upload",
          "",
          choices =
            list("DESeq2 Results" = 1),
          selected = 1
        )
      ),
      
      #Plot section
      conditionalPanel (
        condition = "input.tab == 'Plot'",
        selectInput("annotations", label = "Plot Annotation", choices = "-"),
        sliderInput("pointSize", "Size Of The Datapoints", 0, 10, 4),
        sliderInput("foldChange", "Fold Change Threshold:", -5, 5, step = 0.1, value = c(-1.5, 1.5)),
        sliderInput("NegLog10PVal", "Significance Value Threshold:", 0, 5, step = 0.1, value = 1.30),
        selectInput("filter", label = "Filter hits:", choices = list("Differentially expressed" = "significant", "Up-regulated only" = "up", "Down-regulated only" = "down"), selected = "significant"),
        numericInput("numOfHits", "Number of hits to label on the plot:", value = 10),
        selectizeInput(
          inputId = "searchGenes",
          label = "Search Genes:",
          choices = "-",
          selected = NULL,
          multiple = TRUE,
          options = list(create = TRUE)
        ),
        checkboxInput(
          inputId = "hide_labels",
          label = "Hide labels in the plot",
          value = FALSE
        ),
        checkboxInput(
          inputId = "add_title",
          label = "Add title",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.add_title == true",
          textInput("title", "Title:", value = "")
        ),
        checkboxInput(
          inputId = "add_subtitle",
          label = "Add subtitle",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.add_subtitle == true",
          textInput("subtitle", "Subtitle:", value = "")
        ),
        checkboxInput(
          inputId = "add_caption",
          label = "Add Plot Caption",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.add_caption == true",
          textInput("caption", "Caption:", value = "")
        ),
        checkboxInput(
          inputId = "label_axes",
          label = "Change axis labels",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.label_axes == true",
          textInput("lab_x", "X-axis:", value = ""),
          textInput("lab_y", "Y-axis:", value = "")
        ),
        checkboxInput(
          inputId = "transformScale",
          label = "Transform axis",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.transformScale == true",
          checkboxInput(inputId = "scale_log_10", label = "Log10 on y-axis", value = FALSE)
        ),
        numericInput("plotHeight", "Plot height in pixels: ", value = 600),
        numericInput("plotWidth", "Plot width in pixels:", value = 800)
      ),
      #Results Section
      conditionalPanel (
        condition = "input.tab == 'Results'",
        selectInput("downloadResults", 
                    label = "Download Results:", 
                    choices = list("All" = "DESeq2_Results",
                                   "Up and Down-regulated" = "DESeq2_Up_and_Down-regulated_Results", 
                                   "Up-regulated only" = "DESeq2_Up-regulated_Only_Results", 
                                   "Down-regulated only" = "DESeq2_Down-regulated_Only_Results"), 
                    selected = "all"
        ),
        actionButton("authenticate", "Click to authenticate"),
        br(),
        br(),
        downloadButton("download", label = "Download CSV")
      ),
      conditionalPanel (
        condition = "input.tab == 'Design Primers'",
        h3("FAQ"),
        p("A primer is a short single-stranded nucleic acid used by all living organism in the initiation of DNA synthesis."),
        h3("Instructions"),
        textOutput("primerInstruction1"),
        textOutput("primerInstruction2"),
        textOutput("primerInstruction3"),
        textOutput("primerInstruction4"),
        textOutput("primerInstruction5"),
        textOutput("primerInstruction6"),
        textOutput("primerInstruction7"),
        textOutput("primerInstruction8"),
        br(),
        selectizeInput(
          inputId = "searchGene",
          label = "Search Gene:",
          choices = "-",
          selected = NULL,
          multiple = FALSE,
          options = list(create = TRUE)
        ),
        actionButton("getPage", "Open jbrowse")
      ),
    ),
    
    #main panel
    mainPanel (
      tabsetPanel (
        id = "tab",
        tabPanel ("About",
                  h3("General Information"),
                  p("This program allows you to explore an interactive volcano plot, 
                  download filtered differential gene expression results, and design 
                  primers. The dataset in use was created using RNA expression measurements 
                  taken from sea urchin embryos at 30 hours post fertilization."),
                  h3("Application Functionality"),
                  tags$ol (
                    tags$li(h4("Data Section")),
                    tags$ul(
                      tags$li("The data table in this section contains the results from the differential 
                        gene expression analysis carried out using the DESeq2 package in R.")
                    ),
                    tags$li(h4("Plot Section")),
                    tags$ul(
                      tags$li("The volcano plot in this section is interactive. Threshold settings and 
                                aesthetic options can both be changed on the left panel of the section.")
                    ),
                    tags$li(h4("Results Section")),
                    tags$ul(
                      tags$li("A data table in this section includes a column that indicates whether a gene is 
                                enriched, depleted or unchanged. The option to download datasets needs a password.")
                    ),
                    tags$li(h4("Design Primers Section")),
                    tags$ul(
                      tags$li("To design primers for a given gene, this section combines Primer3Plus and JBrowse.")
                    ),
                  ),
                  h3("Credits"),
                  tags$ol (
                    tags$li((h4(tags$a(href="https://hardingnj.shinyapps.io/volcanoshiny/", "VolcanoShiny")))),
                    tags$li((h4(tags$a(href="https://jbrowse.org/jb2/", "JBrowse")))),
                    tags$li((h4(tags$a(href="https://www.primer3plus.com/", "Primer3Plus")))),
                  ),
                  h3("Contact Details"),
                  tags$ol (
                    tags$li((h4("Norman Zvenyika: noz224@lehigh.edu"))),
                    tags$li((h4("Dr. J. Barsi: julius.barsi@bios.edu"))),
                  ),
        ),
        tabPanel ("Data",
                  br(), #add some space to move the table down
                  dataTableOutput("uploadedDataSet")),
        tabPanel (
          "Plot",
          textOutput("text"),
          div(
            style = "position:relative",
            plotOutput("VolcanoPlot",
                       height = "auto",
                       dblclick = "volcanoPlotDBClick",
                       brush = brushOpts(
                         "volcanoPlotBrush",
                         resetOnNew = TRUE
                       ),
                       hover = hoverOpts("plotHover", delay = 10, delayType = "debounce"),
                       
            ), 
            uiOutput("hoveringInfo"),
          ),
          downloadButton("downloadPlot", "Download plot (png-file)")
        ),
        tabPanel (
          "Results",
          h3("Results"),
          verbatimTextOutput("code"),
          br(),
          withSpinner(dataTableOutput("results"))
        ),
        tabPanel (
          "Design Primers",
          div (
            style = "position:relative",
            htmlOutput("Primer3Plus")
          ),
        )
      )
    )
  )
)

###server function
server  <- function(input, output, session) {
  #hde the download button
  shinyjs::hide("download")
  
  ##authenticate
  observeEvent(input$authenticate, {
    shinyalert(
      "Enter password", type = "input", 
      callbackR = function(x) {
        if(x == "BIOS2022") {
          shinyalert("Authenticated", type="success")
          shinyjs::show("download")
        }
        else {
          shinyjs::hide("download")
          shinyalert("Wrong password. Try again", type="error")
        }
      }
    )
  })
  
  ##Method for uploadng the dataSet
  dataSetUpload <- reactive ({
    
    ###import the results
    results <- read.csv("DGEResults.csv", na.strings = "")
    
    #upload the results and reformat the column names
    dataSet <<- results %>%
      select_all( ~ gsub("\\s+|\\.", "_", .))
    
    #initialize annotation with Id_Number1
    plotAnnotation <<- "Id_Number1"
    return(dataSet)
  })
  
  ##display the uploaded results as a table
  output$uploadedDataSet <- renderDataTable(
    dataSetUpload(),
    escape=FALSE,
    rownames = FALSE,
    editable = FALSE,
    options = list (
      pageLength = 20,
      autoWidth = FALSE,
      lengthMenu = c(20, 100)
    )
  )
  
  ##Obtain possible variables for annotating the plot
  observe ({
    #select all columns with possible names for labelling the points on the plot
    possibleNames <- names(
      Filter(
        function(x) is.factor(x) || is.logical(x) || is.character(x),
        dataSetUpload()
      )
    )
    
    #remove the jbrowse column so that it is not a possible name
    possibleNames <-  possibleNames[! possibleNames %in% c("Genome_Browser")]
    #update the options for the user to choose (by default, it starts with id selected)
    updateSelectizeInput(session, "annotations", choices = possibleNames, selected = plotAnnotation)
  })
  
  ##User defined information for plotting the volcano plot
  userDefined <- reactive ({
    #user defined variables
    plotAnnotation <- input$annotations
    
    df <- dataSetUpload() 
    
    #select columns of interest from the dataset
    df <- df  %>%
      select(Name,Id_Number1,Id_Number2,baseMean,log10BaseMean,
             log2FoldChange,lfcSE,stat,pvalue,Negative_Log10_P_Value,
             padj,Genome_Browser)
    
    #if user select Id_Number2s for annotation, remove the commas/ other names to label the plot easily
    if(plotAnnotation =="Id_Number2") {
      df <- df %>%
        mutate(Id_Number2 = gsub(";.*", "", Id_Number2))
      updateSelectizeInput(session, "searchGenes", choices = df$Id_Number2,server=TRUE)
      updateSelectizeInput(session, "searchGene", choices = df$Id_Number2,server=TRUE)
    }
    
    #if the user search for some some genes, update the geneList using the annotation option
    if(plotAnnotation == "Id_Number1") {
      updateSelectizeInput(session, "searchGenes", choices = df$Id_Number1, server=TRUE)
      updateSelectizeInput(session, "searchGene", choices = df$Id_Number1, server=TRUE)
    }
    
    #if the user search for some some genes, update the geneList using the annotation option
    if(plotAnnotation == "Name") {
      updateSelectizeInput(session, "searchGenes", choices = df$Name, server=TRUE)
      updateSelectizeInput(session, "searchGene", choices = df$Name, server=TRUE)
    }
    
    
    #get the fold change values from the slider
    fcMin <- input$foldChange[1]
    fcMax <- input$foldChange[2]
    
    #get the pvalues from the slider
    pval <- input$NegLog10PVal
    
    #add the Differentially_Expressed column to the df
    if(input$filter == "down") {
      df <- df %>% mutate(
        Differentially_Expressed = case_when(
          log2FoldChange < fcMin & Negative_Log10_P_Value > pval ~ "Down-regulated",
          TRUE ~ "Unchanged"
        )
      )
    }
    else if (input$filter == "up") {
      df <- df %>% mutate(
        Differentially_Expressed = case_when(
          log2FoldChange > fcMax & Negative_Log10_P_Value > pval ~ "Up-regulated",
          TRUE ~ "Unchanged"
        )
      )
    }
    else if (input$filter == "significant") {
      df <- df %>% mutate(
        Differentially_Expressed = case_when(
          log2FoldChange > fcMax & Negative_Log10_P_Value > pval ~ "Up-regulated",
          log2FoldChange < fcMin & Negative_Log10_P_Value > pval ~ "Down-regulated",
          TRUE ~ "Unchanged"
        )
      )
    }
    
    #arrange the columns
    df <- df %>%
      select(Name,Id_Number1,Id_Number2,log2FoldChange,Negative_Log10_P_Value,Differentially_Expressed,Genome_Browser)
    
    return(df)
  })
  
  ##get a summary of the results (number of up/down/unchanged genes)
  summaryOfResults <-  reactive({
    userDefinedInfo <-  userDefined()
    table(userDefinedInfo$Differentially_Expressed)
  })
  
  #Method to filter the genes according to their classification
  topHits <- reactive({
    
    #call the filtered dataframe
    df <- userDefined()
    
    #filter the df according to the user selection
    if(input$filter == "up") {
      df <- df %>%
        filter(Differentially_Expressed == "Up-regulated")
    }
    else if (input$filter == "down") {
      df <- df %>%
        filter(Differentially_Expressed == "Down-regulated")
    }
    else if (input$filter == "significant"){
      df <- df %>%
        filter(Differentially_Expressed != "Unchanged")
    }
    
    #arrange by descending negative p-values
    df <- df %>%
      arrange(desc(Negative_Log10_P_Value)) %>%
      top_n(input$numOfHits,Negative_Log10_P_Value)
    
    #check for duplicates and remove them if present
    if(plotAnnotation == "Id_Number1") {
      df <- bind_rows(df,userSearchGenes()) %>%
        distinct(Id_Number1, .keep_all = TRUE)
      return(df)
    }
    else if(plotAnnotation == "Id_Number2"){
      df <- bind_rows(df,userSearchGenes()) %>%
        distinct(Id_Number2, .keep_all = TRUE)
      return(df)
    }
    else {
      df <- bind_rows(df,userSearchGenes()) %>%
        distinct(Name, .keep_all = TRUE)
      return(df)
    }
  })
  
  ##Method to allow the user to search genes
  userSearchGenes <- reactive({
    #call he user definedInfor
    df <- as.data.frame(userDefined())
    
    #get the list of userSearchGenes
    userGenesList <- input$searchGenes
    plotAnnotation <- input$annotations
    #filter genes according to the user selected genes
    if(plotAnnotation == "Id_Number1") {
      df <- df %>%
        filter(Id_Number1 %in% userGenesList)  #if the user is using the Id_Number1 for annotations
    }
    else if(plotAnnotation == "Id_Number2") {
      df <- df %>%
        filter(Id_Number2 %in% userGenesList) #if the user is using the Id_Number2s for annotations
    }
    else {
      df <- df %>%
        filter(Name %in% userGenesList) #if the user is using the Id_Number2s for annotations
    }
    return(df)
  })
  
  ##Method for preparing the volcano plotting
  plotInfor <- reactive ({
    
    #convert the user definedInfo into a data frame
    userDefinedDf <<- as.data.frame(userDefined())
    plotAnnotation <- input$annotations
    
    #make the Differentially_Expressed column to be factors for getting the legend right
    userDefinedDf$Differentially_Expressed <<- factor(userDefinedDf$Differentially_Expressed, levels = c("Unchanged", "Up-regulated","Down-regulated"))
    
    #set of colors to use for labels
    colors <- c("grey","red","blue")
    
    #plot the volcano plot
    p <- ggplot(data = userDefinedDf) +
      aes(x = log2FoldChange) +
      aes(y = Negative_Log10_P_Value) +
      aes(color = Differentially_Expressed) +
      scale_color_manual(values = colors) +
      geom_point(size = input$pointSize, shape = 16) +
      theme_light(base_size = 16)
    
    #set cut-off lien color
    cutOffLineColor <- "gray20"
    
    #add vlines to indicate the cutoffs
    if (input$filter == "up" || input$filter == "significant") {
      p <- p + geom_vline(xintercept = input$foldChange[2], linetype = "dashed", color = cutOffLineColor)
    }
    if (input$filter == "down" || input$filter == "significant") {
      p <- p + geom_vline(xintercept = input$foldChange[1], linetype = "dashed", color = cutOffLineColor)
    }
    
    #add hlines to the plot
    p <- p + geom_hline(yintercept = input$NegLog10PVal, linetype="dashed", color = cutOffLineColor) 
    
    #add labels to the points on the plot
    if (input$hide_labels == FALSE && plotAnnotation == "Id_Number1") {
      p <- p + 
        geom_point(data=topHits(), 
                   aes(x = log2FoldChange, 
                       y = Negative_Log10_P_Value),
                   shape=1,
                   color=cutOffLineColor, 
                   size=(input$pointSize)
        )+
        geom_text_repel(
          data = topHits(),
          aes(label = Id_Number1),
          size = 6,
          color = cutOffLineColor,
          nudge_x = 0.2,
          nudge_y = 0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3 + input$pointSize * 0.1, "lines")
        )
      p <-  p + 
        geom_point(data=userSearchGenes(), 
                   aes(x=log2FoldChange,y=Negative_Log10_P_Value), 
                   shape=16,
                   color=colors[4], 
                   size=(input$pointSize))   
    }
    else if (input$hide_labels == FALSE && plotAnnotation == "Id_Number2"){
      p <- p + 
        geom_point(data=topHits(), 
                   aes(x = log2FoldChange, 
                       y = Negative_Log10_P_Value),
                   shape=1,
                   color=cutOffLineColor, 
                   size=(input$pointSize)
        )+
        geom_text_repel(
          data = topHits(),
          aes(label = Id_Number2),
          size = 6,
          color = cutOffLineColor,
          nudge_x = 0.2,
          nudge_y = 0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3 + input$pointSize * 0.1, "lines")
        )
      p <-  p + 
        geom_point(data=userSearchGenes(), 
                   aes(x=log2FoldChange,y=Negative_Log10_P_Value), 
                   shape=16,
                   color=colors[4], 
                   size=(input$pointSize)) 
    }
    else if (input$hide_labels == FALSE && plotAnnotation == "Name"){
      p <- p + 
        geom_point(data=topHits(), 
                   aes(x = log2FoldChange, 
                       y = Negative_Log10_P_Value),
                   shape=1,
                   color=cutOffLineColor, 
                   size=(input$pointSize)
        )+
        geom_text_repel(
          data = topHits(),
          aes(label = Name),
          size = 6,
          color = cutOffLineColor,
          nudge_x = 0.2,
          nudge_y = 0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3 + input$pointSize * 0.1, "lines")
        )
      p <-  p + 
        geom_point(data=userSearchGenes(), 
                   aes(x=log2FoldChange,y=Negative_Log10_P_Value), 
                   shape=16,
                   color=colors[4], 
                   size=(input$pointSize))
    }
    
    #from https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
    #add the option of a zoomable plot
    p <- p + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)
    
    #check if log10 scale is required on the y axis
    if (input$scale_log_10) {
      p <- p + scale_y_log10() 
    }
    
    #add title option
    if (input$add_title == TRUE) {
      title <- paste(input$title, "\n", sep = "")
      p <- p + 
        labs (title = title) +
        theme(plot.title = element_text(color = "black", size = 25, face = "bold", hjust = 0.5, lineheight = 0.1))
    }
    
    #add sub-title option
    if (input$add_subtitle == TRUE) {
      subtitle <- paste(input$subtitle, "\n", sep = "")
      p <- p + 
        labs(subtitle = subtitle) +
        theme(plot.subtitle = element_text(color = "black", hjust = 0.5, vjust = -0.5))
    }
    
    #add plot option
    if (input$add_caption == TRUE) {
      caption <- paste(input$caption, "\n", sep = "")
      p <- p + 
        labs(caption = caption) +
        theme(plot.caption = element_text(color = "black", face = "italic", vjust = -3))
    }
    
    
    
    #add labels option
    if (input$label_axes){
      p <- p + labs(x = input$lab_x, y = input$lab_y)}
    else {
      p <- p + labs(x=bquote(''*Log[2]*' Fold Change'), y=bquote(''*-Log[10]*' P-Value'))
    }
    
    #plot
    p
  })
  
  #Instructions on how to interact with the plot
  output$text <- renderText("To zoom-in on a specific area, highlight the area, and double click the highlighted area. Double click again to zoomout.")
  
  ##ranges for the zoomable plot
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  ##on double click, check for a highlighted area. If present, zoom on the plot. If not, reset the zoom
  observeEvent(input$volcanoPlotDBClick, {
    brush <- input$volcanoPlotBrush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } 
    else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  ##set plot area width
  width <- reactive({
    input$plotWidth
  })
  
  ##set plot area height
  height <- reactive({
    input$plotHeight
  })
  
  ##Print the summary of results
  output$code <- renderPrint(summaryOfResults())
  
  ##ouput the plot
  output$VolcanoPlot <- renderPlot(width = width, height = height, {
    plotInfor()
  })
  
  ##hover over the points infor
  output$hoveringInfo <- renderUI ({
    df <- as.data.frame(userDefined())
    plotAnnotation <- input$annotations
    hover <- input$plotHover
    point <- nearPoints(df, hover, threshold = 10, maxpoints = 1, addDist = FALSE)
    
    if (nrow(point) == 0) {
      return(NULL)
    }
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    style <- paste0(
      "position:absolute;
                  padding: 5px;
                  z-index:100; background-color: rgba(233, 233, 247, 1); ",
      "left:", left_px / 5, "px; top:", top_px / 5, "px;"
    )
    
    # actual tooltip created as wellPanel
    if(plotAnnotation == "Id_Number1") {
      wellPanel(
        style = style,
        p(HTML(paste0(
          "<b> Id_Number1: </b>", point$Id_Number1, "<br/>",
          "<b> Log 2 Fold change: </b>", round(point[4], 2), "<br/>",
          "<b> -Log 10 P-Val: </b>", round(point[5], 2), "<br/>",
          NULL
        )))
      )
    }
    else if (plotAnnotation == "Id_Number2"){
      wellPanel(
        style = style,
        p(HTML(paste0(
          "<b> Id_Number2: </b>", point$Id_Number2, "<br/>",
          "<b> Log 2 Fold change: </b>", round(point[4], 2), "<br/>",
          "<b> -Log 10 P-Val: </b>", round(point[5], 2), "<br/>",
          NULL
        )))
      )
    }
    else {
      wellPanel(
        style = style,
        p(HTML(paste0(
          "<b> Name: </b>", point$Name, "<br/>",
          "<b> Log 2 Fold change: </b>", round(point[4], 2), "<br/>",
          "<b> -Log 10 P-Val: </b>", round(point[5], 2), "<br/>",
          NULL
        )))
      )
    }
    
  })
  
  #Method to download plots
  output$downloadPlot <- downloadHandler (
    #set filename
    filename <- function() {
      paste("VolcanoPlot.png")
    },
    
    #content of the file
    content <- function(file) {
      png(
        file,
        width = input$plotWidth * 4,
        height = input$plotHeight * 4,
        res = 300
      )
      plot(plotInfor())
      dev.off()
    },
    
    #set content type
    contentType = "application/png"
  )
  
  
  ##Ouput a table with results
  output$results <- renderDataTable (
    userDefined(),
    escape=FALSE,
    width = 40,
    rownames = FALSE,
    options = list(dom = 'Blfrtip', 
                   autoWidth = TRUE,
                   columnDefs = list(list(width = "5px", targets = "_all")),
                   lengthMenu = c(20, 50, 100)),
    editable = FALSE,selection = 'none'
  )
  
  ##filer results according to the user
  filterResults <- reactive({
    df <- userDefined()
    option <- input$downloadResults
    
    if(option == "DESeq2_Results") {
      return(df)
    }
    else if (option == "DESeq2_Up_and_Down-regulated_Results") {
      df <- df %>%
        filter(Differentially_Expressed != "Unchanged")
    }
    else if (option == "DESeq2_Up-regulated_Only_Results") {
      df <- df %>%
        filter(Differentially_Expressed == "Up-regulated")
      return(df)
    }
    else if (option == "DESeq2_Down-regulated_Only_Results") {
      df <- df %>%
        filter(Differentially_Expressed == "Down-regulated")
      return(df)
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      option <- input$downloadResults
      paste(`option`,'.csv', sep='')
    },
    content = function(file) {
      write.csv(filterResults(),file, row.names=FALSE)
    }
  )
  
  
  
  
  #instructions on how to get primers
  output$primerInstruction1 <- renderText("1. Select a gene on using the Search Gene option below")
  output$primerInstruction2 <- renderText("2. Click Open jbrowse button. This will redirect you to another window with jbrowse")
  output$primerInstruction3 <- renderText("3. Click the gene Id_Number1/Id_Number2 in the jbrowse window")
  output$primerInstruction4 <- renderText("   * A pop-out window with details about the transcript should appear")
  output$primerInstruction5 <- renderText("4. Copy the sequence or download the FastA file you want to use for designing the primers")
  output$primerInstruction6 <- renderText("5. Return to this page, and paste the sequence or upload the FastA file in the Primer 3 Plus window")
  output$primerInstruction7 <- renderText("6. Click Pick primer to get the primer using default settings/conditions")
  output$primerInstruction8 <- renderText("   * You can adjust primer design conditions.")
  
  
  #view a gene on jbrowse
  userSearchGene <- reactive({
    #call he user definedInfor
    df <- as.data.frame(userDefined())
    
    userGene <- input$searchGene
    
    #search for the selected gene link
    if(plotAnnotation == "Id_Number1") {
      
      #locate the index of the gene
      index <- which(df$Id_Number1 == userGene)
      
      #get link of the gene
      link <- df$Genome_Browser[index]
      
      #remove the link as a webpage
      htmlPage <- read_html(link)
      
      #remove the tages
      webPage <<- head(html_attr(html_nodes(htmlPage, "a"), "href"))
      return(webPage)
    }
    else if (plotAnnotation == "Id_Number2"){
      #locate the index of the gene
      index <- which(df$Id_Number2 == userGene)
      
      #get link of the gene
      link <- df$Genome_Browser[index]
      
      #remove the link as a webpage
      htmlPage <- read_html(link)
      
      #remove the tages
      webPage <<- head(html_attr(html_nodes(htmlPage, "a"), "href"))
      return(webPage)
    }
    else {
      #locate the index of the gene
      index <- which(df$Name == userGene)
      
      #get link of the gene
      link <- df$Genome_Browser[index]
      
      #remove the link as a webpage
      htmlPage <- read_html(link)
      
      #remove the tages
      webPage <<- head(html_attr(html_nodes(htmlPage, "a"), "href"))
      return(webPage)
    }
  })
  
  #Genome_Browser web page
  getJBPage <- eventReactive(input$searchGene, {
    jBrowseWebsite <<- userSearchGene()
  })
  
  #open another window to Genome_Browserr
  observeEvent(input$getPage, {
    js$browseURL(getJBPage())
  })
  
  
  #primer3plus webpage
  observe({
    primerWebSite <<- "https://www.primer3plus.com" 
  })
  
  
  #Output the primer3plus webpage
  output$Primer3Plus <- renderUI({
    page <- tags$iframe(src=primerWebSite, height = 925, width ="100%",frameBorder="0")
    page
  })
}

##run the application
shinyApp(ui = ui, server = server)
