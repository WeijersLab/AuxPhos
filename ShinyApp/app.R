#
# A Shiny web application to visualize phosphoproteomics data.
#

# Load required R packages
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(DT)
library(r3dmol)
library(dplyr)
library(ggplot2)
library(reshape2)
library(svglite)
library(shinycssloaders)

js <- "
function openFullscreen(elem) {
  if (elem.requestFullscreen) {
    elem.requestFullscreen();
  } else if (elem.mozRequestFullScreen) { /* Firefox */
    elem.mozRequestFullScreen();
  } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
    elem.webkitRequestFullscreen();
  } else if (elem.msRequestFullscreen) { /* IE/Edge */
    elem.msRequestFullscreen();
  }
}"

css <- "
#ggplot:-webkit-full-screen {
  height: 100%;
  margin: 0;
}
#ggplot:-ms-fullscreen {
  height: 100%;
}
#ggplot:fullscreen {
  height: 100%;
}"

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "AuxPhos"),
    dashboardSidebar(id = "",
                     sidebarMenu(
                         menuItem("Overview", tabName = "overview"),
                         menuItem("Phosphoproteomes", tabName = "time"),
                         menuItem("Orthogroups", tabName = "ortho"),
                         menuItem("Help", tabName = "help")
                     )),
    
    dashboardBody(
        
        # Add background 'white' for the body to make plot/structures look nicer
        tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }
                                ')),
                  tags$script(HTML(js)),
                  tags$style(HTML(css))
                  ),
        
        tabItems(

        # 'Overview' tab content
        tabItem(tabName = "overview", 
                h2(strong("AuxPhos (AUXin PHOSphoproteomics resource)"), align='center'), br(), 
                h4(strong("Background")), p("The naturally occurring plant hormone auxin (indole 3-acetic acid (IAA)) is found in some prokaryotes and eukaryotes but in all the land plants studied to date. IAA can trigger a wide range of responses that in turn affect plant growth and development. However, all these responses studied so far were observed to occur through the Nuclear Auxin Pathway (NAP), a transcriptional meachanism where auxin promotes interaction of Aux/IAA transcriptional inhibitors with a ubiquitin ligase complex (SCF-TIR1/AFB) to promote Aux/IAA protein degradation. With this, the DNA-binding ARF transcription factors transcriptionally control their many target genes to elicit the auxin dependent responses. However, the first IAA-induced transcripts are visible around 10 minutes after IAA treatment, making it unlikely for NAP to account for the changes in e.g. ion fluxes, cellular growth and subcellular traffic that have been observed to occur within seconds to minutes. In an effort to identify the mechanisms underlying these rapid responses, we explored the possible role of protein phosphorylation by studying the rapid phosphorylation changes across multiple plant species and mutant alleles of some of the genes expected to be involved in these responses."), br(), 
                h4(strong("Experimental data")), p("In the table below, details about the various auxin treatments across species and the mutants are given. 'Dataset' contains the short name used across the database. 'Species/ecotype/mutant' refers to the plant genetic background from which this particular dataset has been generated. 'Treatment' shows the concentration and the hormone/chemical used for treatment, otherwise given as Mock. 'Phosphosites' shows the number of total peptides found in that dataset."), br(),
                fluidRow(column(12, align="center", div(DT::dataTableOutput("samplesTable")))), br(),
                h4(strong("Data accessibility")), p("Through the 'Phosphoproteomes' page, all the data mentioned in the section above can be accessed, whereas 'Orthogroups' page contains a lookup table showing the clustering of orthogroups from various species, shown in the 'Phosphoproteomes' page. Please refer to the 'Help' page for detailed usage instructions of AuxPhos tool. However, if you are interested in using this app instance on your own computer, we recommend looking at the instructions provided in the AuxPhos GitHub respository here: https://github.com/sumanthmutte/AuxPhos."), br(), 
                h4(strong("References")), p("Roosjen M, Kuhn A et al., in preparation."), br() 
        ),
          

        # 'Phosphoproteomes' tab content
        tabItem(
            tabName = "time",h2("Phosphoproteome data"),br(),
            #selectInput("Columns","Columns",choices = NULL, selected = NULL, multiple = TRUE, width = '100%'),
            fluidRow(column(12, div(withSpinner(DT::dataTableOutput("overviewTable"))))),
            fluidRow(column(6, align="center", downloadButton('downloadPlot','Download Plot'), plotOutput("chart"), style='padding-top:30px; padding-bottom:10px'),
            fluidRow(column(6, align="center", actionButton("fs", "Full screen", onclick = "openFullscreen(document.getElementById('pdb'));"), 
              r3dmolOutput("pdb"), style='padding-top:30px; padding-bottom:10px'))
            )
        ),

        
        # 'Orthogroups' tab content
        tabItem(
          tabName = "ortho",h2("Orthogroups"),
          p("Lookup table for genes and corresponding orthogroups indicated in the phosphoproteome data"), br(),
          fluidRow(column(12, div(DT::dataTableOutput("orthoTable"))))
        ),
        
        
        # 'Help' tab content
        tabItem(tabName = "help", h2("Help"), br(),
                h4(strong("Visualizations")), p("The different visualizations"), br(), 
                h4(strong("Searching")), p("How to search"), br(),
                h4(strong("Contact")), p("Contact details here.") 
        )
        
    )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Load the samples file for overview page
    dataSamples <- data.table::fread("./data/Samples_AuxPhos.csv",header = TRUE, sep = "\t", stringsAsFactors = T)
    output$samplesTable <- DT::renderDT(
      datatable(
        data = dataSamples,
        filter = 'top',
        rownames= F,
        extensions = c('Select','Buttons'),
        options = list(
          select = list(style = "multi", items = "row"),
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          dom = "Blfrtip",
          buttons = c('selectAll', 'selectNone','copy', 'csv', 'excel')
        ),
        selection="none"
      )
    )
    

    # Load the orthogroups file
    dataOrthogroups <- data.table::fread("./data/Orthogroups_AuxPhos.csv",header = TRUE, sep = "\t")
    output$orthoTable <- DT::renderDT(
      datatable(
        data = dataOrthogroups,
        filter = 'top',
        rownames= F,
        extensions = c('Select','Buttons'),
        options = list(
          select = list(style = "multi", items = "row"),
          scrollX = TRUE,   ## enable scrolling on X axis
          scrollY = TRUE,   ## enable scrolling on Y axis
          autoWidth = TRUE, ## use smart column width handling
          searchHighlight = TRUE,
          columnDefs =
            list(
              list(width = '10%', targets = 0),
              list(width = '18%', targets = c(1,2,3,4,5))
            ),
          dom = "Blfrtip",
          buttons = c('selectAll', 'selectNone','copy', 'csv', 'excel')
        ),
        selection="none"
      )
    )
    
    
    # Loads the phosphoproteome data (time series + other species + mutants)
    timeSeries <- data.table::fread("./data/PhosphoData_AuxPhos_trimmed.csv",header = TRUE, sep = "\t", colClasses=c(Dataset="factor"), na.strings = NULL)
    # output$picker <- renderUI({
    #   pickerInput(inputId = 'pick',
    #               label = 'Choose', 
    #               choices = colnames(timeSeries),
    #               options = list(`actions-box` = TRUE),multiple = T)
    #   })        
    # 
    # # Selected by default
    # updateSelectInput(session, "Columns", choices=names(timeSeries), 
    #                   selected = c("UniqueID", "Dataset", "T0.5 min", "T1 min", "T2 min", "T5 min", "T10 min", "Gene ID", "Gene Name", "Orthogroup")
    #                   )
    # Column filtering changing the data table
    #    observeEvent(input$Columns, {
    #        columnNumbers <- which(!names(timeSeries) %in% input$Columns)
    output$overviewTable <- DT::renderDT(
      datatable(
        data = timeSeries,
        extensions = c('Select','Buttons'),
        filter = 'top',
        rownames= F,
        options = list(
          select = list(style = "os", items = "row"),
          scrollX = TRUE,   ## enable scrolling on X axis
          scrollY = TRUE,   ## enable scrolling on Y axis
          autoWidth = TRUE, ## use smart column width handling
          #columnDefs = list( list(targets = columnNumbers, visible = FALSE) ),
          dom = "Blfrtip",
          buttons = c('selectNone','copy', 'csv', 'excel') # 'selectAll', 
        ),
        selection="none"
      ), server = F
    )
    #        })
    
    
    
    
    data2 <- reactiveValues()
    # highlight selected rows in the lineplot and show 3d structure
    observeEvent(input$overviewTable_rows_selected, ignoreNULL = FALSE, {
        s = input$overviewTable_rows_selected
        # Remove PDB and Plot
        output$chart = NULL
        output$pdb = NULL
        output$chartMultiple = NULL
        if (!is.null(s)) {
          
          # Obtain the rows selected
          rowData <- timeSeries[s,]
          # Reduce columns to the selected few
          rowData <- rowData %>% select(3,4,5,6,7,8,9)
          # Turn into 3 columns for ggplot
          df_melted = melt(rowData, id.vars = 'UniqueID')
          # Plot
          data2$plotObject = ggplot(df_melted, aes(x = variable, y = value)) + 
            geom_point(aes(color = UniqueID, group = UniqueID), size=3.5) +
            geom_line(aes(color = UniqueID, group = UniqueID), size=1.2) +
            xlab("Time points") + ylab("Normalized ratio") + theme_bw() + 
            theme(legend.position="bottom", 
                  axis.title.x = element_text(size=14, face="bold", vjust = -1),
                  axis.title.y = element_text(size=14, face="bold"),
                  axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12))
          
          # Chart plot
          output$chart = renderPlot({ data2$plotObject })
            
          # Save plot
          output$downloadPlot <- downloadHandler(
            filename = "AuxPhosPlot.svg",
            content = function(file){ ggsave(file,plot=data2$plotObject,device="svg",width = 10, height = 6) }
            )
                
          # Check if the ones selected are from the same 3d structure
          if (length(unique(timeSeries[s,]$Structure)) == 1) {
            
            # Create PDB plot
            pdb_file = paste0("./data/pdb/",unique(timeSeries[s,]$Structure),".pdb")
            print(paste("Row",s, "Loading", pdb_file))
            highlight = c(timeSeries[s,]$Position)
              
            if (file.exists(pdb_file)) {
              expression <- {
                r3dmol( viewer_spec = m_viewer_spec(cartoonQuality = 10, lowerZoomLimit = 50, upperZoomLimit = 350) ) %>%
                  # Add model to scene
                  m_add_model(data = pdb_file, format = "pdb") %>%
                  # Zoom to encompass the whole scene
                  m_zoom_to() %>%
                  # Set style of structures
                  m_set_style(style = m_style_cartoon(color = "#00cc96")) %>%
                  # Set style of specific selection (selecting by secondary)
                  m_set_style( sel = m_sel(ss = "s"), style = m_style_cartoon(color = "#636efa", arrows = TRUE) ) %>%
                  # Style the alpha helix
                  m_set_style( sel = m_sel(ss = "h"), style = m_style_cartoon(color = "#ff7f0e") ) 
                }
                        
                for (i in highlight) {
                  expression <- expression %>%
                    m_add_sphere(
                      text = "The middle of the selection",
                      center = m_sel(resi = c(i)),
                      spec = m_shape_spec(color = "red", wireframe = TRUE),
                      radius = 2.5 ) %>%
                    m_add_label(
                      text = i,
                      sel = m_sel(resi = c(i)),
                      style = m_style_label( backgroundColor = "#666666", backgroundOpacity = 0.9 ) )
                  }
                
              output$pdb <- renderR3dmol(expression)

            }

          } else {
            # Show message
            expression = r3dmol() %>% m_add_label( text = "Select one structure" )
            output$pdb <- renderR3dmol(expression)
        } 
        } else {
          # Show message
          expression = r3dmol() %>% m_add_label( text = "Select one structure" )
          output$pdb <- renderR3dmol(expression)
        }
    }
    )


    }

    
# Run the application
shinyApp(ui = ui, server = server)
