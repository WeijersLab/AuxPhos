#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(DT)
# devtools::install_github("swsoyee/r3dmol")
library(r3dmol)
library(dplyr)
library(ggplot2)
library(reshape2)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "PAUXPHOR"),
    dashboardSidebar(id = "",
                     sidebarMenu(
                         menuItem("Time Series", tabName = "time"),
                         menuItem("Overview", tabName = "overview"),
                         menuItem("3D structures", tabName = "3dstructures")
                     )),
    
    dashboardBody(tabItems(
        # First tab content
        tabItem(
            tabName = "time",
            h2("Time series overview"),
            
            selectInput("Columns","Columns",choices = NULL, selected = NULL, multiple = TRUE),
            # fluidRow(column(12, uiOutput("picker"), actionButton("view", "View Selection"))),
            # fluidRow(column(12, selectInput(inputId = "age2", label = "Select", choices = c("young", "old")), 
            #                 actionButton("view", "Go!"), 
            #                 uiOutput("picker"))),
            fluidRow(column(12, div(DT::dataTableOutput("overviewTable")))),
            fluidRow(column(5, plotOutput("chart")),
                     fluidRow(column(5, r3dmolOutput("pdb"))),
                     
                     # fluidRow(column(5, plotOutput("chart")),column(5, r3dmolOutput("pdb")))
                     
            ) # end of main panel
            
        ),
        
        # Second tab content
        tabItem(tabName = "overview", h2("Overview")),
        
        # third tab content
        tabItem(
            tabName = "3dstructures", 
            h2("3D tab content"), 
            fluidRow(column(12, div(DT::dataTableOutput("pdbTable")))),
        )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Loads the time series data
    timeSeries <- data.table::fread("./data/timeseries.tsv",header = TRUE, sep = "\t")
    pdbList <- list.files("./data/pdb", full.names = TRUE)
    
    output$picker <- renderUI({
        pickerInput(inputId = 'pick', 
                    label = 'Choose', 
                    choices = colnames(timeSeries),
                    options = list(`actions-box` = TRUE),multiple = T)
    })        
    
    # Selected by default
    updateSelectInput(session, "Columns", choices=names(timeSeries), selected = c("UniqueID", "T0 min", "T0.5 min", "T1 min", "T2 min", "T5 min", "T10 min", "Gene description"))
    
    
    # highlight selected rows in the scatterplot and show 3d structure
    observeEvent(input$overviewTable_rows_selected, ignoreNULL = FALSE, {
        s = input$overviewTable_rows_selected
        if (!is.null(s)) {
            # Chart plot
            output$chart = renderPlot({
                # Obtain the rows selected
                rowData <- timeSeries[s,]
                # Reduce columns to the selected few
                rowData <- rowData %>% select(1, 2,3,4,5,6,7)
                # print(colnames(rowData))
                # Turn into 3 columns for ggplot
                df_melted = melt(rowData, id.vars = 'UniqueID')
                # Plot
                ggplot(df_melted, aes(x = variable, y = value)) + geom_line(aes(color = UniqueID, group = UniqueID))
            })
            # If only 1 is selected we can show the 3d plot
            if (length(s) == 1) {
                # Create PDB plot
                output$pdb <- renderR3dmol({
                    print(timeSeries[s,])
                    highlight = timeSeries[s,]$`Amino acid`
                    pdb_file = pdbList[s]
                    if (!dir.exists("pdb")) {
                        showNotification("Please unzip the PDB zipped folder", type=c("error"))
                    } else if (file.exists(pdb_file)) {
                        r3dmol(
                            viewer_spec = m_viewer_spec(
                                cartoonQuality = 10,
                                lowerZoomLimit = 50,
                                upperZoomLimit = 350
                            ),
                            # id = "demo2",
                            # elementId = "demo2"
                        ) %>%
                            # Add model to scene
                            m_add_model(data = pdb_file, format = "pdb") %>%
                            # Zoom to encompass the whole scene
                            m_zoom_to() %>%
                            # Set style of structures
                            m_set_style(style = m_style_cartoon(color = "#00cc96")) %>%
                            # Set style of specific selection (selecting by secondary)
                            m_set_style(
                                sel = m_sel(ss = "s"),
                                style = m_style_cartoon(color = "#636efa", arrows = TRUE)
                            ) %>%
                            # Style the alpha helix
                            m_set_style(
                                sel = m_sel(ss = "h"), # Style alpha helix
                                style = m_style_cartoon(color = "#ff7f0e")
                            ) %>%
                            # Rotate the scene by given angle on given axis
                            m_rotate(angle = 90, axis = "y") %>%
                            # Animate the scene by spinning it
                            # m_spin() %>%
                            # Label selection for specific sites
                            m_add_sphere(
                                text = "The middle of the selection",
                                center = m_sel(resi = highlight), 
                                spec = m_shape_spec(color = "pink", wireframe = TRUE),
                                radius = 2.5
                            )
                    }
                })
            } else {
                output$pdb = NULL
            }
        }
    }
    )
    
    
    
    # Column filtering changing the data table
    observeEvent(input$Columns, {
        columnNumbers <- which(!names(timeSeries) %in% input$Columns)
        output$overviewTable <- DT::renderDT(
            datatable(
                data = timeSeries,
                # extensions = 'Buttons',
                filter = 'top',
                rownames= TRUE,
                options = list(
                    scrollX = TRUE,   ## enable scrolling on X axis
                    scrollY = TRUE,   ## enable scrolling on Y axis
                    autoWidth = TRUE, ## use smart column width handling
                    columnDefs = list(
                        list(width = '200px', targets = "_all"),
                        list(targets = columnNumbers, visible = FALSE)
                    )
                )
            ))
    })
    
    
    # Generate PDB table (discontinued)
    output$pdbTable <- DT::renderDataTable(datatable(
        data=as.data.frame(pdbList),
        # extensions = 'Buttons',
        filter = 'top',
        selection = 'single',
        rownames= FALSE,
        options = list()
    ))
}

# Run the application
shinyApp(ui = ui, server = server)
