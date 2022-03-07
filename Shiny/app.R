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

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "PAUXPHOR"),
    dashboardSidebar(id = "",
                     sidebarMenu(
                         menuItem("Overview", tabName = "overview"),
                         menuItem("Time Series", tabName = "time")
                     )),
    
    dashboardBody(
        
        # Add background 'white' for the body to make plot/structures look nicer
        tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }
                                '))),
        
        tabItems(

        # 'Time series' tab content
        tabItem(
            tabName = "time",h2("Time series data"),
            selectInput("Columns","Columns",choices = NULL, selected = NULL, multiple = TRUE, width = '100%'),
            fluidRow(column(12, div(DT::dataTableOutput("overviewTable")))),
            fluidRow(column(6, plotOutput("chart"), style='padding-top:30px; padding-bottom:10px'),
            fluidRow(column(6, r3dmolOutput("pdb"), style='padding-top:30px; padding-bottom:10px'))
            )
            
        ),
        
        # 'Overview' tab content
        tabItem(tabName = "overview", h2("Overview"))
        
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Loads the time series data
    timeSeries <- data.table::fread("./data/timeseries.tsv",header = TRUE, sep = "\t")

    output$picker <- renderUI({
        pickerInput(inputId = 'pick', 
                    label = 'Choose', 
                    choices = colnames(timeSeries),
                    options = list(`actions-box` = TRUE),multiple = T)
    })        
    
    # Selected by default
    updateSelectInput(session, "Columns", choices=names(timeSeries), selected = c("UniqueID", "T0 min", "T0.5 min", "T1 min", "T2 min", "T5 min", "T10 min", "Gene ID", "Gene name"))
    
    
    # highlight selected rows in the scatterplot and show 3d structure
    observeEvent(input$overviewTable_rows_selected, ignoreNULL = FALSE, {
        s = input$overviewTable_rows_selected
        # Remove PDB and Plot
        output$chart = NULL
        output$pdb = NULL
        output$chartMultiple = NULL
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
                    ggplot(df_melted, aes(x = variable, y = value)) + geom_line(aes(color = UniqueID, group = UniqueID), size=1.2) +
                    xlab("Time points") + ylab("Normalized ratio") + theme_bw() + 
                    theme(legend.position="bottom", 
                          axis.title.x = element_text(size=14, face="bold", vjust = -1),
                          axis.title.y = element_text(size=14, face="bold"),
                          axis.text.x = element_text(size=12),
                          axis.text.y = element_text(size=12))
                })
            
            # If only 1 is selected we can show the 3d plot
            if (length(s) == 1) {
                
                # Create PDB plot
                output$pdb <- renderR3dmol({
                    highlight = timeSeries[s,]$`Amino acid`
                    pdb_file = paste0("./data/pdb/AF-",timeSeries[s,]$Structure,"-F1-model_v2.pdb")
                    print(paste("Row",s, "Loading", pdb_file))
                    if (file.exists(pdb_file)) {
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
                                spec = m_shape_spec(color = "grey", wireframe = TRUE),
                                radius = 2.5
                            )
                    }
                })
            } else {
                # Show warning message
            }
        }
    }
    )
    
    
    # Column filtering changing the data table
    observeEvent(input$Columns, {
        textLimit <- which(names(timeSeries) %in% c("Gene description"))
        print(textLimit)
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
                    columnDefs = 
                        list(
                        list(targets = columnNumbers, visible = FALSE),
                    # ),
                    list(
                            targets = "_all",
                            render = JS(
                                "function(data, type, row, meta) {",
                                "return type === 'display' && data != null && data.length > 25 ?",
                                "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
                                "}")))
                ),
            ))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
