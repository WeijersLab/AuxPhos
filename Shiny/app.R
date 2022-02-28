#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(DT)
# devtools::install_github("swsoyee/r3dmol")
library(r3dmol)

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
            fluidRow(column(12, div(DT::dataTableOutput("overviewTable")))),
            fluidRow(column(12, plotOutput("x2"))
                
            ) # end of main panel
            
        ),
        
        # Second tab content
        tabItem(tabName = "overview", h2("Overview")),
        
        # third tab content
        tabItem(
            tabName = "3dstructures", 
            h2("3D tab content"), 
            fluidRow(column(12, div(DT::dataTableOutput("pdbTable")))),
            fluidRow(column(12, r3dmolOutput("pdb")))
         )
)))
    
# Define server logic required to draw a histogram
server <- function(input, output, session) {
        # Loads the time series data
        timeSeries <- data.table::fread("./data/timeseries.tsv",header = TRUE, sep = "\t")
        pdbList <- list.files("./data/pdb", full.names = TRUE)
        
        output$overviewTable <- DT::renderDataTable(
            datatable(
                data = timeSeries,
                # extensions = 'Buttons',
                filter = 'top',
                rownames= FALSE,
                options = list()
            ))
    
    # highlight selected rows in the scatterplot
    output$x2 = renderPlot({
        s = input$overviewTable_rows_selected
        if (!is.null(s)) {
            # Obtain the rows selected
            rowData <- timeSeries[s,]
            # Reduce columns to the selected few
            rowData <- rowData %>% select(1,2,3,4,5,6, 8)
            # Turn into 3 columns for ggplot
            df_melted = melt(rowData, id.vars = 'Proteins')
            # Plot
            ggplot(df_melted, aes(x = variable, y = value)) + geom_line(aes(color = Proteins, group = Proteins))
        }
    })
    
    output$pdbTable <- DT::renderDataTable(datatable(
        data=as.data.frame(pdbList),
        # extensions = 'Buttons',
        filter = 'top',
        selection = 'single',
        rownames= FALSE,
        options = list()
        ))
    
    output$pdb <- renderR3dmol({
        s = input$pdbTable_rows_selected
        if (!is.null(s)) {
            pdb_file = pdbList[s]
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
                m_spin()
        }
        })
}
    
    # Run the application
    shinyApp(ui = ui, server = server)
    