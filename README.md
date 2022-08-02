# AuxPhos (AUXin PHOSphoproteomics resource)
AuxPhos is a tool/resource to access data from the Auxin-dependent phosphorylation studies performed at Weijers lab, Laboratory of Biochemistry, Wageningen University (www.bic.wur.nl). This repository contains analyzed data as well as a Shiny app to visualize the data. Look at the 'Overview' page in the Shiny app, for details about experimental data. 

Shiny app is accessible here: [https://sumanthmutte.shinyapps.io/AuxPhos](https://sumanthmutte.shinyapps.io/AuxPhos)

---

For some technical reason (or the ease of access), if you are interested in using/running the Shiny app on your own computer either from R or Rstudio, please follow the steps below:

**Install R packages (if not installed already):**

- shinydashboard
- shiny
- shinyWidgets
- DT
- r3dmol
- dplyr
- ggplot2
- reshape2

**Load AuxPhos from R:**

- Download the 'ShinyApp' directory 
- If your 'ShinyApp' diretcory is in your 'Downloads', then type 'R -e "shiny::runApp('~/Downloads/ShinyApp')"' (excluding outer single quotes) in your terminal window.
- Once the app is launched, it prints a message similar to this 'Listening on http://127.0.0.1:5858'. 
- Open your favourite browser and navigate to the link mentioned in the previous message (in this case: http://127.0.0.1:5858).
- You will see the launched AuxPhos tool
	
**Load AuxPhos from Rstudio:**

- Download the 'ShinyApp' directory 
- Open 'Rstudio'
- Navigate to the downloaded location of 'ShinyApp' directory
- Open 'app.R' and click 'Run App' on the top-right corner of script window
- A browser window will open automatically with the launched AuxPhos tool 

---

If you use this tool/data, please refer/cite the following publications:

- Roosjen M, Kuhn A et al., Unpublished
- Kuhn A, Roosjen M et al., Unpublished

---

If you have any suggestions, feedback or issues, please write to us (XXX@email.com) or 'create an issue' here in the GitHub repository. 


