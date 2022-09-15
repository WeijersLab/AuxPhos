# AuxPhos (AUXin PHOSphoproteomics resource)
AuxPhos is a tool/resource to access data from the Auxin-dependent phosphorylation studies performed at the [Weijers lab, Laboratory of Biochemistry, Wageningen University](https://www.wur.nl/en/Research-Results/Chair-groups/Agrotechnology-and-Food-Sciences/Biomolecular-Sciences/Laboratory-of-Biochemistry/Research/Plant-Development.htm). This repository contains analyzed data as well as a Shiny app to visualize the data. Look at the 'Overview' page in the Shiny app, for details about experimental data. 

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
- svglite

**Load AuxPhos from R:**

- Download the whole 'ShinyApp' directory from files above
- If your 'ShinyApp' directory is in 'Downloads', then type `Rscript -e "library(methods); shiny::runApp('~/Downloads/ShinyApp', launch.browser = TRUE)"` in your terminal window.
- AuxPhos tool is now launched in your deafult web browser
	
**Load AuxPhos from Rstudio:**

- Download the whole 'ShinyApp' directory from files above
- Open 'Rstudio'
- Navigate to the downloaded location of 'ShinyApp' directory
- Open 'app.R' and click 'Run App' on the top-right corner of script window
- A browser window will open automatically with the launched AuxPhos tool 

---

If you use this tool/data, please refer/cite the following publications:

- Roosjen M, Kuhn A et al., Unpublished
- Kuhn A, Roosjen M et al., Unpublished

---

If you have any suggestions, feedback or issues, please write to us (dolf [dot] weijers [at] wur [dot] nl) or 'create an issue' [here](https://github.com/sumanthmutte/AuxPhos/issues) in the GitHub repository. 


