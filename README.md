# CART Visualizations and Replication

This repository contains the supplementary material for a project on Classification Trees. The project comprises the creation of a webpage entry introducing the topic at a level suitable for end-of-bachelor students who wish to learn more about classical machine learning models, as well as a more in-depth paper on the mathematical theory behind Classification Trees and their application to a self-chosen dataset. The corresponding fu:stat website article is yet to be published (as of September 2025).

## Repository Contents
- **Auxiliary Website:** HTML website supporting the presentation of the fu:stat webpage entry  
  - `generate3dplot.py` and `generateHTMLTable.py` use the files in the `data/` folder and can each be run individually.  
    - Outputs: `plot.html` and `table.html`  
  - `main.html` serves as the main HTML frame for the auxiliary website and integrates `plot.html` and `table.html`.  
- **Data:** Dataset on performance parameters of all clubs in the 1st Bundesliga, used for replication and visualization of CART models  
  - `bundesliga_dataset.csv`  
  - `data_trainF3.csv` (generated during preprocessing)  
- **Paper:** Code implementations replicating the results of the accompanying scientific paper, including output files  
  - `replication_paper.R` is executed by `master.R` and produces all paper-related output files.  
- **Webentry:** Code implementations replicating all visualizations included in the fu:stat webpage entry, including output files  
  - `replication_webentry.R` is executed by `master.R` and produces all webentry-related output files.  
- **master.R:** The main R script to run the analyses.  
  - Cleans and preprocesses the data.  
  - Generates the `data_trainF3.csv` dataset.  
  - Executes both `replication_paper.R` and `replication_webentry.R`.  


## Getting Started
Instructions on how to run the code or view the website:


  
