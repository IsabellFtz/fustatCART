# CART Visualizations and Replication
This repository provides supplementary material for a project on Classification Trees.  
The project includes two components:  
1. A webpage entry introducing the topic at a level suitable for end-of-bachelor students interested in classical machine learning models.  
2. A scientific paper offering a deeper discussion of the mathematical theory behind Classification Trees and their application to a dataset on football performance measures of all clubs in the German 1st Bundesliga (seasons 2019/2020–2024/25).  
Using CART models, we estimate a decision rule to predict whether a club qualifies for the UEFA Champions League based on its performance in the previous season.  
The corresponding fu:stat website article is expected to be published at a later date (as of September 2025).  



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
  - cleans and preprocesses the data.  
  - generates the `data_trainF3.csv` dataset.  
  - executes both `replication_paper.R` and `replication_webentry.R`.  


## How to run ...
1) Download the repository by clicking on the green *Code* button and selecting *Download ZIP*. Unzip the folder and choose a location on your computer.  
2) The folders `webentry/` and `paper/` each contain a subfolder called `output/` with pre-rendered output files.  
   - Readers may delete these subfolders and re-run the code to regenerate all outputs.  
   - The same applies to the dataset `data_trainF3.csv`, which is generated during preprocessing but already provided for users with time constraints.  
   - Likewise, the files `auxiliary-website/table.html` and `auxiliary-website/plot.html` are pre-generated but can also be recreated by running the Python scripts.  
### ... the R code
3) Open `master.R` in RStudio (or another R environment).  
   - Expand the document outline by clicking the symbol with grey lines next to the *Source* button in RStudio, or use the shortcut **SHIFT+ALT+O** (Mac: **SHIFT+COMMAND+O**).  
4) Read the notes in the *Credits* section and check your R version.  
   - In Section A of `master.R`, set the working directory (line 34) to the downloaded `fustatCART` folder.  
   - For help with setting the working directory, we recommend [Nathaniel D. Phillips' guidelines](https://bookdown.org/ndphillips/YaRrr/the-working-directory.html).  
5) Run Sections B–D of `master.R`.  
   - Please follow the comments for further assistance.  
   - Generated outputs will be stored in `paper/output/` and `webentry/output/`.  
### ... the Python code
6) Set up your Python environment in the `fustatCART` directory and check your Python version.
   - For help with environment setup, we suggest consulting guidelines such as [Python Virtual Environments](https://docs.python.org/3/library/venv.html).  
7) Open `generate3dplot.py` in VS Code (or another IDE) and run the script.  
   - This generates `auxiliary-website/plot.html`.  
8) Open `generateHTMLTable.py` and run the script.  
   - This generates `auxiliary-website/table.html`.


## Required Software
R (version ≥ 4.3.0) and Python (version ≥ 3.10.8)  



  
