# credits  ---------------------------------------------------------------------
'
Autorinnen:         Isabell & Laura 
Erstelldatum:       06.06.2025    
Letztes Update:     21.09.2025
Zweck:              Dieses Master-Skript steuert die gesamte Analyse und Replikation.
                    Es lädt zwei separate Skripte ein:
                    (1) "replication_website.R" für die 
                        Replikation der Ergebnisse für den FU Stat Webeintrag "Classification Trees"
                    (2) "replication_paper.R" für die
                        Replikation der Ergebnisse für das zugehörige Paper

Softwareumgebung: - R version 4.3.0 (2023-04-21)
                  - Platform: x86_64-apple-darwin20 (macOS 15.5)
                  - Locale: en_US.UTF-8
                  - Timezone: Europe/Berlin
                  - RStudio: 2024.09.1+394 "Cranberry Hibiscus"
                  - Relevante Paketversionen:
                    (readr 2.1.4; rpart   4.1.24, rpart.plot 3.1.2, 
                    dplyr  1.1.4; ggplot2 3.4.4;  tidyr      1.3.0)

Hinweise:   Für vollständige Details zur Softwareumgebung, siehe sessionInfo.txt. 
            Bitte stellen Sie sicher, dass der Pfad zum Arbeitsverzeichnis in
            Zeile 34 vor dem Ausführen des Codes angepasst wird. Die Sprache der
            Kommentare ist aus Gründen der Kürze vollständig auf Englisch 
            gehalten. Bitte vergrößern Sie das Plot-Fenster in RStudio vor Ausführung
            der Zeile 124, da sonst der Fehler "Error in plot.new(): figure margins 
            too large" auftreten kann.
' 
# A) initialization ------------------------------------------------------------
rm(list=ls(all=TRUE))         # clear environment
graphics.off()                # clear console
# set working directory 
setwd("...")
# install & load packages (do not change anything, just run the code!)
libraries = c("readr", "dplyr", "ggplot2", "tidyr", "xtable", "rpart", "rpart.plot", 
              "ragg", "stringr", "plotmo")
lapply(libraries, function(x) if (!(x %in% installed.packages())) 
{install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Please do not change. Just run the code. 
# The next lines create subfolders where all figures and tables for the website 
# and the paper (as they appear in our submissions) will be saved into: 
main_folders <- c("paper", "webentry")
for (mf in main_folders) {
  mf_path <- file.path(getwd(), mf)     
  output_path <- file.path(mf_path, "output")   
  if (!dir.exists(mf_path)) {
    dir.create(mf_path)
  }
  if (!dir.exists(output_path)) {
    dir.create(output_path)
  }
}
# define paths
path_paper <- file.path(getwd(), "paper") 
path_paper_out <- file.path(getwd(), "paper", "output")
path_webentry <- file.path(getwd(), "webentry") 
path_webentry_out <- file.path(getwd(), "webentry", "output")

# how to cite the package rpart
citation("rpart")



# B) load in data --------------------------------------------------------------
df <- read_delim("data/bundesliga_dataset.csv", delim = ";", show_col_types = FALSE)
data_all <- df %>%
  mutate(
    CL = ifelse(Zielklasse == "CL", "CL", "Nicht-CL"),
    CL = as.factor(CL),
    club_year = substr(Club, nchar(Club) - 1, nchar(Club)), 
    Trainerwechsel = as.integer(Trainerwechsel_Vorsaison), 
    Passquote = as.integer(Passquote_Vorsaison), 
    Ballbesitz = as.integer(Ballbesitz_Vorsaison), 
    Zweikampf = round(as.numeric(Zweikampf_Vorsaison)/34,0), 
    Titel = as.integer(Titel_Vorsaison), 
    Fouls = as.integer(Fouls_Vorsaison)) %>% 
  drop_na() %>%
  select(CL, Passquote, Zweikampf, Trainerwechsel, Titel, Ballbesitz, Fouls)
# extract dummy variable names
features_dummies <- data_all %>% select(where(~ n_distinct(.) == 2)) %>% names() 



# C) prepare data --------------------------------------------------------------
set.seed(484) # seed for reproducibility 
df_train <- data_all %>%
  group_by(CL) %>%
  slice_sample(prop = 0.8) %>% # 80:20 partition of full dataset 
  ungroup()
df_test <- anti_join(data_all, df_train, by = colnames(data_all))

features_all <- c("Passquote", "Zweikampf", "Trainerwechsel", "Titel", "Ballbesitz", "Fouls")
X_train <- df_train %>% select(all_of(features_all))
Y_train <- df_train$CL
X_test <- df_test %>% select(all_of(features_all))
Y_test <- df_test$CL

# combine to one data frame 
data_train <- X_train
data_train$CL <- Y_train
data_test <- X_test
data_test$CL <- Y_test

# create training test und training data with only two features 
features2 <- c("Fouls","Zweikampf")
data_trainF2 <- data_train %>% select(all_of(features2), CL)
data_testF2 <- data_test %>% select(all_of(features2), CL)

# create training test and training data with only two features 
features3 <- c("Fouls","Zweikampf", "Passquote")
data_trainF3 <- data_train %>% select(all_of(features3), CL)
# export training data with 3 features for Python Code 
write.csv(data_trainF3, paste0("data/data_trainF3.csv"), row.names = FALSE)


# D) run subscripts ------------------------------------------------------------
## 1) for paper --------------------------------
# NOTE: Please enlarge the plot window in RStudio before running this code,
# otherwise the error "Error in plot.new(): figure margins too large" may occur.
# Load the replication script and save outputs to paper/output
source(file.path(path_paper, "replication_paper.R"))

## 2) for fu:stat webentry --------------------------------
# load in replication_paper.R and check folder paper/output
source(file = file.path(path_webentry, "replication_webentry.R")) 


