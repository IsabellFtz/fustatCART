# credits  ---------------------------------------------------------------------
'
Erstelldatum:       06.06.2025    
Letztes Update:     05.07.2025
Zweck:              Dieses R-Skript generiert alle Grafiken für den fu:stat 
                    Webeintrag "Classification Trees". 

Softwareumgebung: - R version 4.3.0 (2023-04-21)
                  - Platform: x86_64-apple-darwin20 (macOS 15.5)
                  - Locale: en_US.UTF-8
                  - Timezone: Europe/Berlin
                  - RStudio: 2024.09.1+394 "Cranberry Hibiscus"
                  - Relevante Paketversionen:
                      • readr       2.1.4
                      • rpart       4.1.24
                      • rpart.plot  3.1.2
                      • dplyr       1.1.4
                      • ggplot2     3.4.4
                      • tidyr       1.3.0

Hinweise:   Für vollständige Details zur Softwareumgebung, siehe sessionInfo.txt. 
            Bitte stellen Sie sicher, dass der Pfad zum Arbeitsverzeichnis in
            Zeile 31 vor dem Ausführen des Codes angepasst wird. Die Sprache der
            Kommentare ist aus Gründen der Kürze vollständig auf Englisch 
            gehalten.
' 
# 1) initialization -------------------------------------------------------------
rm(list=ls(all=TRUE))         # clear environment
graphics.off()                # clear console
# set working directory 
setwd("...")
# install & load packages (do not change anything, just run the code!)
libraries = c("readr", "rpart", "rpart.plot", "dplyr", "ggplot2", "tidyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) 
{install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Please do not change. Just run the code. 
# The next lines create a subfolder where all figures and tables--as they appear 
# in our submission--will be saved into: 
path_output <- here::here(getwd(), "output")  
if(dir.exists(path_output)!=TRUE){
  dir.create(file.path(getwd(), "output"))}



# 2) load in data -------------------
df <- read_delim("data/bundesliga_dataset.csv", delim = ";", show_col_types = FALSE)
df <- df %>%
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



# 3) prepare data -------------------
set.seed(484)
train_df <- df %>%
  group_by(CL) %>%
  slice_sample(prop = 0.8) %>%
  ungroup()
test_df <- anti_join(df, train_df, by = colnames(df))

names(train_df)
features_all <- c("Passquote", "Zweikampf", "Trainerwechsel", "Titel", "Ballbesitz", "Fouls")
X_train <- train_df %>% select(all_of(features_all))
y_train <- train_df$CL
X_test <- test_df %>% select(all_of(features_all))
y_test <- test_df$CL

# combine to one data frame 
train_data <- X_train
train_data$CL <- y_train
test_data <- X_test
test_data$CL <- y_test



# 4) complete tree --------------------------------------------------------------
clf <- rpart(
  CL ~ ., 
  data = train_data, 
  method = "class", 
  control = rpart.control(cp = 0.01, minbucket = 1,  minsplit = 2),
  parms = list(split = "gini"))

# save plot 
png(paste0(path_output,"/tree_complete.png"), 
    width = 4000, 
    height =2000, 
    res = 300, 
    bg = "transparent")
rpart.plot(clf,
    type = 2,
    box.palette = c("#cbfb04", "#87b2f3"),
    extra = 1,
    fallen.leaves = TRUE,
    branch.col = "blue",  
    split.border.col = "blue")
dev.off()



# 5) 2d tree pruned/ unpruned---------------------------------------------------
features_two <- c("Fouls","Zweikampf")
train_data_2d <- train_data %>% select(all_of(features_two), CL)
test_data_2d <- test_data %>% select(all_of(features_two), CL)

# functions
draw_decision_steps <- function(features, clf) {
  f1 <- features[1]
  f2 <- features[2]
  
  x_seq <- seq(min(train_data[[f1]], na.rm = TRUE) - 50, max(train_data[[f1]], na.rm = TRUE) + 50, length.out = 200)
  y_seq <- seq(min(train_data[[f2]], na.rm = TRUE) - 5, max(train_data[[f2]], na.rm = TRUE) + 5, length.out = 200)
  
  grid <- expand.grid(x = x_seq, y = y_seq)
  colnames(grid) <- c(f1, f2)
  
  plots <- list()
  ablines <- list()
  
  splits <- which(clf$frame$var != "<leaf>")
  splits <- c(splits, max(splits) + 1)
  line_segments_all <- list()
  
  for (i in seq_along(splits)) {
    temp_clf <- prune(clf, cp = clf$frame$complexity[splits[i]])
    
    # split lines
    splits_pruned <- as.data.frame(temp_clf$splits)
    splits_pruned$feature <- rownames(temp_clf$splits)
    rownames(splits_pruned) <- NULL
    splits_nonzero <- splits_pruned[splits_pruned$count != 0, ]
    
    if (nrow(splits_pruned) > 0) {
      tree_nodes <- splits_nonzero %>%
        group_by(count) %>%
        slice_max(improve, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(index = index) %>%
        select(index, feature)
      
      ablines[[i]] <- tree_nodes
      all_previous <- do.call(rbind, ablines[1:(i - 1)])
      
      if (length(ablines) > 2 && nrow(all_previous) > 0) {
        ablines[[i]] <- anti_join(ablines[[i]], all_previous, by = c("index", "feature"))
      }
    } else {
      ablines[[i]] <- NULL
    }
    
    grid$CL <- predict(temp_clf, newdata = grid, type = "class")
    
    xmax <- 525
    ymax <- 120
    xmin <- 275
    ymin <- 85
    p <- ggplot() +
      geom_tile(data = grid, aes_string(x = f1, y = f2, fill = "CL"), alpha = 0.4) +
      geom_point(data = train_data_2d, aes_string(x = f1, y = f2, color = "CL"), size = 4) +
      scale_fill_manual(values = c("CL" = "#cbfb04", "Nicht-CL" = "#87b2f3"), name = NULL) +
      scale_color_manual(values = c("CL" = "#99cc02", "Nicht-CL" = "#2e79e9"), name = NULL) +
      labs(x = f1, y = f2) +
      scale_y_continuous(limits = c(ymin, ymax), breaks = seq(ymin, ymax, 5)) +
      scale_x_continuous(limits = c(xmin, xmax), breaks = seq(xmin, xmax, 25)) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.text = element_text(color = "#4d4d4d", size = 15), 
        axis.title.x = element_text(colour = "#4d4d4d", size = 15),
        axis.title.y = element_text(colour = "#4d4d4d", size = 15),
        axis.text = element_text(size = 14, colour = "#4d4d4d"))
    p
    
    # draw lines
    if (i !=1 && nrow(ablines[[i]]) == 0) {next}
    
    # save plot
    png(filename = file.path(path_output, paste0("tree2d_", i, ".png")),
        width = 2000, height = 1600, res = 300, bg = "transparent")
    rpart.plot(temp_clf, 
               type = 2,
               box.palette = c("#cbfb04", "#87b2f3"), 
               extra = 1, nn = TRUE,
               yesno=1)
    dev.off()
    
    if (i != 1) {
      splits_previous <- ablines[1:i-1]
      splits_previous_df <- do.call(rbind, splits_previous)
      splits_current_df <- ablines[[i]]
      nrow(splits_current_df)
      line_segments <- list()
      
      for (j in 1:nrow(splits_current_df)) {
        threshold_name <- splits_current_df$feature[j]
        threshold_value <- splits_current_df$index[j]
        
        if (threshold_name == f1) {
          line_segments[[length(line_segments) + 1]] <- data.frame(
            x = threshold_value,
            xend = threshold_value,
            y = ymin,
            yend = ymax
          )
        } else if (threshold_name == f2) {
          if (any(splits_previous_df$feature == f1)) {
            x_start <- max(splits_previous_df$index[splits_previous_df$feature == f1], na.rm = TRUE)
          } else {
            x_start <- xmin
          }
          line_segments[[length(line_segments) + 1]] <- data.frame(
            x = x_start,
            xend = xmax,
            y = threshold_value,
            yend = threshold_value
          )
        }
      }
      
      # combine 
      segments_df <- do.call(rbind, line_segments)
      line_segments_all[[i]] <- segments_df
      
      # for all lines cumulatively: 
      segments_df_all <- do.call(rbind, line_segments_all)
      p <- p + geom_segment(
        data = segments_df_all,
        aes(x = x, xend = xend, y = y, yend = yend),
        linetype = "dashed", color = "#4d4d4d", linewidth = 1)
      # for lines from the current step: 
      # p <- p + geom_segment(
      # data = segments_df,
      # aes(x = x, xend = xend, y = y, yend = yend),
      # linetype = "dashed", color = "#4d4d4d", linewidth = 1)
    }
    
    plots[[i]] <- p
    
    # save plot
    ggsave(
      filename = file.path(path_output, paste0("boundary2d_", i, ".png")),
      plot = p,
      width = 10,
      height = 7,
      dpi = 300)
  }
  
  return(plots)
}

draw_decision_boundaries <- function(features, clf, data){
  f1 <- features[1]
  f2 <- features[2]

  x_seq <- seq(min(train_data[[f1]], na.rm = TRUE)-50, max(train_data[[f1]], na.rm = TRUE)+50, length.out = 200)
  y_seq <- seq(min(train_data[[f2]], na.rm = TRUE)-5, max(train_data[[f2]], na.rm = TRUE)+5, length.out = 200)
  
  grid <- expand.grid(x = x_seq, y = y_seq)
  colnames(grid) <- c(f1, f2)

  for (f in setdiff(features, features)) {
    default <- as.numeric(stats::median(data[[f]], na.rm = TRUE))
    grid[[f]] <- default
  }
  
  # grid
  grid$CL <- predict(clf, newdata = grid, type = "class")
  
  p <- ggplot() +
    geom_tile(data = grid, aes_string(x = f1, y = f2, fill = "CL"), alpha = 0.4) +
    geom_point(data = data, aes_string(x = f1, y = f2, color = "CL"), size = 4) +
    scale_fill_manual(values = c("CL" = "#cbfb04", "Nicht-CL" = "#87b2f3"), name = NULL) +
    scale_color_manual(values = c("CL" = "#99cc02", "Nicht-CL" = "#2e79e9"), name = NULL) +
    labs(x = f1, y = f2) +
    scale_y_continuous(limits = c(85, 120), breaks = seq(85, 120, 5)) +
    scale_x_continuous(limits = c(275, 525), breaks = seq(275, 525, 25)) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.x = element_text(color = "#4d4d4d", size = 15),
      axis.title.y = element_text(color = "#4d4d4d", size = 15),
      plot.title = element_blank(),
      panel.grid.minor = element_blank(), 
      legend.text = element_text(color = "#4d4d4d", size = 15), 
      axis.text = element_text(size = 14, colour = "#4d4d4d"))
  
  return(p)
}


## 5.1) pruned 2d tree ---------
clf_two <- rpart(
  CL ~ ., 
  data = train_data_2d, 
  method = "class", 
  control = rpart.control(maxdepth = 4, minbucket = 1,  minsplit = 2),
  parms = list(split = "gini"))
# save plot 
png(filename = file.path(path_output, paste0("tree2d_pruned.png")),
    width = 2000, height = 1600, res = 300, bg = "transparent")
plot_2d_pruned <- rpart.plot(clf_two, 
                             type = 2,
                             box.palette = c("#cbfb04", "#87b2f3"), 
                             extra = 1, 
                             yesno=1)
dev.off()
# draw tree step by step  
draw_decision_steps(features_two, clf_two)
# draw boundaries 
pruned_train <- draw_decision_boundaries(features_two,clf_two,train_data_2d) # on training data 
pruned_test <- draw_decision_boundaries(features_two,clf_two,test_data_2d) # on testing data 


## 5.2) unpruned 2d tree ---------
clf_two_unpruned <- rpart(
  CL ~ ., 
  data = train_data_2d, 
  method = "class", 
  control = rpart.control(cp=0.01, minbucket = 1,  minsplit = 2),
  parms = list(split = "gini"))
# save plot 
png(filename = file.path(path_output, paste0("tree2d_unpruned.png")),
    width = 2000, height = 1600, res = 300, bg = "transparent")
plot_2d_unpruned <- rpart.plot(clf_two_unpruned, 
                             type = 2,
                             box.palette = c("#cbfb04", "#87b2f3"), 
                             extra = 1, 
                             yesno=1)
dev.off()
# draw boundaries 
unpruned_train <- draw_decision_boundaries(features_two,clf_two_unpruned,train_data_2d) # on training data 
unpruned_test <- draw_decision_boundaries(features_two,clf_two_unpruned,test_data_2d) # on testing data 

# save boundary plots
plots <- list(
  pruned_train = draw_decision_boundaries(features_two, clf_two, train_data_2d),
  pruned_test = draw_decision_boundaries(features_two, clf_two, test_data_2d),
  unpruned_train = draw_decision_boundaries(features_two, clf_two_unpruned, train_data_2d),
  unpruned_test = draw_decision_boundaries(features_two, clf_two_unpruned, test_data_2d))
for (name in names(plots)) {
  ggsave(
    filename = file.path(path_output, paste0("boundary2d_", name, ".png")),
    plot = plots[[name]],
    width = 10,
    height = 7,
    dpi = 300)
  }



# 6) accuracy ------------------------------------------------------------------
pred_unpruned <- predict(clf_two_unpruned, newdata = test_data, type = "class")
table(Predicted = pred_unpruned, Actual = y_test)
(3+10)/(3+10+2+4)

pred_pruned <- predict(clf_two, newdata = test_data, type = "class")
table(Predicted = pred_pruned, Actual = y_test)
(3+11)/(3+11+2+3)



# 7) 3d Tree -------------------------------------------------------------------
features_three <- c("Fouls","Zweikampf","Passquote")
train_data_3d <- train_data %>% select(all_of(features_three), CL)

# export training data with 3 features for Python Code 
write.csv(train_data_3d, paste0(path_output,"/train_data.csv"), row.names = FALSE)

# 3d tree
clf_three <- rpart(
  CL ~ ., 
  data = train_data_3d, 
  method = "class", 
  control = rpart.control(maxdepth = 3, minbucket = 1,  minsplit = 2),
  parms = list(split = "gini"))
# save plot 
png(paste0(path_output,"/tree3d.png"), 
    width = 2000, 
    height =2000, 
    res = 300, 
    bg = "transparent")
rpart.plot(clf_three,
    type = 2,
    box.palette = c("#cbfb04", "#87b2f3"),
    extra = 1,
    fallen.leaves = TRUE,
    branch.col = "blue",  
    split.border.col = "blue")
dev.off()



# 8) software environment ------------------------------------------------------
sink(paste0(path_output,"/sessionInfo.txt"))
sessionInfo()
sink()







