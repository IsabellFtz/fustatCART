# credits ----------------------------------------------------------------------
'
WIRD IN DER DATEI master.R AUSGEFÜHRT!

Zweck:            Dieses Skript repliziert alle Ergebnisse, Abbildungen, die im 
                  fu:stat Webentry eingefügt wurden.
                  Es wird von master.R eingelesen und sollte nicht
                  eigenständig ausgeführt werden (außer zu Debugging-Zwecken).

Output:           Ergebnisse werden in "webentry/output/" gespeichert,
                  wie in master.R definiert. 

Hinweise:         Anpassungen der Arbeitsverzeichnisse erfolgen ausschließlich 
                  in master.R. Die Kommentare sind zur Einheitlichkeit auf Englisch 
                  gehalten.
'

# 0) define path ---------------------------------------------------------------
path <- path_webentry_out



# 1) plot and save trees -------------------------------------------------------
# fully grown tree 
clf <- rpart( CL ~ ., data = data_train, method = "class", parms = list(split = "gini"),
  control = rpart.control(cp = 0.01, minbucket = 1,  minsplit = 2))
png(paste0(path,"/tree_complete.png"), width = 4000, height =2000, res = 300, bg = "transparent")
rpart.plot(clf, type = 2, box.palette = c("#cbfb04", "#87b2f3"), extra = 1,
           fallen.leaves = TRUE, branch.col = "blue", split.border.col = "blue")
dev.off()

# unpruned tree with two features 
clf_two_unpruned <- rpart(CL ~ ., data = data_trainF2, method = "class", parms = list(split = "gini"),
  control = rpart.control(cp=0.01, minbucket = 1,  minsplit = 2))
png(filename = file.path(path, paste0("tree2d_unpruned.png")), width = 2000, height = 1600, res = 300, bg = "transparent")
plot_2d_unpruned <- rpart.plot(clf_two_unpruned, type = 2,box.palette = c("#cbfb04", "#87b2f3"), 
                               extra = 1, yesno=1)
dev.off()

## tree with two features and stopping criterion (maxdepth = 4)
clf_two <- rpart(CL ~ ., data = data_trainF2, method = "class", parms = list(split = "gini"),
  control = rpart.control(maxdepth = 4, minbucket = 1,  minsplit = 2))
png(filename = file.path(path, paste0("tree2d_pruned.png")), width = 2000, height = 1600, res = 300, bg = "transparent")
plot_2d_pruned <- rpart.plot(clf_two, type = 2, box.palette = c("#cbfb04", "#87b2f3"), 
                             extra = 1, yesno=1)
dev.off()

# tree with three features and stopping criterion (maxdepth = 3)
clf_three <- rpart(CL ~ ., data = data_trainF3, method = "class", parms = list(split = "gini"),
  control = rpart.control(maxdepth = 3, minbucket = 1,  minsplit = 2))
png(paste0(path,"/tree3d.png"), width = 2000, height =2000, res = 300, bg = "transparent")
rpart.plot(clf_three, type = 2, box.palette = c("#cbfb04", "#87b2f3"), extra = 1,
           fallen.leaves = TRUE, branch.col = "blue", split.border.col = "blue")
dev.off()



# 2) draw 2D decision boundaries -----------------------------------------------
# load auxiliary functions: 

# draw_decision_steps(): 
# Generates stepwise (!) 2D visualizations of a CART decision tree in the feature space.
# Input:  features (vector of length 2 with feature names for the 2D plot); 
#         clf (rpart object; the fitted tree)
# Output: list of ggplot objects (one per tree split);  saves PNG files of stepwise 
#         plots and decision boundaries to `path`.
draw_decision_steps <- function(features, clf) {
  f1 <- features[1]
  f2 <- features[2]
  
  x_seq <- seq(min(data_train[[f1]], na.rm = TRUE) - 50, max(data_train[[f1]], na.rm = TRUE) + 50, length.out = 200)
  y_seq <- seq(min(data_train[[f2]], na.rm = TRUE) - 5, max(data_train[[f2]], na.rm = TRUE) + 5, length.out = 200)
  
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
      geom_tile(data = grid, aes_string(x = f1, y = f2, fill = "CL"), alpha = 0.4, na.rm = TRUE) +
      geom_point(data = data_trainF2, aes_string(x = f1, y = f2, color = "CL"), size = 4) +
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
    png(filename = file.path(path, paste0("tree2d_", i, ".png")),
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
      filename = file.path(path, paste0("boundary2d_", i, ".png")),
      plot = p,
      width = 10,
      height = 7,
      dpi = 300)
  }
  
  return(plots)
}

# draw_decision_boundaries()
# Generates 2D visualizations of a CART decision tree in the feature space.
# (Does not iterate through splits.)
# Input:  features (vector of length 2); clf (rpart object; the fitted tree); 
#         data (dataset used for plotting points on top of the decision regions)
# Output: one ggplot object (visualizing the decision boundaries)
draw_decision_boundaries <- function(features, clf, data){
  f1 <- features[1]
  f2 <- features[2]
  
  x_seq <- seq(min(data_train[[f1]], na.rm = TRUE)-50, max(data_train[[f1]], na.rm = TRUE)+50, length.out = 200)
  y_seq <- seq(min(data_train[[f2]], na.rm = TRUE)-5, max(data_train[[f2]], na.rm = TRUE)+5, length.out = 200)
  
  grid <- expand.grid(x = x_seq, y = y_seq)
  colnames(grid) <- c(f1, f2)
  
  for (f in setdiff(features, features)) {
    default <- as.numeric(stats::median(data[[f]], na.rm = TRUE))
    grid[[f]] <- default
  }
  
  # grid
  grid$CL <- predict(clf, newdata = grid, type = "class")
  
  p <- ggplot() +
    geom_tile(data = grid, aes_string(x = f1, y = f2, fill = "CL"), alpha = 0.4, na.rm = TRUE) +
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

# draw decision regions ...
# ... for unpruned CART with two features, with ...
unpruned_train <- draw_decision_boundaries(features2,clf_two_unpruned,data_trainF2) # on training data 
unpruned_test <- draw_decision_boundaries(features2,clf_two_unpruned,data_testF2) # on testing data 

# ... for CART with two features and stopping criterion (maxdepth = 4), with ...
pruned_train <- draw_decision_boundaries(features2,clf_two,data_trainF2) # ... training data points
pruned_test <- draw_decision_boundaries(features2,clf_two,data_testF2) # ... testing data points

# ... step by step for for CART with two features and stopping criterion (maxdepth = 4)
draw_decision_steps(features2, clf_two)

# save plots
plots <- list(
  pruned_train = draw_decision_boundaries(features2, clf_two, data_trainF2),
  pruned_test = draw_decision_boundaries(features2, clf_two, data_testF2),
  unpruned_train = draw_decision_boundaries(features2, clf_two_unpruned, data_trainF2),
  unpruned_test = draw_decision_boundaries(features2, clf_two_unpruned, data_testF2))
for (name in names(plots)) {
  ggsave( filename = file.path(path, paste0("boundary2d_", name, ".png")),
    plot = plots[[name]], width = 10, height = 7, dpi = 300)
}



# 3) get accuracy --------------------------------------------------------------
pred_unpruned <- predict(clf_two_unpruned, newdata = data_testF2, type = "class")
C_unpruned <- table(Predicted = pred_unpruned, True = Y_test)
acc_unpruned <- (C_unpruned[1]+C_unpruned[4])/sum(C_unpruned)  

pred_pruned <- predict(clf_two, newdata = data_testF2, type = "class")
C_pruned <- table(Predicted = pred_pruned, True = Y_test)
acc_pruned <- (C_pruned[1]+C_pruned[4])/sum(C_pruned) 

# save as .txt file 
output_file <- file.path(path, "accuracy.txt")
sink(output_file)
cat("Confusion matrix: unpruned CART  with two features\n")
print(C_unpruned)
cat("\nAccuracy (unpruned):", acc_unpruned, "\n\n")
cat("Confusion matrix: pruned CART with two features and stopping criterion (maxdepth =4)\n")
print(C_pruned)
cat("\nAccuracy (pruned):", acc_pruned, "\n")
sink() 


