# credits ----------------------------------------------------------------------
'
WIRD IN DER DATEI master.R AUSGEFÜHRT!

Zweck:            Dieses Skript repliziert alle Ergebnisse, Abbildungen
                  und Tabellen, die im Paper erscheinen. 
                  Es wird von master.R eingelesen und sollte nicht
                  eigenständig ausgeführt werden (außer zu Debugging-Zwecken).

Output:           Ergebnisse werden in "paper/output/" gespeichert,
                  wie in master.R definiert. 

Hinweise:         Anpassungen der Arbeitsverzeichnisse erfolgen ausschließlich 
                  in master.R. Die Kommentare sind zur Einheitlichkeit auf Englisch 
                  gehalten.
'
# 0) define path ---------------------------------------------------------------
path <- path_paper_out



# 1) descriptive statistics ----------------------------------------------------
# load auxiliary function: 

# get_descriptive_table(): 
# Computes descriptive statistics for all numeric variables in a data frame.
# Input:  data (dataframe); digits (number of decimal places for rounding)
# Output: tidy dataframe (where each row corresponds to a variable and columns 
#         contain mean, standard deviation, min, 10%-quantile, 25%-quantile, median, 
#         75%-quantile, 90%-quantile, and max values for each variable)
get_descriptive_table <- function(data, digits = 3) {
  summary <- data %>% mutate( CL = as.numeric(CL), CL = ifelse(CL ==1, 1,0)) %>%
    summarise(across(where(is.numeric), list(
      mean = ~mean(.), sd   = ~sd(.), min  = ~min(.), q10  = ~quantile(., 0.10),
      q25  = ~quantile(., 0.25), median = ~median(.), q75  = ~quantile(., 0.75), 
      q90  = ~quantile(., 0.90), max  = ~max(.)
    ), .names = "{col}_{fn}")) %>%
    pivot_longer(everything(), names_to = c("Variable", "Statistik"), names_sep = "_") %>%
    pivot_wider(names_from = Statistik, values_from = value)
  rownames(summary) <- NULL
  return(summary)
}

# apply auxiliary function and export as .tex file for ...
# ... full data
summary_all <- get_descriptive_table(data_all)
print(xtable(summary_all, digits = 2, type = "latex"), 
      include.rownames = FALSE, file = file.path(path, "descriptiveStats_all.tex"))

# ... training data
summary_train <- get_descriptive_table(data_train)
print(xtable(summary_train, digits = 2, type = "latex"), 
      include.rownames = FALSE, file = file.path(path, "descriptiveStats_train.tex")) 

# ... test data
summary_test <- get_descriptive_table(data_test)
print(xtable(summary_test, digits = 2, type = "latex"), 
      include.rownames = FALSE, file = file.path(path, "descriptiveStats_test.tex"))



# 2) grow CART -----------------------------------------------------------------
# load auxiliary functions: 
#
# grow_cart():
# Fits a classification tree (CART) to the input data using the rpart package.
# The target variable is CL and all other columns are used as predictors.
# Input:  data (dataframe); minsplit (minimum number of observations required to attempt a split); 
#         maxdepth (maximum depth of the tree); cp (complexity parameter for pruning)
# Output: rpart object representing the fitted classification tree (CART tree)
grow_cart <- function(data, minsplit = 2, maxdepth = 30, cp = 0.0001) {
  clf <- rpart(CL ~ ., data = data, method = "class", parms = list(split = "gini"), model = TRUE , 
               control = rpart.control(cp = cp, minbucket = 1, minsplit = minsplit, maxdepth = maxdepth))
  return(clf)
}

# fun_nodes_some() and fun_nodes_all():
# Generates custom labels for nodes of an rpart classification tree, displayed 
# for fun_nodes_some() at root node, its children and leaves; 
# for fun_nodes_all() at all nodes 
# Input:  x (rpart object); labs (ignored, but required by rpart.plot); 
#         digits (ignored, but required by rpart.plot); varlen (ignored, but required by rpart.plot); 
# Output: character vector (of labels for each node in the tree)
fun_nodes_some <- function(x, labs, digits, varlen) {
  is_leaf <- x$frame$var != "<leaf>"  # which nodes are leaves?
  counts <- apply(cbind(x$frame$yval2[,3], x$frame$yval2[,2]), 1, 
                  function(row) paste(row, collapse = "/"))  # absolute counts per class (CL / Nicht-CL)
  majority_class <- ifelse(x$frame$yval == 1, "CL", "Nicht-CL")
  order <- as.integer(rownames(x$frame)) # node order
  # show counts only for root, its two children, and leaves
  counts_display <- ifelse(order>=4, "", counts)
  counts_display <- ifelse(is_leaf, counts_display, paste0(majority_class, "\n", counts))
  paste0(counts_display)
}
fun_nodes_all <- function(x, labs, digits, varlen) {
  is_leaf <- x$frame$var != "<leaf>"  # which nodes are leaves?
  counts <- apply(cbind(x$frame$yval2[,3], x$frame$yval2[,2]), 1, 
                  function(row) paste(row, collapse = "/"))  # absolute counts per class (CL / Nicht-CL)
  majority_class <- ifelse(x$frame$yval == 1, "CL", "Nicht-CL")
  order <- as.integer(rownames(x$frame)) # node order
  # show counts for all nodes in tree
  counts_display <- ifelse(is_leaf, counts, paste0(majority_class, "\n", counts))
  paste0(counts_display)
}

# plot_cart()
# Plots a classification tree (CART) using the rpart.plot package with customized node labels
# and visual settings. Provides an easy wrapper around rpart.plot for consistent styling.
# Input:  x (rpart object); type (tree type (see rpart.plot::prp);
#         extra (additional information to display in nodes); 
#         under (logical; place labels under the boxes); 
#         node.fun (function to generate node labels); 
#         box.col (color of node boxes); border.col (color of box borders); 
#         shadow.col (color of shadow); yesno (logical; show yes/no splits); 
#         uniform (logical; uniform branch lengths); branch (branch type); 
#         cex (text scaling factor); varlen (max length of variable names); 
#         roundint (logical; round numeric node values to integers)
# Output: plot of CART (with customized node labels and appearance)
plot_cart <- function(x, type = 2, extra = 100, under = TRUE, 
                      node.fun = fun_nodes_all, 
                      box.col = "white", border.col = "white", shadow.col = NA, yesno = FALSE, 
                      uniform = TRUE, branch = 1, cex = 0.9, varlen = 0, roundint=TRUE){
  rpart.plot::prp(x = x, type = type, extra = extra, under = under, node.fun = node.fun, 
                  box.col = box.col , border.col = border.col, shadow.col = shadow.col, yesno = yesno, 
                  uniform = uniform, branch = branch, cex = cex, varlen = varlen, roundint= roundint)
}

# built CARTs
cart_full  <- grow_cart(data_train, minsplit = 2)  # fully grown CART 
cart_stop1 <- grow_cart(data_train, minsplit = 2, maxdepth = 2) # CART with stopping criterion maxdepth = 2
cart_stop2 <- grow_cart(data_train, minsplit = 14) # CART with stopping criterion minsplit = 14
# quick visual check
rpart.plot(cart_full, extra = 1)
rpart.plot(cart_stop1, extra = 1)
rpart.plot(cart_stop2, extra = 1)

# variable importance in fully grown CART
cart_full$variable.importance
# Passquote          Fouls     Ballbesitz      Zweikampf          Titel Trainerwechsel 
# 12.574737      12.505202      11.506945       7.897793       5.893864       3.044156 


# export CART as plot for ...
# ... fully grown CART 
agg_png(file.path(path, "cart_full.png"), width = 2000, height = 3500, res = 300) 
plot_cart(cart_full, node.fun = fun_nodes_some)
par(xpd = NA) 
text(x = par("usr")[1], y = par("usr")[4], labels = "a", adj = c(0,1), cex = 1.4, font = 2)
dev.off()

# ... CART with stopping criterion maxdepth = 2
agg_png(file.path(path, "cart_stop1.png"), width = 1200, height = 1000, res = 300) 
plot_cart(cart_stop1, node.fun = fun_nodes_all)
par(xpd = NA)
text(x = par("usr")[1], y = par("usr")[4], labels = "a", adj = c(0,1), cex = 1.2, font = 2)
dev.off()

# ... CART with stopping criterion minsplit = 14
agg_png(file.path(path, "cart_stop2.png"), width = 1500, height = 2000, res = 300) 
plot_cart(cart_stop2, node.fun = fun_nodes_all)
par(xpd = NA) 
text(x = par("usr")[1], y = par("usr")[4], labels = "b", adj = c(0,1), cex = 1.4, font = 2)
dev.off()



# 3) summary table of CART splits with delta Q improvement value ---------------
# load auxiliary function:

# get_groupIndex()
# Generates a grouping index for consecutive repeated values in a vector.
# Input: x (vector)
# Output: integer vector (of the same length as 'x', where each consecutive run 
#         of identical values is assigned a unique integer indicating its group)
# Example: x <- c(44,44,56,56,56,7) returns: c(1,1,2,2,2,3)
get_groupIndex <- function(x) { 
  rl <- rle(x)                
  rep(seq_along(rl$lengths), rl$lengths) 
}

# get absolute counts per class 
abs_no <- apply(cbind(cart_full$frame$yval2[,3], cart_full$frame$yval2[,2]), 1, 
                function(row) paste(row, collapse = "/")) 
is_leaf <- cart_full$frame$var != "<leaf>" # which nodes are leaves?
abs_no <- ifelse(is_leaf, abs_no, "") # only keep absolute counts for the leaves 
abs_no <- abs_no[abs_no != ""]
# absolute counts and group index in data frame 
abs_no_df <- data.frame( classcount = abs_no, group_index = 1:length(abs_no))

# prepare absolute-counts-dataframe
deltaQsummary <- as.data.frame(cart_full$splits) # get split info from fully grown CART
deltaQsummary <- cbind(feature = rownames(deltaQsummary), deltaQsummary) %>%   
  filter(!deltaQsummary$count == 0) %>%  
  mutate(feature = as.character(feature), 
         feature = stringr::str_remove(feature, "\\..*$"),  
         sign = ifelse(ncat >0, ">=", "<"),             
         sign = ifelse(feature %in% features_dummies, "=", sign), 
         index = ceiling(index),                          
         group_index = get_groupIndex(count)) %>%          
  select(!adj) 
rownames(deltaQsummary) <- NULL  

# create headers for each group 
headers <- deltaQsummary %>% 
  group_by(group_index) %>%
  slice_max(order_by = improve, n = 1, with_ties = TRUE) %>%  # pick split with max improvement
  slice_min(row_number(), n = 1) %>%  # if tie, take first row
  ungroup() %>%
  mutate(header = paste(feature, sign, index)) %>% 
  select(feature, group_index, header) 

# merge headers with absolute-counts-dataframe
if (nrow(headers) == nrow(abs_no_df)) { headers <- merge(headers, abs_no_df, by = "group_index")}
deltaQtable <- left_join(deltaQsummary, headers, by = c("feature","group_index")) 
deltaQtable <- deltaQtable %>%  
  mutate(feature = stringr::str_remove(feature, "\\..*$"), 
         header = str_replace(header, "\\.\\d{1,2}", ""), 
         improve = round(improve,4), 
         header = ifelse(is.na(header), "", header)) %>% 
  select(!c(sign, ncat, count))

# prepare table for xtable export 
deltaQtable_prep <- deltaQtable %>%
  group_by(group_index) %>%
  group_modify(~ { 
    g <- .x
    hdr <- g$header[g$header != ""]
    hdr <- if (length(hdr) > 0) hdr[1] else NA_character_
    rows <- g %>% mutate(
      feature = feature, 
      classcount = classcount,
      improve = formatC(improve, digits = 4, format = "f"),
      s = as.character(index)) 
    if (!is.na(hdr)) { 
      header_row <- tibble(feature = paste0(hdr, " (", g$classcount[1], ")"), improve = "", s = "")
      rows <- bind_rows(header_row, rows) 
    }
    rows
  }) %>% 
  ungroup()

# export to.tex file
deltaQtable_prep <- as.data.frame(deltaQtable_prep %>% select(feature, improve, s))
rownames(deltaQtable_prep) <- NULL
deltaQtable_tex <- xtable( deltaQtable_prep,
                           label   = "tab:splits",
                           align   = c("l","l","r","r"))  
print(deltaQtable_tex, include.rownames = FALSE, file = file.path(path, "deltaQtable.tex"))



# 4) prune CART with 2 features ------------------------------------------------
set.seed(309)
cart2F_unpruned <- grow_cart(data_trainF2, minsplit = 2) # unpruned CART with two features 
# check visually 
rpart.plot(cart2F_unpruned, extra = 1)

# prune tree
printcp(cart2F_unpruned) # print xerror, cp, depth 
plotcp(cart2F_unpruned) # plot xerror-cp/depth 
# choose optimal cp for pruning 
minimal_cp <- as.data.frame(cart2F_unpruned$cptable) %>% filter(xerror == min(xerror)) %>% pull(CP)
cart2F_pruned <- grow_cart(data_trainF2, cp = minimal_cp, minsplit = 2) # pruned CART with two features 
# check visually 
rpart.plot(cart2F_pruned, extra = 1)

# export CART as plot for ...
# ... unpruned CART with two features 
agg_png(file.path(path, "cart2F_unpruned.png"), width = 3000, height = 3000, res = 300) 
plot_cart(cart2F_unpruned, node.fun = fun_nodes_all)
par(xpd = NA)  
dev.off()

# ... pruned CART with two features 
agg_png(file.path(path, "cart2F_pruned.png"), width = 1000, height = 1100, res = 300) 
plot_cart(cart2F_pruned, node.fun = fun_nodes_all) 
par(xpd = NA)  
dev.off()

# create own xerror-cp/depth plot 
png(file.path(path, "xerror.png"),width = 4000, height = 1600, res = 300)
plotcp <- as.data.frame(cart2F_unpruned$cptable) %>% mutate(depth = nsplit + 1)
plot(1:nrow(plotcp), plotcp$xerror,
     type = "b", pch = 16, col = "black",
     xaxt = "n", yaxt = "n",
     xlab = "Penalisierungsparameter (skaliert)",
     ylab = "relativer CV-Fehler", 
     ylim = c(0.2, 1.4), 
     cex = 2, cex.lab = 1.5)    
axis(1, at = 1:nrow(plotcp), labels = round(plotcp$CP, 3), cex.axis = 1.4)
axis(3, at = 1:nrow(plotcp), labels = plotcp$depth, cex.axis = 1.4)
mtext("Baumtiefe", side = 3, line = 2.7, cex = 1.5)
y_ticks <- seq(0, 1.4, by = 0.2)
axis(2, at = y_ticks, labels = round(y_ticks, 3), cex.axis = 1.4)
abline(h = 0.8, lty = 2, col = "black", lwd = 1)
arrows(1:nrow(plotcp), plotcp$xerror - plotcp$xstd, 
       1:nrow(plotcp), plotcp$xerror + plotcp$xstd,
       angle = 90, code = 3, length = 0.05, col = "black")
dev.off()



# 5) draw 2D decision boundaries -----------------------------------------------
# load auxiliary functions: 

# plot_space()
# Plots the decision boundaries of a classification tree in a two-dimensional feature space
# using the plotmo package.
# Input:  tree (rpart object); type (tree type for plotmo); 
#         type2 (second type of plot for plotmo; here: "image" to show decision regions); 
#         pt.cex (size of points); cex.lab (axis label size); main (main title); 
#         cex.axis (axis tick label size); pt.col (vector of point colors, by class); 
#         all1 (logical; whether to plot all variables one at a time); 
#         do.par (logical; whether plotmo should set up graphical parameters); 
#         col.image (vector of colors for background); pt.pch (vector of point symbols)
# Output: plotmo object
plot_space <- function(tree, type ="class", type2 = "image",  pt.cex = 2, 
                       cex.lab = 1.5, main = "", cex.axis = 1.4,
                       pt.col = ifelse(data_trainF2$CL == "CL", "blue", "black"), 
                       all1=FALSE, do.par=FALSE, col.image = c("grey91", "white"), 
                       pt.pch = ifelse(data_trainF2$CL == "CL", 16, 1)) {
  clf <- plotmo(tree, type = type, type2 = type2, pt.col = pt.col, main = main, 
                all1=all1, pt.pch=pt.pch, do.par=do.par, col.image = col.image, pt.cex = pt.cex, 
                plotres = FALSE,   nresponse = 1, 
                ndresponse = 2, cex.lab = cex.lab, cex.axis = cex.axis)
  return(clf)
}


# plot feature space with decision rule for ...
# ... unpruned graph with ....
plot_space(cart2F_unpruned,  pt.cex = 2) # ... training data points
plot_space(cart2F_pruned, pt.col = NA,  pt.cex = 0) 
points(data_testF2[[1]], data_testF2[[2]], , cex=2,
       pch = ifelse(data_testF2$CL == "CL", 17, 2), # ... testing data points
       col=ifelse(data_testF2$CL=="CL","blue","black"))
# and for pruned graph with ...
plot_space(cart2F_unpruned, pt.cex = 2) # ... training data points
plot_space(cart2F_pruned, pt.col = NA,  pt.cex = 0) 
points(data_testF2[[1]], data_testF2[[2]], cex=2, # ... testing data points
       pch = ifelse(data_testF2$CL == "CL", 17, 2),
       col=ifelse(data_testF2$CL=="CL","blue","black"))

# export feature space plots for ...
# ... unpruned CART with training data points
png(file.path(path, "featurespaceA.png"), width = 2000, height = 2000, res = 300)
plot_space(cart2F_unpruned)
mtext("a", side = 3, adj = 0, line = 0.5, font = 2, cex= 2)
dev.off()

# ... unpruned CART with testing data points
png(file.path(path, "featurespaceB.png"), width = 2000, height = 2000, res = 300)
plot_space(cart2F_unpruned, pt.col = NA,  pt.cex = 0) 
points(data_testF2[[1]], data_testF2[[2]], cex=2, 
       pch = ifelse(data_testF2$CL == "CL", 17, 2),
       col=ifelse(data_testF2$CL=="CL","blue","black"))
mtext("b", side = 3, adj = 0, line = 0.5, font = 2, cex= 2)
dev.off()

# ... pruned CART with training data points
png(file.path(path, "featurespaceC.png"), width = 2000, height = 2000, res = 300)
plot_space(cart2F_pruned) # train data
mtext("c", side = 3, adj = 0, line = 0.5, font = 2, cex= 2)
dev.off()

# ... pruned CART with testing data points
png(file.path(path, "featurespaceD.png"), width = 2000, height = 2000, res = 300)
plot_space(cart2F_pruned, pt.col = NA,  pt.cex = 0) 
points(data_testF2[[1]], data_testF2[[2]], cex=2, 
       pch = ifelse(data_testF2$CL == "CL", 17, 2),
       col=ifelse(data_testF2$CL=="CL","blue","black"))
mtext("d", side = 3, adj = 0, line = 0.5, font = 2, cex= 2)
dev.off()



# 6) get accuracy --------------------------------------------------------------
pred_unpruned <- predict(cart2F_unpruned, newdata = data_testF2, type = "class")
C_unpruned <- table(Predicted = pred_unpruned, True = Y_test)
acc_unpruned <- (C_unpruned[1]+C_unpruned[4])/sum(C_unpruned)  

pred_pruned <- predict(cart2F_pruned, newdata = data_testF2, type = "class")
C_pruned <-table(Predicted = pred_pruned, True = Y_test)
acc_pruned <- (C_pruned[1]+C_pruned[4])/sum(C_pruned) 

# save as .txt file 
output_file <- file.path(path, "accuracy.txt")
sink(output_file)
cat("Confusion matrix: unpruned CART with two features\n")
print(C_unpruned)
cat("\nAccuracy (unpruned):", acc_unpruned, "\n\n")
cat("Confusion matrix: optimally pruned CART  with two features\n")
print(C_pruned)
cat("\nAccuracy (pruned):", acc_pruned, "\n")
sink() 


